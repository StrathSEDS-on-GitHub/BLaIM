use askama::Template as _;
use axum::{extract::{self, Path, Request}, response::{Html, IntoResponse}};
use blaim_db::{BorrowUpdates, Owner};
use rand::{distr::Alphanumeric, Rng};
use reqwest::StatusCode;
use time::OffsetDateTime;
use tower_sessions::Session;

use crate::{session::BlaimSession, templates, AppState, BlaimError};


#[axum::debug_handler]
pub async fn query_item_search(
    session: Session,
    extract::State(state): extract::State<AppState>,
    Path(name): Path<String>,
    req: Request,
) -> Result<impl IntoResponse, BlaimError> {
    let items = blaim_db::lookup_item(&mut *state.pool.acquire().await?, &name).await?;

    if items.is_empty() {
        return Ok((StatusCode::NOT_FOUND, Html("No such item.".to_owned())));
    }
    let item = &items[0];

    query_item(None, session, extract::State(state), Path((name, item.id)), req).await
}

pub async fn query_item(
    borrow_updates: Option<BorrowUpdates>,
    session: Session,
    extract::State(state): extract::State<AppState>,
    Path((_name, id)): Path<(String, i32)>,
    req: Request,
) -> Result<(StatusCode, Html<String>), BlaimError> {
    let auth: Option<BlaimSession> = session.get("session").await.ok().flatten();

    let auth = match auth {
        Some(session @ BlaimSession::Authenticated { .. }) => session,
        Some(session @ BlaimSession::Challenged { at, .. })
            if (OffsetDateTime::now_utc() - at).whole_minutes() < 5 =>
        {
            session
        }
        _ => {
            let oauth_state = rand::rng()
                .sample_iter(Alphanumeric)
                .take(64)
                .map(char::from)
                .collect();
            let auth = BlaimSession::Challenged {
                at: OffsetDateTime::now_utc(),
                state: oauth_state,
                redirect: req.uri().to_string(),
            };
            session.insert("session", &auth).await?;
            auth
        }
    };

    let Some(item) = blaim_db::get_item_by_id(&mut *state.pool.acquire().await?, id).await? else {
        return Ok((StatusCode::NOT_FOUND, Html("No such item.".to_owned())));
    };

    let owner = blaim_db::get_last_holder(&mut *state.pool.acquire().await?, id).await?;
    let owner = if let Some(owner) = owner {
        blaim_db::get_owner_info(&mut *state.pool.acquire().await?, &owner).await?
    } else {
        Owner::Ethereal
    };

    let borrow_history = blaim_db::borrow_history(&mut *state.pool.acquire().await?, item.id)
        .await?
        .into_iter()
        .map(async |(owner, dt)| {
            Ok::<_, BlaimError>((
                blaim_db::get_owner_info(&mut *state.pool.acquire().await?, &owner).await?,
                dt,
            ))
        });

    let borrow_history = futures::future::try_join_all(borrow_history).await?;

    let template = templates::item_status::ItemStatusTemplate {
        item,
        owner,
        borrow_history,
        session: auth,
        borrow_updates
    };

    Ok((StatusCode::OK, Html(template.render()?)))
}