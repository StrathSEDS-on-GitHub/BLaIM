use askama::{Template, filters};
use blaim_db::{BorrowUpdates, Item, ItemTree, MemberOwner, Owner};
use sqlx::types::time::OffsetDateTime;

use crate::session::BlaimSession;
use time::format_description::well_known::Rfc2822;

use crate::closure;
use crate::templates::ItemTreeTemplate;
use axum::{
    extract::{self, Path},
    http::Uri,
    response::{Html, IntoResponse},
};
use rand::{Rng, distr::Alphanumeric};
use reqwest::StatusCode;
use tower_sessions::Session;

use crate::{AppState, BlaimError};

#[axum::debug_handler]
pub async fn query_item_search(
    session: Session,
    extract::State(state): extract::State<AppState>,
    Path(name): Path<String>,
) -> Result<impl IntoResponse, BlaimError> {
    let items = blaim_db::lookup_item(&mut *state.pool.acquire().await?, &name).await?;

    if items.is_empty() {
        return Ok((StatusCode::NOT_FOUND, Html("No such item.".to_owned())));
    }
    let item = &items[0];

    query_item(
        None,
        session,
        extract::State(state),
        Path((name, item.id)),
        format!(
            "/item/{}/{}",
            filters::urlencode_strict(item.name.clone()).unwrap(),
            item.id
        )
        .parse()
        .unwrap(),
    )
    .await
}

pub async fn query_item(
    borrow_updates: Option<BorrowUpdates>,
    session: Session,
    extract::State(state): extract::State<AppState>,
    Path((_name, id)): Path<(String, i32)>,
    uri: Uri,
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
                redirect: uri.to_string(),
            };
            session.insert("session", &auth).await?;
            auth
        }
    };

    let Some(item) = blaim_db::get_item_by_id(&mut *state.pool.acquire().await?, id).await? else {
        return Ok((StatusCode::NOT_FOUND, Html("No such item.".to_owned())));
    };

    let owner = blaim_db::get_owner(&mut *state.pool.acquire().await?, id).await?;
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
    let box_contents = blaim_db::box_contents(&mut *state.pool.acquire().await?, &item).await?;

    let template = ItemStatusTemplate {
        item,
        owner,
        borrow_history,
        session: auth,
        borrow_updates,
        box_contents,
    };

    Ok((StatusCode::OK, Html(template.render()?)))
}

#[derive(Template)]
#[template(path = "item_status.html")]
pub struct ItemStatusTemplate {
    pub session: BlaimSession,

    pub item: Item,
    pub box_contents: ItemTree,
    pub owner: Option<Owner>,
    pub borrow_history: Vec<(Owner, OffsetDateTime)>,
    pub borrow_updates: Option<BorrowUpdates>,
}
