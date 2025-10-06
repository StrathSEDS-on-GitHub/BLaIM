use axum::{
    Form,
    extract::{self, Path},
    response::Html,
};
use blaim_db::Owner;
use reqwest::StatusCode;
use tower_sessions::Session;

use crate::{AppState, BlaimError, routes::query_item, session::BlaimSession};

#[derive(serde::Deserialize)]
pub struct GiveQuery {
    to: Owner,
}

#[axum::debug_handler]
pub async fn give_item(
    session: Session,
    extract::State(state): extract::State<AppState>,
    Path(item): Path<u64>,
    Form(GiveQuery { to }): Form<GiveQuery>,
) -> Result<(StatusCode, Html<String>), BlaimError> {
    let Some(BlaimSession::Authenticated { .. }) = session.get("session").await.ok().flatten()
    else {
        return Ok((StatusCode::FORBIDDEN, Html("Forbidden".to_string())));
    };

    let Some(item) =
        blaim_db::get_item_by_id(&mut *state.pool.acquire().await?, item as i32).await?
    else {
        return Ok((StatusCode::NOT_FOUND, Html("Unknown item id".to_string())));
    };

    let updates =
        blaim_db::borrow_item(&mut *state.pool.acquire().await?, &item, &to.db_string()).await?;

    query_item(
        Some(updates),
        session,
        extract::State(state),
        Path((item.name.clone(), item.id)),
        format!(
            "/item/{}/{}",
            askama::filters::urlencode_strict(item.name).unwrap(),
            item.id
        )
        .parse()
        .unwrap(),
    )
    .await
}
