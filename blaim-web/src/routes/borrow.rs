use axum::{
    extract::{self, Path, Request},
    response::Html,
};
use reqwest::StatusCode;
use tower_sessions::Session;
use tracing::info;

use crate::{AppState, BlaimError, routes, session::BlaimSession};

#[axum::debug_handler]
pub async fn borrow_item(
    session: Session,
    extract::State(state): extract::State<AppState>,
    Path(item): Path<u64>,
    req: Request,
) -> Result<(StatusCode, Html<String>), BlaimError> {
    let Some(BlaimSession::Authenticated { member }) = session.get("session").await.ok().flatten()
    else {
        return Ok((StatusCode::FORBIDDEN, Html("Forbidden".to_string())));
    };

    let Some(item) =
        blaim_db::get_item_by_id(&mut *state.pool.acquire().await?, item as i32).await?
    else {
        return Ok((StatusCode::NOT_FOUND, Html("Unknown item id".to_string())));
    };

    let updates = blaim_db::borrow_item(
        &mut *state.pool.acquire().await?,
        &item,
        &member.id.to_string(),
    )
    .await?;

    info!("Borrow updates: {:?}", updates);

    routes::item::query_item(
        Some(updates),
        session,
        extract::State(state),
        Path((item.name, item.id)),
        req,
    )
    .await
}
