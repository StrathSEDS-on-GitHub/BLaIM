use askama::Template;
use axum::{
    extract::State,
    http::StatusCode,
    response::{AppendHeaders, Html, IntoResponse},
};
use reqwest::header;
use tower_sessions::Session;

use crate::{AppState, BlaimError, session::BlaimSession, templates};

#[axum::debug_handler]
pub async fn list_members(
    session: Session,
    State(state): State<AppState>,
) -> Result<impl IntoResponse, BlaimError> {
    let Some(BlaimSession::Authenticated { .. }) = session.get("session").await.ok().flatten()
    else {
        return Ok((StatusCode::FORBIDDEN, Html("Invalid session.")).into_response());
    };

    let rendered = templates::members::MembersTemplate {
        all_members: blaim_db::get_all_cached_members(&mut *state.pool.acquire().await?).await?,
    }
    .render()?;

    Ok((
        StatusCode::OK,
        AppendHeaders([(header::CONTENT_TYPE, "text/javascript; charset=utf-8")]),
        (rendered),
    )
        .into_response())
}
