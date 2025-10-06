use askama::Template;
use axum::{
    extract::State,
    http::StatusCode,
    response::{AppendHeaders, IntoResponse},
};
use reqwest::header;

use crate::{AppState, BlaimError};

use askama::filters;
use blaim_db::Item;

#[derive(Template)]
#[template(path = "items.js", escape = "none")]
pub struct ItemsTemplate {
    pub all_items: Vec<Item>,
}

#[axum::debug_handler]
pub async fn list_items(State(state): State<AppState>) -> Result<impl IntoResponse, BlaimError> {
    let rendered = ItemsTemplate {
        all_items: blaim_db::get_all_items(&mut *state.pool.acquire().await?).await?,
    }
    .render()?;

    Ok((
        StatusCode::OK,
        AppendHeaders([(header::CONTENT_TYPE, "text/javascript; charset=utf-8")]),
        (rendered),
    )
        .into_response())
}
