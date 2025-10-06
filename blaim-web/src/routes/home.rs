
use askama::Template;
use axum::response::{Html, IntoResponse};

use crate::BlaimError;

#[derive(Template)]
#[template(path = "index.html")]
pub struct Home;

pub async fn home() -> Result<impl IntoResponse, BlaimError> {
    Ok(Html(Home.render()?))
}