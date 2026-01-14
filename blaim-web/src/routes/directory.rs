use askama::Template;
use blaim_db::ItemTreeOwned;

use axum::{
    extract::{self},
    response::Html,
};
use reqwest::StatusCode;

use crate::{AppState, BlaimError, templates::ItemTreeOwnedTemplate};

pub async fn directory(
    extract::State(mut state): extract::State<AppState>,
) -> Result<(StatusCode, Html<String>), BlaimError> {
    let template = DirectoryTemplate {
        items: blaim_db::get_all_itemtrees_with_owner(&mut state.pool)
            .await?
    };

    Ok((StatusCode::OK, Html(template.render()?)))
}

#[derive(Template)]
#[template(path = "directory.html")]
pub struct DirectoryTemplate {
    pub items: Vec<ItemTreeOwned>,
}
