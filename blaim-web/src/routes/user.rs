use askama::Template;
use blaim_db::{ItemTree, MemberOwner, Owner};

use axum::{
    extract::{self},
    response::Html,
};
use reqwest::StatusCode;

use crate::{AppState, BlaimError, templates::ItemTreeTemplate};

pub async fn user_search(
    extract::State(state): extract::State<AppState>,
    extract::Path(username): extract::Path<String>,
) -> Result<(StatusCode, Html<String>), BlaimError> {
    let owner = blaim_db::lookup_user(&mut *state.pool.acquire().await?, &username).await;

    if let Some(owner) = owner {
        user_impl(
            state,
            owner.db_string(),
        )
        .await
    } else {
        Ok((StatusCode::NOT_FOUND, Html("No such user.".to_owned())))
    }
}

async fn user_impl(state: AppState, user_id: String) -> Result<(StatusCode, Html<String>), BlaimError> {
    let owner = blaim_db::get_owner_info(&mut *state.pool.acquire().await?, &user_id).await?;

    let template = UserTemplate {
        owner,
        items: blaim_db::get_itemtrees(&mut *state.pool.acquire().await?, &user_id).await?,
    };

    Ok((StatusCode::OK, Html(template.render()?)))

}

pub async fn user(
    extract::State(state): extract::State<AppState>,
    extract::Path((username, user_id)): extract::Path<(String, String)>,
) -> Result<(StatusCode, Html<String>), BlaimError> {
    if user_id.trim().is_empty() {
        return user_search(extract::State(state), extract::Path(username)).await;
    }

    user_impl(state, user_id).await
}

#[derive(Template)]
#[template(path = "user.html")]
pub struct UserTemplate {
    pub owner: Owner,
    pub items: Vec<ItemTree>,
}
