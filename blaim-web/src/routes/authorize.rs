use axum::{
    extract::{Query, State},
    http::StatusCode,
    response::{Html, IntoResponse, Redirect, Response},
};
use blaim_db::try_resolve_member;
use color_eyre::eyre::eyre;
use reqwest::{Client, Method};
use time::OffsetDateTime;
use tower_sessions::Session;
use tracing::info;

use crate::{AppState, BlaimError, session::BlaimSession, templates::OAUTH_REDIRECT_URI};

#[derive(serde::Deserialize)]
pub struct AuthorizeQuery {
    code: String,
    state: String,
}

#[axum::debug_handler]
pub async fn authorize(
    session: Session,
    State(app_state): State<AppState>,
    Query(AuthorizeQuery { code, state }): Query<AuthorizeQuery>,
) -> Result<Response, BlaimError> {
    info!(
        "Received OAuth callback with code: {}, state: {}",
        code, state
    );
    info!(
        "Session data: {:?}",
        session.get::<BlaimSession>("session").await
    );

    let Some(BlaimSession::Challenged {
        at,
        state: state_ve,
        redirect,
    }): Option<BlaimSession> = session.get("session").await.ok().flatten()
    else {
        return Ok((StatusCode::FORBIDDEN, Html("Invalid session.")).into_response());
    };

    if (OffsetDateTime::now_utc() - at).whole_minutes() >= 5 {
        return Ok((StatusCode::FORBIDDEN, Html("Session expired.")).into_response());
    }

    if state != state_ve {
        return Ok((StatusCode::FORBIDDEN, Html("Invalid state.")).into_response());
    }

    // Exchange code for token
    let response = Client::new().request(Method::POST,
        "https://discord.com/api/oauth2/token")
        .header("content-type", "application/x-www-form-urlencoded")
        .body(format!(
            "client_id=1239583904554549258&client_secret={}&grant_type=authorization_code&code={}&redirect_uri={}",
            std::env::var("DISCORD_CLIENT_SECRET").expect("DISCORD_CLIENT_SECRET must be set"),
            code,
            askama::filters::urlencode_strict(OAUTH_REDIRECT_URI).unwrap()
        )).send().await?.json::<serde_json::Value>().await?;

    info!("Discord token response: {:?}", response);

    let access_token = response
        .get("access_token")
        .and_then(|v| v.as_str())
        .ok_or_else(|| eyre!("Failed to get access token from Discord response"))?;

    // Get the user's snowflake
    let response = Client::new()
        .request(Method::GET, "https://discord.com/api/v10/users/@me")
        .bearer_auth(access_token)
        .send()
        .await?
        .json::<serde_json::Value>()
        .await?;

    let snowflake = response
        .get("id")
        .and_then(|v| v.as_str())
        .ok_or_else(|| eyre!("Failed to get user ID from Discord response"))?
        .parse::<u64>()?;

    let member = try_resolve_member(&mut *app_state.pool.acquire().await?, snowflake).await?;

    let blaim_db::MemberOwner::Resolved(member) = member else {
        return Ok((
            StatusCode::FORBIDDEN,
            Html("Please join the <a href=\"https://strathseds.org/discord\">StrathSEDS discord.</a>".to_owned())).into_response()
        );
    };

    session
        .insert("session", BlaimSession::Authenticated { member })
        .await?;

    Ok(Redirect::to(&redirect).into_response())
}
