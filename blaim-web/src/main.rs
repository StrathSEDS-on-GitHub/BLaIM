use askama::Template;
use axum::{
    Router,
    extract::State,
    http::StatusCode,
    response::{Html, IntoResponse},
    routing::get,
};
use color_eyre::eyre::Context;
use sqlx::PgPool;
use tower_http::{services::ServeDir, trace::TraceLayer};
use tower_sessions::cookie::SameSite;

use crate::routes::{authorize, query_item, query_item_search};

mod routes;
mod session;
mod templates;

#[derive(Clone)]
pub struct AppState {
    pool: PgPool,
}

#[tokio::main]
async fn main() -> color_eyre::Result<()> {
    tracing_subscriber::fmt()
        .with_max_level(tracing::Level::DEBUG)
        .init();

    let db_url = std::env::var("DATABASE_URL").wrap_err("DATABASE_URL not set")?;

    let pool = sqlx::Pool::connect_lazy(&db_url).wrap_err("Failed to connect to DB")?;

    let storage = tower_sessions_sqlx_store::PostgresStore::new(pool.clone());
    storage.migrate().await?;

    // build our application with a single route
    let app = Router::new()
        .route("/", get(home))
        .route("/item/{:name}", get(query_item_search))
        .route("/item/{:name}/{:id}", get(query_item))
        .route("/authorize", get(authorize))
        .nest_service("/pkg", ServeDir::new("pkg"))
        .layer(TraceLayer::new_for_http())
        .layer(tower_sessions::SessionManagerLayer::new(storage).with_same_site(SameSite::Lax))
        .with_state(AppState { pool });

    // run our app with hyper, listening globally on port 3000
    let listener = tokio::net::TcpListener::bind("0.0.0.0:8080").await.unwrap();
    println!("Serving at 0.0.0.0:8080");
    axum::serve(listener, app).await.unwrap();
    println!("Bye!");

    Ok(())
}

#[derive(Debug)]
struct BlaimError(color_eyre::eyre::ErrReport);

impl<E> From<E> for BlaimError
where
    E: Into<color_eyre::eyre::ErrReport>,
{
    fn from(value: E) -> Self {
        BlaimError(value.into())
    }
}

impl IntoResponse for BlaimError {
    fn into_response(self) -> axum::response::Response {
        (StatusCode::INTERNAL_SERVER_ERROR, format!("{:#}", self.0)).into_response()
    }
}

async fn home(state: State<AppState>) -> Result<impl IntoResponse, BlaimError> {
    let items = blaim_db::get_all_items(&mut *state.pool.acquire().await?).await?;
    Ok(Html(templates::home::Home { items }.render()?))
}
