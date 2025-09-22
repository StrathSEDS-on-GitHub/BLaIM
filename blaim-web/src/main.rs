use askama::Template;
use axum::{
    Router,
    extract::{self, Path, State},
    http::StatusCode,
    response::{Html, IntoResponse},
    routing::get,
};
use blaim_db::Owner;
use color_eyre::eyre::Context;
use sqlx::PgPool;
use tower_http::{services::ServeDir, trace::TraceLayer};

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

    // build our application with a single route
    let app = Router::new()
        .route("/", get(home))
        .route("/item/{:name}", get(query_item_search))
        .route("/item/{:name}/{:id}", get(query_item))
        .nest_service("/pkg", ServeDir::new("pkg"))
        .layer(TraceLayer::new_for_http())
        .with_state(AppState {
            pool: sqlx::Pool::connect_lazy(&db_url).wrap_err("Failed to connect to DB")?,
        });

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

#[axum::debug_handler]
async fn query_item_search(
    extract::State(state): extract::State<AppState>,
    Path(name): Path<String>,
) -> Result<impl IntoResponse, BlaimError> {
    let items = blaim_db::lookup_item(&mut *state.pool.acquire().await?, &name).await?;

    if items.is_empty() {
        return Ok((StatusCode::NOT_FOUND, Html("No such item.".to_owned())));
    }
    let item = &items[0];

    query_item(extract::State(state), Path((name, item.id))).await
}

#[axum::debug_handler]
async fn query_item(
    extract::State(state): extract::State<AppState>,
    Path((_name, id)): Path<(String, i32)>,
) -> Result<(StatusCode, Html<String>), BlaimError> {
    let Some(item) = blaim_db::get_item_by_id(&mut *state.pool.acquire().await?, id).await? else {
        return Ok((StatusCode::NOT_FOUND, Html("No such item.".to_owned())));
    };

    let owner = blaim_db::get_last_holder(&mut *state.pool.acquire().await?, id).await?;
    let owner = if let Some(owner) = owner {
        blaim_db::get_owner_info(&mut *state.pool.acquire().await?, &owner).await?
    } else {
        Owner::Ethereal
    };

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

    let template = templates::item_status::ItemStatusTemplate {
        item,
        owner,
        borrow_history,
    };

    Ok((StatusCode::OK, Html(template.render()?)))
}

async fn home(state: State<AppState>) -> Result<impl IntoResponse, BlaimError> {
    let items = blaim_db::get_all_items(&mut *state.pool.acquire().await?).await?;
    Ok(Html(templates::home::Home { items }.render()?))
}
