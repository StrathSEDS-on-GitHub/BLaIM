use std::sync::LazyLock;

use chrono::{DateTime, Utc};
use fuzzy_matcher::skim::SkimMatcherV2;
use fuzzy_matcher::FuzzyMatcher;
use sqlx::{Acquire, SqlitePool};

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Item {
    pub id: i64,
    pub name: String,
}

fn fuzzy_search<'b, T>(
    needle: &'b str,
    haystack: impl Iterator<Item = T> + 'b,
    key: impl Fn(&T) -> &str,
) -> Vec<(i64, T)> {
    static MATCHER: LazyLock<SkimMatcherV2> =
        LazyLock::new(|| SkimMatcherV2::default().smart_case().use_cache(true));
    let mut scores: Vec<_> = haystack
        .filter_map(|s| MATCHER.fuzzy_match(key(&s), needle).map(|score| (score, s)))
        .collect();
    scores.sort_by_key(|(score, _)| *score);
    scores.splice(scores.len().min(3).., []);
    scores
}

pub(crate) async fn lookup_item(pool: &SqlitePool, item: &str) -> anyhow::Result<Vec<Item>> {
    let mut connection = pool.acquire().await?;
    let item = item.to_lowercase();
    let exact_matches: Vec<_> = sqlx::query_as!(
        Item,
        "SELECT id, name FROM items WHERE strid = ? OR name = ?",
        item,
        item
    )
    .fetch_all(&mut *connection)
    .await?;

    let fuzzy_matches = sqlx::query_as!(Item, "SELECT id, name FROM items;")
        .fetch_all(&mut *connection)
        .await?
        .into_iter();

    let results = fuzzy_search(&item, fuzzy_matches, |i| &i.name)
        .into_iter()
        .filter_map(|(_, item)| {
            if !exact_matches.contains(&item) {
                Some(item)
            } else {
                None
            }
        })
        .collect::<Vec<_>>();

    Ok(exact_matches.into_iter().chain(results).collect())
}

pub(crate) async fn get_item_by_id(pool: &SqlitePool, id: i64) -> anyhow::Result<Option<Item>> {
    let mut connection = pool.acquire().await?;
    let item = sqlx::query_as!(Item, "SELECT id, name FROM items WHERE id = ?", id)
        .fetch_optional(&mut *connection)
        .await?;
    Ok(item)
}

pub(crate) async fn get_last_holder(
    pool: &SqlitePool,
    item_id: i64,
) -> anyhow::Result<Option<String>> {
    let mut connection = pool.acquire().await?;
    let holder = sqlx::query!(
        "SELECT to_user FROM borrow WHERE item_id = ? ORDER BY ordering DESC LIMIT 1",
        item_id
    )
    .fetch_optional(&mut *connection)
    .await?;
    Ok(holder.map(|row| row.to_user))
}

pub(crate) async fn borrow_item(
    pool: &SqlitePool,
    item_id: i64,
    user: &str,
) -> anyhow::Result<i32> {
    let mut connection = pool.acquire().await?;
    let now = sqlx::types::chrono::Utc::now();
    let id = sqlx::query!(
        "INSERT INTO borrow (item_id, to_user, time) VALUES (?, ?, ?); SELECT last_insert_rowid() as id;",
        item_id,
        user,
        now,
    )
    .fetch_one(&mut *connection)
    .await?;
    Ok(id.id)
}

pub(crate) async fn delete_borrow(
    pool: &SqlitePool,
    borrow_id: i32,
) -> anyhow::Result<()> {
    let mut connection = pool.acquire().await?;
    sqlx::query!(
        "DELETE FROM borrow WHERE ordering = ?",
        borrow_id,
    )
    .execute(&mut *connection)
    .await?;
    Ok(())
}

pub(crate) async fn update_borrow_item(pool: &SqlitePool, borrow_id: i32, item_id: i64) -> anyhow::Result<()> {
    let mut connection = pool.acquire().await?;
    sqlx::query!(
        "UPDATE borrow SET item_id = ? WHERE ordering = ?",
        item_id,
        borrow_id,
    ).execute(&mut *connection).await?;
    Ok(())
}

pub(crate) async fn borrow_history(pool: &SqlitePool, item_id: i64) -> anyhow::Result<Vec<(String, DateTime<Utc>)>> {
    let mut connection = pool.acquire().await?;
    let history = sqlx::query!(
        "SELECT to_user, time FROM borrow WHERE item_id = ? ORDER BY ordering DESC",
        item_id
    )
    .fetch_all(&mut *connection)
    .await?;
    Ok(history.into_iter().map(|row| (row.to_user, DateTime::from_naive_utc_and_offset(row.time, Utc))).collect())
}

pub(crate) async fn register_item(pool: &SqlitePool, strid: &str, name: &str) -> anyhow::Result<()> {
    let mut connection = pool.acquire().await?;
    sqlx::query!(
        "INSERT INTO items (strid, name) VALUES (?, ?);",
        strid,
        name,
    ).execute(&mut *connection).await?;
    Ok(())
}

