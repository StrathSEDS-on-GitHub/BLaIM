use std::collections::{HashMap, VecDeque};
use std::fmt::{self, Display};
use std::sync::LazyLock;

use color_eyre::eyre::Context;
use fuzzy_matcher::FuzzyMatcher;
use fuzzy_matcher::skim::SkimMatcherV2;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use sqlx::types::time::{OffsetDateTime, PrimitiveDateTime};
use sqlx::{Acquire, PgConnection, PgPool, Row};

mod itemtree;
pub use itemtree::*;

#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize)]
pub struct Item {
    pub id: i32,
    pub name: String,
    pub strid: String,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Location {
    pub id: i32,
    pub name: String,
    pub strid: String,
}

trait FuzzySearcher<'x, T> {
    type R: Iterator<Item = &'x str> + 'x;
    fn call(&'x mut self, item: &'x T) -> Self::R;
}

impl<'x, T, R, F> FuzzySearcher<'x, T> for F
where
    T: 'x,
    R: Iterator<Item = &'x str> + 'x,
    F: FnMut(&'x T) -> R,
{
    type R = R;
    fn call(&'x mut self, i: &'x T) -> Self::R {
        self(i)
    }
}

fn fuzzy_search<'b, 'a, T: std::fmt::Debug + 'a, F>(
    needle: &'b str,
    haystack: impl Iterator<Item = T>,
    mut key: F,
) -> Vec<(i64, T)>
where
    F: for<'x> FuzzySearcher<'x, T>,
{
    static MATCHER: LazyLock<SkimMatcherV2> =
        LazyLock::new(|| SkimMatcherV2::default().smart_case().use_cache(true));
    let mut scores: Vec<_> = haystack
        .filter_map(|s| {
            let x = key
                .call(&s)
                .map(|it| MATCHER.fuzzy_match(it, needle))
                .max_by_key(|it| it.unwrap_or(i64::MIN))
                .flatten();
            if let Some(x) = x { Some((x, s)) } else { None }
        })
        .collect();
    scores.sort_by_key(|(score, _)| -*score);
    scores.splice(scores.len().min(5).., []);
    scores
}

// NOTE: Can't be a closure due to https://github.com/rust-lang/rust/issues/70263
macro_rules! fuzzy_searcher {
    ($ty:ty, |$var:ident| $body:expr) => {{
        fn searcher_impl<'a>($var: &'a $ty) -> impl Iterator<Item = &'a str> {
            $body
        }

        searcher_impl
    }};
}

macro_rules! fuzzy_searcher_once {
    ($ty: ty, |$var:ident| $body:expr) => {
        fuzzy_searcher!($ty, |$var| std::iter::once($body))
    };
}

pub async fn lookup_user(connection: &mut PgConnection, user: &str) -> Option<Owner> {
    let user = user.to_lowercase();

    let all_users = sqlx::query!("SELECT id, nick, username, avatar FROM members;")
        .fetch_all(&mut *connection)
        .await
        .unwrap();
    let all_users = all_users.into_iter().map(|it| Member {
        id: it.id as u64,
        nick: it.nick,
        username: it.username,
        avatar: it.avatar,
    });

    let results = fuzzy_search(
        &user,
        all_users,
        fuzzy_searcher!(Member, |member| {
            [
                member.username.as_str(),
                member.nick.as_ref().map_or("", |v| v),
            ]
            .into_iter()
        }),
    );

    if let Some((_, member)) = results.into_iter().next() {
        Some(Owner::Member(MemberOwner::Resolved(member)))
    } else {
        None
    }
}

pub async fn lookup_storage(
    connection: &mut PgConnection,
    location: &str,
) -> color_eyre::Result<Vec<Location>> {
    let item = location.to_lowercase();
    let exact_matches: Vec<_> = sqlx::query_as!(
        Location,
        "SELECT id, strid, name FROM storage WHERE strid = $1 OR name = $2",
        item,
        item
    )
    .fetch_all(&mut *connection)
    .await?;

    let fuzzy_matches = sqlx::query_as!(Location, "SELECT id, strid, name FROM storage;")
        .fetch_all(&mut *connection)
        .await?
        .into_iter();

    let results = fuzzy_search(
        &item,
        fuzzy_matches,
        fuzzy_searcher_once!(Location, |location| location.name.as_str()),
    )
    .into_iter()
    .filter_map(|(_, location)| {
        if !exact_matches.contains(&location) {
            Some(location)
        } else {
            None
        }
    })
    .collect::<Vec<_>>();

    Ok(exact_matches.into_iter().chain(results).collect())
}

pub async fn lookup_item(
    connection: &mut PgConnection,
    item: &str,
) -> color_eyre::Result<Vec<Item>> {
    let item = item.to_lowercase();
    let exact_matches: Vec<_> = sqlx::query_as!(
        Item,
        "SELECT id, strid, name FROM items WHERE strid = $1 OR name = $2",
        item,
        item
    )
    .fetch_all(&mut *connection)
    .await?;

    let fuzzy_matches = sqlx::query_as!(Item, "SELECT id, strid, name FROM items;")
        .fetch_all(&mut *connection)
        .await?
        .into_iter();

    let results = fuzzy_search(
        &item,
        fuzzy_matches,
        fuzzy_searcher_once!(Item, |i| i.name.as_str()),
    )
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

pub async fn get_item_by_id(
    connection: &mut PgConnection,
    id: i32,
) -> color_eyre::Result<Option<Item>> {
    let item = sqlx::query_as!(Item, "SELECT id, strid, name FROM items WHERE id = $1", id)
        .fetch_optional(&mut *connection)
        .await?;
    Ok(item)
}

async fn get_discord_info(snowflake: u64) -> color_eyre::Result<Member> {
    const SEDS_GUILD_ID: u64 = 755426438185877614;

    let discord_token = std::env::var("DISCORD_TOKEN").expect("DISCORD_TOKEN not set");
    let client = reqwest::Client::new();
    let url = format!(
        "https://discord.com/api/v10/guilds/{}/members/{}",
        SEDS_GUILD_ID, snowflake
    );
    let response = client
        .get(url)
        .header("Authorization", &format!("Bot {}", discord_token))
        .send()
        .await?;

    let body = response.bytes().await?;
    let json: Value = serde_json::from_slice(&body)?;
    let nick = json.get("nick").and_then(Value::as_str).map(str::to_string);
    let user = json.get("user").and_then(Value::as_object);

    let id = user
        .and_then(|u| u.get("id"))
        .and_then(Value::as_str)
        .and_then(|s| s.parse::<u64>().ok());
    let username = user.and_then(|u| u.get("username")).and_then(Value::as_str);

    let avatar = user
        .and_then(|u| u.get("avatar"))
        .and_then(Value::as_str)
        .map(str::to_string);

    if let (Some(id), Some(username)) = (id, username) {
        Ok(Member {
            id,
            username: username.to_string(),
            nick,
            avatar,
        })
    } else {
        Err(color_eyre::eyre::eyre!("Malformed Discord response"))
    }
}

pub async fn get_owner_info(conn: &mut PgConnection, owner_str: &str) -> color_eyre::Result<Owner> {
    if let Some(loc) = owner_str.strip_prefix("loc:") {
        let location = lookup_storage(&mut *conn, loc)
            .await?
            .into_iter()
            .next()
            .ok_or(color_eyre::eyre::eyre!("No such location"))?;

        Ok(Owner::Location(location))
    } else {
        // Owned by a user
        let snowflake = owner_str.parse::<u64>().wrap_err("Non snowflake in DB")?;
        try_resolve_member(conn, snowflake).await.map(Owner::Member)
    }
}

pub async fn get_owner(
    connection: &mut PgConnection,
    item_id: i32,
) -> color_eyre::Result<Option<Owner>> {
    let owner = get_last_holder(&mut *connection, item_id).await?;
    if let Some(owner) = owner {
        Ok(Some(get_owner_info(&mut *connection, &owner).await?))
    } else {
        Ok(None)
    }
}

pub async fn get_last_holder(
    connection: &mut PgConnection,
    item_id: i32,
) -> color_eyre::Result<Option<String>> {
    let holder = sqlx::query!(
        "SELECT to_user FROM borrow WHERE item_id = $1 ORDER BY ordering DESC LIMIT 1",
        item_id
    )
    .fetch_optional(&mut *connection)
    .await?;
    Ok(holder.map(|row| row.to_user))
}

#[derive(Debug, Clone)]
pub struct BorrowUpdates {
    pub updated_items: ItemTree,
    pub present_updates: Vec<(Item, Item, bool)>,
}

pub async fn borrow_item(
    connection: &mut PgConnection,
    item: &Item,
    user: &str,
) -> color_eyre::Result<BorrowUpdates> {
    let now = PrimitiveDateTime::new(
        OffsetDateTime::now_utc().date(),
        OffsetDateTime::now_utc().time(),
    );
    let items = box_contents(connection, item).await?;
    for (_, node, _) in items.iter_depth_first().filter(|(_, node, _)| node.present) {
        sqlx::query!(
            "INSERT INTO borrow (item_id, to_user, time) VALUES ($1, $2, $3);",
            node.item.id,
            user,
            now,
        )
        .execute(&mut *connection)
        .await?;
    }

    let mut present_updates = Vec::new();
    let owned_items = get_items_by_owner(connection, user)
        .await?
        .iter()
        .map(|i| i.id)
        .collect::<Vec<_>>();

    // Mark items as present if the parent is owned by the borrower
    for (_, node, _) in items.iter_depth_first().filter(|(_, node, _)| node.present) {
        let mut builder =
            sqlx::QueryBuilder::new("UPDATE meta SET present = (parent = any (array[");
        let mut sep = builder.separated(",");
        owned_items.iter().for_each(|it| {
            sep.push_bind(it);
        });
        builder
            .push("]::integer[])) WHERE child = ")
            .push_bind(node.item.id)
            .push(" AND present != (parent = any (array[");
        let mut sep = builder.separated(",");
        owned_items.iter().for_each(|it| {
            sep.push_bind(it);
        });
        builder.push("]::integer[])) RETURNING parent, present;");

        let query = builder.build();
        let result = query.fetch_optional(&mut *connection).await?;

        if let Some(row) = result {
            let parent: i32 = row.get(0);
            let box_item = get_item_by_id(connection, parent).await?.unwrap();
            let present = row.get(1);
            present_updates.push((box_item, node.item.clone(), present));
        }
    }

    // Mark items owned by the borrower as present if the parent is being borrowed
    for owned_item in owned_items.iter() {
        let mut builder = sqlx::QueryBuilder::new("UPDATE meta SET present = true ");
        builder
            .push("WHERE child = ")
            .push_bind(owned_item)
            .push(" AND (parent = any (array[");
        let mut sep = builder.separated(",");
        items
            .iter_depth_first()
            .filter(|(_, node, _)| node.present)
            .for_each(|(_, node, _)| {
                sep.push_bind(node.item.id);
            });
        builder.push("]::integer[])) AND present = false RETURNING parent, present;");

        let query = builder.build();
        let result = query.fetch_optional(&mut *connection).await?;

        if let Some(row) = result {
            let parent: i32 = row.get(0);
            let box_item = get_item_by_id(connection, parent).await?.unwrap();
            let present = row.get(1);
            present_updates.push((
                box_item,
                get_item_by_id(connection, *owned_item).await?.unwrap(),
                present,
            ));
        }
    }

    let updated_items: ItemTree = items
        .into_iter_depth_first()
        .filter(|(_, node, _)| node.present)
        .collect();
    Ok(BorrowUpdates {
        updated_items,
        present_updates,
    })
}

pub async fn borrow_history(
    connection: &mut PgConnection,
    item_id: i32,
) -> color_eyre::Result<Vec<(String, OffsetDateTime)>> {
    let history = sqlx::query!(
        "SELECT to_user, time FROM borrow WHERE item_id = $1 ORDER BY ordering DESC",
        item_id
    )
    .fetch_all(&mut *connection)
    .await?;
    Ok(history
        .into_iter()
        .map(|row| {
            (
                row.to_user,
                OffsetDateTime::new_utc(row.time.date(), row.time.time()),
            )
        })
        .collect())
}

pub async fn register_item(
    connection: &mut PgConnection,
    strid: &str,
    name: &str,
) -> color_eyre::Result<()> {
    sqlx::query!(
        "INSERT INTO items (strid, name) VALUES ($1, $2);",
        strid,
        name,
    )
    .execute(&mut *connection)
    .await?;
    Ok(())
}

pub async fn get_items_by_owner(
    pool: &mut PgConnection,
    owner: &str,
) -> color_eyre::Result<Vec<Item>> {
    let connection = pool.acquire().await?;
    let items = sqlx::query_as!(
        Item,
        "SELECT i.id, i.strid, i.name
        FROM borrow b
        JOIN (
            SELECT item_id, MAX(ordering) AS max_ordering
            FROM borrow
            GROUP BY item_id
        ) AS max_orders
        ON b.item_id = max_orders.item_id 
        JOIN items i ON b.item_id = i.id
        WHERE b.ordering = max_orders.max_ordering AND b.to_user = $1;",
        owner
    )
    .fetch_all(&mut *connection)
    .await?;
    Ok(items)
}

#[derive(Debug)]
pub enum BoxingError {
    NonEuclidean { prior_parent: Item, item: Item },
    AlreadyBoxed { prior_parent: Item, item: Item },
    Other(color_eyre::eyre::Error),
}
impl From<sqlx::Error> for BoxingError {
    fn from(err: sqlx::Error) -> Self {
        Self::Other(err.into())
    }
}

impl From<color_eyre::eyre::Error> for BoxingError {
    fn from(err: color_eyre::eyre::Error) -> Self {
        Self::Other(err)
    }
}

impl std::fmt::Display for BoxingError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::NonEuclidean { prior_parent, item } => write!(
                f,
                "Non-euclidean boxes not yet supported. '{}' was previously in box '{}', but will now be a parent of '{}'.",
                item.name, prior_parent.name, prior_parent.name
            ),
            Self::AlreadyBoxed {
                item,
                prior_parent: prior_box,
            } => {
                write!(f, "'{}' is already in box {}.", item.name, prior_box.name)
            }
            Self::Other(err) => write!(f, "An error occurred: {}", err),
        }
    }
}

impl std::error::Error for BoxingError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::Other(err) => Some(err.as_ref()),
            _ => None,
        }
    }
}

pub async fn box_all(
    connection: &mut PgConnection,
    owner: &str,
    r#box: &Item,
    items: &[Item],
) -> Result<(), BoxingError> {
    for item in items {
        let contents = box_contents(connection, item).await?;
        if contents.find(r#box.id).is_some() {
            return Err(BoxingError::NonEuclidean {
                item: r#box.clone(),
                prior_parent: item.clone(),
            });
        }

        if let Some(prior_parent) =
            sqlx::query_as!(Item,
            "SELECT i.id, i.strid, i.name FROM meta JOIN items i ON meta.parent = i.id WHERE meta.child = $1",
            item.id
        )
            .fetch_optional(&mut *connection)
            .await?
        {
            return Err(BoxingError::AlreadyBoxed {
                prior_parent,
                item: item.clone(),
            });
        }
    }

    let owned_items = get_items_by_owner(connection, owner).await?;
    for item in items {
        let owned = owned_items.iter().any(|i| i.id == item.id);
        sqlx::query!(
            "INSERT INTO meta (parent, child, present) VALUES ($1, $2, $3);",
            r#box.id,
            item.id,
            owned,
        )
        .execute(&mut *connection)
        .await?;
    }

    Ok(())
}

#[derive(Debug)]
pub enum UnboxingError {
    NotFound { item: Item, r#box: Item },
    Other(color_eyre::eyre::Error),
}

impl Display for UnboxingError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::NotFound { item, r#box } => {
                write!(f, "'{}' is not in box '{}'.", item.name, r#box.name)
            }
            Self::Other(err) => write!(f, "An error occurred: {}", err),
        }
    }
}

impl std::error::Error for UnboxingError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::Other(err) => Some(err.as_ref()),
            _ => None,
        }
    }
}

impl From<sqlx::Error> for UnboxingError {
    fn from(err: sqlx::Error) -> Self {
        Self::Other(err.into())
    }
}

pub async fn parent_box(
    connection: &mut PgConnection,
    item: &Item,
) -> color_eyre::Result<Option<Item>> {
    let parent = sqlx::query_as!(
        Item,
        "SELECT i.id, i.strid, i.name FROM meta JOIN items i ON meta.parent = i.id WHERE meta.child = $1",
        item.id
    )
    .fetch_optional(&mut *connection)
    .await?;
    Ok(parent)
}

pub async fn find_location_name(
    connection: &mut PgConnection,
    strid: &str,
) -> color_eyre::Result<Option<Location>> {
    let location = sqlx::query_as!(
        Location,
        "SELECT id, strid, name FROM storage WHERE strid = $1",
        strid
    )
    .fetch_optional(&mut *connection)
    .await?;
    Ok(location)
}

pub async fn unbox_all(
    connection: &mut PgConnection,
    r#box: &Item,
    items: &[Item],
) -> Result<(), UnboxingError> {
    for item in items {
        if sqlx::query!(
            "DELETE FROM meta WHERE child = $1 AND parent = $2 RETURNING child;",
            item.id,
            r#box.id,
        )
        .fetch_optional(&mut *connection)
        .await?
        .is_none()
        {
            return Err(UnboxingError::NotFound {
                item: item.clone(),
                r#box: r#box.clone(),
            });
        }
    }
    Ok(())
}

pub async fn box_contents(
    connection: &mut PgConnection,
    r#box: &Item,
) -> color_eyre::Result<ItemTree> {
    let mut root = ItemTree::new(r#box.clone(), true);
    let mut open_set = VecDeque::new();
    open_set.push_back(&mut root);
    while let Some(tree) = open_set.pop_front() {
        let children = sqlx::query!(
            "SELECT i.id, i.name, i.strid, m.present
                FROM meta m
                JOIN items i ON m.child = i.id
                WHERE m.parent = $1",
            tree.item.item.id
        )
        .fetch_all(&mut *connection)
        .await?;

        let child_trees = children.into_iter().map(|item| {
            ItemTree::new(
                Item {
                    id: item.id,
                    strid: item.strid,
                    name: item.name,
                },
                item.present,
            )
        });

        open_set.extend(tree.add_children(child_trees));
    }
    Ok(root)
}

pub async fn box_contents_with_owner(
    pool: &mut PgPool,
    r#box: &Item,
) -> color_eyre::Result<ItemTreeOwned> {
    let mut root: ItemTreeOwned = ItemTreeOwned::new_associated(
        r#box.clone(),
        true,
        get_owner(&mut *pool.acquire().await?, r#box.id).await?,
    );
    let mut open_set = VecDeque::new();
    open_set.push_back(&mut root);
    while let Some(tree) = open_set.pop_front() {
        let children = sqlx::query!(
            "SELECT i.id, i.name, i.strid, m.present, (SELECT to_user FROM borrow WHERE item_id = i.id ORDER BY ordering DESC LIMIT 1) AS owner
                FROM meta m
                JOIN items i ON m.child = i.id
                WHERE m.parent = $1",
            tree.item.item.id
        )
        .fetch_all(&mut *pool.acquire().await?)
        .await?;

        let child_trees = children.into_iter().map(async |item| {
            let owner = if let Some(owner_str) = item.owner {
                Some(get_owner_info(&mut *pool.acquire().await?, &owner_str).await?)
            } else {
                None
            };

            Ok::<_, color_eyre::Report>(ItemTreeOwned::new_associated(
                Item {
                    id: item.id,
                    strid: item.strid,
                    name: item.name,
                },
                item.present,
                owner,
            ))
        });

        let child_trees = futures::future::try_join_all(child_trees)
            .await?
            .into_iter();

        open_set.extend(tree.add_children(child_trees));
    }
    Ok(root)
}

pub async fn get_all_items(conn: &mut PgConnection) -> color_eyre::Result<Vec<Item>> {
    let items = sqlx::query_as!(Item, "SELECT id, strid, name FROM items;")
        .fetch_all(&mut *conn)
        .await?;
    Ok(items)
}

pub async fn get_all_itemtrees(
    connection: &mut PgConnection,
) -> color_eyre::Result<HashMap<Option<String>, Vec<ItemTree>>> {
    let mut trees = HashMap::new();
    let roots = get_itemtree_roots(connection).await?;
    for (owner, item) in roots {
        let tree = box_contents(&mut *connection, &item).await?;
        trees.entry(owner).or_insert(Vec::new()).push(tree);
    }

    Ok(trees)
}

pub async fn get_all_itemtrees_with_owner(
    pool: &mut PgPool,
) -> color_eyre::Result<Vec<ItemTreeOwned>> {
    let mut trees = Vec::new();
    let roots = get_itemtree_roots(&mut *pool.acquire().await?).await?;
    for (_, item) in roots {
        let tree = box_contents_with_owner(pool, &item).await?;
        trees.push(tree);
    }

    Ok(trees)
}

pub async fn get_itemtree_roots(
    connection: &mut PgConnection,
) -> color_eyre::Result<Vec<(Option<String>, Item)>> {
    Ok(sqlx::query_as(
        "SELECT b.to_user, i.id, i.strid, i.name
            FROM borrow b
            JOIN (
                SELECT item_id, MAX(ordering) AS max_ordering
                FROM borrow GROUP BY item_id
            ) AS max_orders
            ON b.item_id = max_orders.item_id 
            LEFT JOIN meta m ON b.item_id = m.child
            RIGHT JOIN items i ON b.item_id = i.id
            WHERE (b.ordering = max_orders.max_ordering 
                OR max_orders.max_ordering IS NULL) 
                AND (m.parent IS NULL OR m.present IS FALSE);",
    )
    .fetch_all(&mut *connection)
    .await?
    .into_iter()
    .map(|(owner, id, strid, name)| (owner, Item { id, strid, name }))
    .collect())
}

pub async fn get_itemtrees(
    connection: &mut PgConnection,
    user: impl AsRef<str>,
) -> color_eyre::Result<Vec<ItemTree>> {
    let single_conn = connection.acquire().await?;
    let user = user.as_ref();
    let roots = sqlx::query_as!(
        Item,
            "SELECT i.id, i.strid, i.name
            FROM borrow b
            JOIN (
                SELECT item_id, MAX(ordering) AS max_ordering
                FROM borrow
                GROUP BY item_id
            ) AS max_orders
            ON b.item_id = max_orders.item_id 
            LEFT JOIN meta m ON b.item_id = m.child
            JOIN items i ON b.item_id = i.id
            WHERE b.ordering = max_orders.max_ordering AND (m.parent IS NULL OR m.present IS FALSE) AND b.to_user = $1;", 
            user
    )
    .fetch_all(&mut *single_conn)
    .await?;

    let mut trees = Vec::new();
    for item in roots {
        let tree = box_contents(&mut *single_conn, &item).await?;
        trees.push(tree);
    }

    Ok(trees)
}

/// Delete the given item, return a tree containing the previous children which are now orphans
pub async fn delete_item(
    connection: &mut PgConnection,
    item: &Item,
) -> color_eyre::Result<ItemTree> {
    let tree = box_contents(connection, item).await?;

    sqlx::query!("DELETE FROM items WHERE id = $1;", item.id)
        .execute(&mut *connection)
        .await?;
    Ok(tree)
}

pub async fn list_storage(
    pool: &PgPool,
    location: Option<Location>,
) -> color_eyre::Result<HashMap<Location, Vec<ItemTree>>> {
    let locations = if let Some(location) = location {
        vec![location]
    } else {
        sqlx::query_as!(Location, "SELECT id, strid, name FROM storage;")
            .fetch_all(&mut *pool.acquire().await?)
            .await?
            .into_iter()
            .collect()
    };

    let mut itemtrees: HashMap<Location, Vec<ItemTree>> = HashMap::new();
    for location in locations {
        let trees = get_itemtrees(
            &mut *pool.acquire().await?,
            format!("loc:{}", location.strid),
        )
        .await?;
        itemtrees.insert(location, trees);
    }

    Ok(itemtrees)
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Member {
    pub id: u64,
    pub nick: Option<String>,
    pub username: String,
    pub avatar: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct JsSafeMember {
    pub id: String,
    pub nick: Option<String>,
    pub username: String,
    pub avatar: Option<String>,
}

impl Member {
    /// JS has a single numeric type and it is fuckING 64-bit floating point.
    /// So this fucking abomination is necessary to avoid loss of precision
    /// when representing the u64 `id`.
    pub fn to_js_safe(&self) -> JsSafeMember {
        JsSafeMember {
            id: self.id.to_string(),
            nick: self.nick.clone(),
            avatar: self.avatar.clone(),
            username: self.username.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum MemberOwner {
    Resolved(Member),
    Unresolved(u64),
}

#[derive(Debug, Clone)]
pub enum Owner {
    Location(Location),
    Member(MemberOwner),
}

impl<'de> Deserialize<'de> for Owner {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        s.as_str().try_into().map_err(serde::de::Error::custom)
    }
}

impl TryFrom<&str> for Owner {
    type Error = &'static str;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        if let Some(loc) = value.strip_prefix("loc:") {
            Ok(Owner::Location(Location {
                id: 0,
                name: loc.to_string(),
                strid: loc.to_string(),
            }))
        } else if let Ok(snowflake) = value.parse::<u64>() {
            Ok(Owner::Member(MemberOwner::Unresolved(snowflake)))
        } else {
            Err("Invalid owner string")
        }
    }
}

impl Owner {
    pub fn snowflake(&self) -> Option<u64> {
        match self {
            Owner::Member(MemberOwner::Resolved(member)) => Some(member.id),
            Owner::Member(MemberOwner::Unresolved(snowflake)) => Some(*snowflake),
            _ => None,
        }
    }

    pub fn db_string(&self) -> String {
        match self {
            Owner::Location(loc) => format!("loc:{}", loc.strid),
            Owner::Member(MemberOwner::Resolved(member)) => member.id.to_string(),
            Owner::Member(MemberOwner::Unresolved(snowflake)) => snowflake.to_string(),
        }
    }
}

impl Display for Owner {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Owner::Location(location) => f.write_str(&location.name),
            Owner::Member(MemberOwner::Resolved(member)) => f.write_str(member
                .nick
                .as_ref()
                .unwrap_or(&member.username)),
            Owner::Member(MemberOwner::Unresolved(id)) => f.write_fmt(format_args!("{}", id)),
        }
    }
}

pub async fn try_resolve_member(
    conn: &mut PgConnection,
    snowflake: u64,
) -> color_eyre::Result<MemberOwner> {
    if let Some(owner) = find_cached_member_info(&mut *conn, snowflake)
        .await?
        .map(|it| {
            if it.username != "#unresolved" {
                MemberOwner::Resolved(it)
            } else {
                MemberOwner::Unresolved(snowflake)
            }
        })
    {
        Ok(owner)
    } else if let Ok(member) = get_discord_info(snowflake).await {
        // Cache this info
        cache_member_info(&mut *conn, &member).await?;
        Ok(MemberOwner::Resolved(member))
    } else {
        Ok(MemberOwner::Unresolved(snowflake))
    }
}

pub async fn get_all_cached_members(conn: &mut PgConnection) -> color_eyre::Result<Vec<Member>> {
    struct ShittyI64Member {
        id: i64,
        nick: Option<String>,
        username: String,
        avatar: Option<String>,
    }

    let i64_member = sqlx::query_as!(ShittyI64Member, "SELECT * FROM members")
        .fetch_all(conn)
        .await?;

    Ok(i64_member
        .into_iter()
        .map(
            |ShittyI64Member {
                 id,
                 nick,
                 username,
                 avatar,
             }| Member {
                id: id as u64,
                nick,
                username,
                avatar,
            },
        )
        .collect())
}

pub async fn find_cached_member_info(
    conn: &mut PgConnection,
    snowflake: u64,
) -> color_eyre::Result<Option<Member>> {
    struct ShittyI64Member {
        id: i64,
        nick: Option<String>,
        username: String,
        avatar: Option<String>,
    }

    let i64_member = sqlx::query_as!(
        ShittyI64Member,
        "SELECT * FROM members WHERE id = $1",
        snowflake as i64
    )
    .fetch_optional(conn)
    .await?;

    Ok(i64_member.map(
        |ShittyI64Member {
             id,
             nick,
             username,
             avatar,
         }| Member {
            id: id as u64,
            nick,
            username,
            avatar,
        },
    ))
}

pub async fn cache_member_info(conn: &mut PgConnection, member: &Member) -> color_eyre::Result<()> {
    sqlx::query!(
        "INSERT INTO members (id, nick, username, avatar) VALUES ($1, $2, $3, $4)
        ON CONFLICT (id) DO UPDATE SET nick = EXCLUDED.nick, username = EXCLUDED.username, avatar = EXCLUDED.avatar;",
        member.id as i64,
        member.nick,
        member.username,
        member.avatar,
    )
    .execute(conn)
    .await?;
    Ok(())
}
