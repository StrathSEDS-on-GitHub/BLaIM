use std::collections::VecDeque;
use std::sync::LazyLock;

use chrono::{DateTime, Utc};
use fuzzy_matcher::skim::SkimMatcherV2;
use fuzzy_matcher::FuzzyMatcher;
use sqlx::SqlitePool;

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

pub(crate) async fn delete_borrow(pool: &SqlitePool, borrow_id: i32) -> anyhow::Result<()> {
    let mut connection = pool.acquire().await?;
    sqlx::query!("DELETE FROM borrow WHERE ordering = ?", borrow_id,)
        .execute(&mut *connection)
        .await?;
    Ok(())
}

pub(crate) async fn update_borrow_item(
    pool: &SqlitePool,
    borrow_id: i32,
    item_id: i64,
) -> anyhow::Result<()> {
    let mut connection = pool.acquire().await?;
    sqlx::query!(
        "UPDATE borrow SET item_id = ? WHERE ordering = ?",
        item_id,
        borrow_id,
    )
    .execute(&mut *connection)
    .await?;
    Ok(())
}

pub(crate) async fn borrow_history(
    pool: &SqlitePool,
    item_id: i64,
) -> anyhow::Result<Vec<(String, DateTime<Utc>)>> {
    let mut connection = pool.acquire().await?;
    let history = sqlx::query!(
        "SELECT to_user, time FROM borrow WHERE item_id = ? ORDER BY ordering DESC",
        item_id
    )
    .fetch_all(&mut *connection)
    .await?;
    Ok(history
        .into_iter()
        .map(|row| {
            (
                row.to_user,
                DateTime::from_naive_utc_and_offset(row.time, Utc),
            )
        })
        .collect())
}

pub(crate) async fn register_item(
    pool: &SqlitePool,
    strid: &str,
    name: &str,
) -> anyhow::Result<()> {
    let mut connection = pool.acquire().await?;
    sqlx::query!(
        "INSERT INTO items (strid, name) VALUES (?, ?);",
        strid,
        name,
    )
    .execute(&mut *connection)
    .await?;
    Ok(())
}

pub(crate) async fn get_items_by_owner(
    pool: &SqlitePool,
    owner: &str,
) -> anyhow::Result<Vec<Item>> {
    let mut connection = pool.acquire().await?;
    let items = sqlx::query_as!(
        Item,
        "SELECT i.id, i.name
        FROM borrow b
        JOIN (
            SELECT item_id, MAX(ordering) AS max_ordering
            FROM borrow
            GROUP BY item_id
        ) AS max_orders
        ON b.item_id = max_orders.item_id 
        JOIN items i ON b.item_id = i.id
        WHERE b.ordering = max_orders.max_ordering AND b.to_user = ?;",
        owner
    )
    .fetch_all(&mut *connection)
    .await?;
    Ok(items)
}

#[derive(Debug)]
pub enum BoxingError {
    NonEuclidean { prior_parent: Item, item: Item },
    AlreadyBoxed { prior_box: Item, item: Item },
    Other(anyhow::Error),
}
impl From<sqlx::Error> for BoxingError {
    fn from(err: sqlx::Error) -> Self {
        Self::Other(err.into())
    }
}

impl From<anyhow::Error> for BoxingError {
    fn from(err: anyhow::Error) -> Self {
        Self::Other(err)
    }
}

impl std::fmt::Display for BoxingError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::NonEuclidean {
                prior_parent,
                item
            } => write!(
                f,
                "Non-euclidean boxes not yet supported. '{}' was previously in box '{}', but will now be a parent of '{}'.",
                item.name, prior_parent.name, item.name
            ),
            Self::AlreadyBoxed { item, prior_box } => {
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

    fn description(&self) -> &str {
        match self {
            Self::NonEuclidean { .. } => "Non-euclidean boxes not yet supported.",
            Self::AlreadyBoxed { .. } => "Item is already in a box.",
            Self::Other(_) => "An error occurred.",
        }
    }
}

pub(crate) async fn box_all(
    pool: &SqlitePool,
    owner: &str,
    r#box: &Item,
    items: &[Item],
) -> Result<(), BoxingError> {
    let mut connection = pool.acquire().await?;
    for item in items {
        let contents = box_contents(pool, item).await?;
        if contents.find(r#box.id).is_some() {
            return Err(BoxingError::AlreadyBoxed {
                item: item.clone(),
                prior_box: r#box.clone(),
            });
        }

        if let Some(prior_parent) =
            sqlx::query_as!(Item,
            "SELECT i.id, i.name FROM meta JOIN items i ON meta.parent = i.id WHERE meta.child = ?",
            item.id
        )
            .fetch_optional(&mut *connection)
            .await?
        {
            return Err(BoxingError::NonEuclidean {
                prior_parent,
                item: item.clone(),
            });
        }
    }

    let owned_items = get_items_by_owner(pool, owner).await?;
    for item in items {
        let owned = owned_items.iter().any(|i| i.id == item.id);
        sqlx::query!(
            "INSERT INTO meta (parent, child, present) VALUES (?, ?, ?);",
            r#box.id,
            item.id,
            owned,
        )
        .execute(&mut *connection)
        .await?;
    }

    Ok(())
}

#[derive(Debug, Clone)]
pub(crate) struct ItemTreeNode {
    pub item: Item,
    pub present: bool,
}

#[derive(Debug, Clone)]
pub(crate) struct ItemTree {
    pub item: ItemTreeNode,
    pub children: Vec<ItemTree>,
}

impl ItemTree {
    pub fn new(item: Item, present: bool) -> Self {
        Self {
            item: ItemTreeNode { item, present },
            children: Vec::new(),
        }
    }

    fn add_children(
        &mut self,
        children: impl Iterator<Item = ItemTree>,
    ) -> impl Iterator<Item = &mut ItemTree> {
        let start = self.children.len();
        self.children.extend(children);

        self.children[start..].iter_mut()
    }

    fn find(&self, id: i64) -> Option<&ItemTree> {
        if self.item.item.id == id {
            return Some(self);
        }
        for child in &self.children {
            if let Some(tree) = child.find(id) {
                return Some(tree);
            }
        }
        None
    }

    pub fn iter_depth_first(&self) -> impl Iterator<Item = (usize, &ItemTree, bool)> {
        let mut stack = vec![(0, self, true)];
        std::iter::from_fn(move || {
            if let Some((depth, tree, s)) = stack.pop() {
                if let Some((last, most)) = tree.children.split_last() {
                    stack.push((depth + 1, last, true));
                    most.iter().rev().for_each(|child| {
                        stack.push((depth + 1, child, false));
                    });
                }

                Some((depth, tree, s))
            } else {
                None
            }
        })
    }
}

pub(crate) async fn box_contents(pool: &SqlitePool, r#box: &Item) -> anyhow::Result<ItemTree> {
    let mut connection = pool.acquire().await?;
    let mut root = ItemTree::new(r#box.clone(), true);
    let mut open_set = VecDeque::new();
    open_set.push_back(&mut root);
    while let Some(tree) = open_set.pop_front() {
        let children = sqlx::query!(
            "SELECT i.id, i.name, m.present
                FROM meta m
                JOIN items i ON m.child = i.id
                WHERE m.parent = ?",
            tree.item.item.id
        )
        .fetch_all(&mut *connection)
        .await?;

        let child_trees = children.into_iter().map(|item| {
            ItemTree::new(
                Item {
                    id: item.id,
                    name: item.name,
                },
                item.present,
            )
        });

        open_set.extend(tree.add_children(child_trees));
    }
    Ok(root)
}
