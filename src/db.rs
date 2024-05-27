use std::collections::VecDeque;
use std::fmt::{self, Display};
use std::sync::LazyLock;

use chrono::{DateTime, Utc};
use fuzzy_matcher::skim::SkimMatcherV2;
use fuzzy_matcher::FuzzyMatcher;
use sqlx::{Acquire, Row, SqliteConnection};

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Item {
    pub id: i64,
    pub name: String,
    pub strid: String,
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
    scores.splice(scores.len().min(5).., []);
    scores
}

pub(crate) async fn lookup_item(
    connection: &mut SqliteConnection,
    item: &str,
) -> anyhow::Result<Vec<Item>> {
    let item = item.to_lowercase();
    let exact_matches: Vec<_> = sqlx::query_as!(
        Item,
        "SELECT id, strid, name FROM items WHERE strid = ? OR name = ?",
        item,
        item
    )
    .fetch_all(&mut *connection)
    .await?;

    let fuzzy_matches = sqlx::query_as!(Item, "SELECT id, strid, name FROM items;")
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

pub(crate) async fn get_item_by_id(
    connection: &mut SqliteConnection,
    id: i64,
) -> anyhow::Result<Option<Item>> {
    let item = sqlx::query_as!(Item, "SELECT id, strid, name FROM items WHERE id = ?", id)
        .fetch_optional(&mut *connection)
        .await?;
    Ok(item)
}

pub(crate) async fn get_last_holder(
    connection: &mut SqliteConnection,
    item_id: i64,
) -> anyhow::Result<Option<String>> {
    let holder = sqlx::query!(
        "SELECT to_user FROM borrow WHERE item_id = ? ORDER BY ordering DESC LIMIT 1",
        item_id
    )
    .fetch_optional(&mut *connection)
    .await?;
    Ok(holder.map(|row| row.to_user))
}

pub(crate) async fn borrow_item(
    connection: &mut SqliteConnection,
    item: &Item,
    user: &str,
) -> anyhow::Result<(ItemTree, Vec<(Item, Item, bool)>)> {
    let now = sqlx::types::chrono::Utc::now();
    let items = box_contents(connection, item).await?;
    for (_, node, _) in items.iter_depth_first().filter(|(_, node, _)| node.present) {
        sqlx::query!(
            "INSERT INTO borrow (item_id, to_user, time) VALUES (?, ?, ?);",
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
    for (_, node, _) in items.iter_depth_first().filter(|(_, node, _)| node.present) {
        let mut builder = sqlx::QueryBuilder::new("UPDATE meta SET present = (parent IN (");
        let mut sep = builder.separated(",");
        owned_items.iter().for_each(|it| {
            sep.push_bind(it);
        });
        builder
            .push(")) WHERE child = ")
            .push_bind(node.item.id)
            .push(" AND present != (parent IN (");
        let mut sep = builder.separated(",");
        owned_items.iter().for_each(|it| {
            sep.push_bind(it);
        });
        builder.push(")) RETURNING parent, present;");

        let mut query = builder.build();

        for i in owned_items.iter() {
            query = query.bind(i);
        }
        for i in owned_items.iter() {
            query = query.bind(i);
        }

        let result = query.fetch_optional(&mut *connection).await?;

        if let Some(row) = result {
            let parent: i64 = row.get(0);
            let box_item = get_item_by_id(connection, parent).await?.unwrap();
            let present = row.get(1);
            present_updates.push((box_item, node.item.clone(), present));
        }
    }

    let update_tree: ItemTree = items
        .into_iter_depth_first()
        .filter(|(_, node, _)| node.present)
        .collect();
    Ok((update_tree, present_updates))
}

pub(crate) async fn borrow_history(
    connection: &mut SqliteConnection,
    item_id: i64,
) -> anyhow::Result<Vec<(String, DateTime<Utc>)>> {
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
    connection: &mut SqliteConnection,
    strid: &str,
    name: &str,
) -> anyhow::Result<()> {
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
    pool: &mut SqliteConnection,
    owner: &str,
) -> anyhow::Result<Vec<Item>> {
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
    AlreadyBoxed { prior_parent: Item, item: Item },
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
                item.name, prior_parent.name, prior_parent.name
            ),
            Self::AlreadyBoxed { item, prior_parent: prior_box } => {
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

pub(crate) async fn box_all(
    connection: &mut SqliteConnection,
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
            "SELECT i.id, i.strid, i.name FROM meta JOIN items i ON meta.parent = i.id WHERE meta.child = ?",
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

#[derive(Debug)]
pub enum UnboxingError {
    NotFound { item: Item, r#box: Item },
    Other(anyhow::Error),
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

pub(crate) async fn unbox_all(
    connection: &mut SqliteConnection,
    r#box: &Item,
    items: &[Item],
) -> Result<(), UnboxingError> {
    for item in items {
        if let None = sqlx::query!(
            "DELETE FROM meta WHERE child = ? AND parent = ? RETURNING child;",
            item.id,
            r#box.id,
        )
        .fetch_optional(&mut *connection)
        .await?
        {
            return Err(UnboxingError::NotFound {
                item: item.clone(),
                r#box: r#box.clone(),
            });
        }
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

    pub fn iter_depth_first(&self) -> impl Iterator<Item = (usize, &ItemTreeNode, bool)> {
        let mut stack = vec![(0, self, true)];
        std::iter::from_fn(move || {
            if let Some((depth, tree, s)) = stack.pop() {
                if let Some((last, most)) = tree.children.split_last() {
                    stack.push((depth + 1, last, true));
                    most.iter().rev().for_each(|child| {
                        stack.push((depth + 1, child, false));
                    });
                }

                Some((depth, &tree.item, s))
            } else {
                None
            }
        })
    }

    pub fn into_iter_depth_first(self) -> impl Iterator<Item = (usize, ItemTreeNode, bool)> {
        let mut stack = vec![(0, self, true)];
        std::iter::from_fn(move || {
            if let Some((depth, tree, s)) = stack.pop() {
                if let Some((last, most)) = tree.children.split_last() {
                    stack.push((depth + 1, last.clone(), true));
                    most.iter().rev().for_each(|child| {
                        stack.push((depth + 1, child.clone(), false));
                    });
                }

                Some((depth, tree.item, s))
            } else {
                None
            }
        })
    }
}

impl Display for ItemTree {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.iter_depth_first()
            .try_for_each(|(depth, it, last_child)| {
                let prefix_size = depth * 4;
                let tree_icon = if depth == 0 {
                    "  "
                } else if last_child {
                    "└─"
                } else {
                    "├─"
                };
                let present_icon = if it.present { "🟢" } else { "🔴" };
                write!(
                    f,
                    "{: <prefix_size$}{tree_icon} {present_icon} {}\n",
                    "", it.item.name
                )
            })
    }
}

impl FromIterator<(usize, ItemTreeNode, bool)> for ItemTree {
    fn from_iter<T: IntoIterator<Item = (usize, ItemTreeNode, bool)>>(iter: T) -> Self {
        let mut iter = iter.into_iter();
        let root_node = iter.next().unwrap().1;
        let mut root = ItemTree::new(root_node.item, root_node.present);

        let mut stack = vec![&mut root as *mut ItemTree];
        for (depth, node, _) in iter {
            stack.truncate(depth);
            let tree = ItemTree::new(node.item, node.present);

            unsafe {
                let parent = stack.last_mut().unwrap();
                let ptr = (**parent)
                    .add_children(std::iter::once(tree))
                    .next()
                    .unwrap() as *mut ItemTree;
                stack.push(ptr);
            }
        }
        root
    }
}

pub(crate) async fn box_contents(
    connection: &mut SqliteConnection,
    r#box: &Item,
) -> anyhow::Result<ItemTree> {
    let mut root = ItemTree::new(r#box.clone(), true);
    let mut open_set = VecDeque::new();
    open_set.push_back(&mut root);
    while let Some(tree) = open_set.pop_front() {
        let children = sqlx::query!(
            "SELECT i.id, i.name, i.strid, m.present
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

pub async fn get_items(
    connection: &mut SqliteConnection,
    user: Option<String>,
) -> anyhow::Result<Vec<ItemTree>> {
    let single_conn = connection.acquire().await?;
    let roots = if let Some(user) = user {
        sqlx::query_as!(
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
            WHERE b.ordering = max_orders.max_ordering AND b.to_user = ? AND (m.parent IS NULL OR m.present IS FALSE);",
            user
        )
        .fetch_all(&mut *single_conn)
        .await?
    } else {
        sqlx::query_as!(
            Item,
            "SELECT id, strid, name FROM items
             LEFT JOIN meta ON meta.child = id WHERE meta.parent IS NULL;"
        )
        .fetch_all(&mut *single_conn)
        .await?
    };

    let mut trees = Vec::new();
    for root in &roots {
        let tree = box_contents(&mut *single_conn, &root).await?;
        trees.push(tree);
    }

    Ok(trees)
}

/// Delete the given item, return a tree containing the previous children which are now orphans
pub async fn delete_item(connection: &mut SqliteConnection, item: &Item) -> anyhow::Result<ItemTree> {
    let tree = box_contents(connection, item).await?;

    sqlx::query!("DELETE FROM items WHERE id = ?;", item.id)
        .execute(&mut *connection)
        .await?;
    Ok(tree)
}
