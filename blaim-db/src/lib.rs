use std::collections::{HashMap, VecDeque};
use std::fmt::{self, Display};
use std::sync::LazyLock;

use chrono::{DateTime, Utc};
use color_eyre::eyre::Context;
use fuzzy_matcher::FuzzyMatcher;
use fuzzy_matcher::skim::SkimMatcherV2;
use serde_json::Value;
use sqlx::{Acquire, PgConnection, PgPool, Row};

#[derive(Debug, PartialEq, Eq, Clone)]
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

fn fuzzy_search<'b, T: std::fmt::Debug>(
    needle: &'b str,
    haystack: impl Iterator<Item = T> + 'b,
    key: impl Fn(&T) -> &str,
) -> Vec<(i64, T)> {
    static MATCHER: LazyLock<SkimMatcherV2> =
        LazyLock::new(|| SkimMatcherV2::default().smart_case().use_cache(true));
    let mut scores: Vec<_> = haystack
        .filter_map(|s| MATCHER.fuzzy_match(key(&s), needle).map(|score| (score, s)))
        .collect();
    scores.sort_by_key(|(score, _)| -*score);
    scores.splice(scores.len().min(5).., []);
    scores
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

    let results = fuzzy_search(&item, fuzzy_matches, |i| &i.name)
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

    if !response.status().is_success() {}
    let body = response.bytes().await?;
    let json: Value = serde_json::from_slice(&*body)?;
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

pub async fn get_owner_info(
    conn: &mut PgConnection,
    owner_str: &str,
) -> color_eyre::Result<Owner> {
    if owner_str.starts_with("loc:") {
        let location = lookup_storage(&mut *conn, &owner_str[4..])
            .await?
            .into_iter()
            .next()
            .ok_or(color_eyre::eyre::eyre!("No such location"))?;

        Ok(Owner::Location(location))
    } else {
        // Owned by a user
        let snowflake = owner_str.parse::<u64>().wrap_err("Non snowflake in DB")?;
        if let Some(owner) = find_member_info(&mut *conn, snowflake).await?.map(|it| {
            if it.username != "#unresolved" {
                Owner::Member(MemberOwner::Resolved(it))
            } else {
                Owner::Member(MemberOwner::Unresolved(snowflake))
            }
        }) {
            Ok(owner)
        } else {
            if let Ok(member) = get_discord_info(snowflake).await {
                // Cache this info
                insert_member_info(&mut *conn, &member).await?;
                Ok(Owner::Member(MemberOwner::Resolved(member)))
            } else {
                Ok(Owner::Member(MemberOwner::Unresolved(snowflake)))
            }
        }
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

pub async fn borrow_item(
    connection: &mut PgConnection,
    item: &Item,
    user: &str,
) -> color_eyre::Result<(ItemTree, Vec<(Item, Item, bool)>)> {
    let now = sqlx::types::chrono::Utc::now().naive_utc();
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
        let mut builder = sqlx::QueryBuilder::new("UPDATE meta SET present = 1 ");
        builder
            .push("WHERE child = ")
            .push_bind(owned_item)
            .push(" AND (parent IN (");
        let mut sep = builder.separated(",");
        items
            .iter_depth_first()
            .filter(|(_, node, _)| node.present)
            .for_each(|(_, node, _)| {
                sep.push_bind(node.item.id);
            });
        builder.push(")) AND present = 0 RETURNING parent, present;");

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

    let update_tree: ItemTree = items
        .into_iter_depth_first()
        .filter(|(_, node, _)| node.present)
        .collect();
    Ok((update_tree, present_updates))
}

pub async fn borrow_history(
    connection: &mut PgConnection,
    item_id: i32,
) -> color_eyre::Result<Vec<(String, DateTime<Utc>)>> {
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
                DateTime::from_naive_utc_and_offset(row.time, Utc),
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
        if let None = sqlx::query!(
            "DELETE FROM meta WHERE child = $1 AND parent = $2 RETURNING child;",
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
pub struct ItemTreeNode {
    pub item: Item,
    pub present: bool,
}

#[derive(Debug, Clone)]
pub struct ItemTree {
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

    fn find(&self, id: i32) -> Option<&ItemTree> {
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

pub async fn get_all_items(
    conn: &mut PgConnection
) -> color_eyre::Result<Vec<Item>> {
    let items = sqlx::query_as!(Item, "SELECT id, strid, name FROM items;")
        .fetch_all(&mut *conn)
        .await?;
    Ok(items)
}

pub async fn get_all_itemtrees(
    connection: &mut PgConnection,
) -> color_eyre::Result<HashMap<Option<String>, Vec<ItemTree>>> {
    let single_conn = connection.acquire().await?;
    let roots: Vec<(Option<String>, i32, String, String)> = sqlx::query_as(
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
    .fetch_all(&mut *single_conn)
    .await?;
    let mut trees = HashMap::new();
    for (owner, id, strid, name) in roots {
        let tree = box_contents(&mut *single_conn, &Item { id, strid, name }).await?;
        trees.entry(owner).or_insert(Vec::new()).push(tree);
    }

    Ok(trees)
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

#[derive(Debug, Clone)]
pub struct Member {
    pub id: u64,
    pub nick: Option<String>,
    pub username: String,
    pub avatar: Option<String>,
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
    Ethereal,
}

pub async fn find_member_info(
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

pub async fn insert_member_info(
    conn: &mut PgConnection,
    member: &Member,
) -> color_eyre::Result<()> {
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
