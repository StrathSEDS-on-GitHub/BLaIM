use askama::Template;
use blaim_db::{BorrowUpdates, Item, ItemTree, MemberOwner, Owner};
use sqlx::types::time::OffsetDateTime;

use crate::session::BlaimSession;
use crate::templates::{ItemTreeTemplate, closure};
use time::format_description::well_known::Rfc2822;

#[derive(Template)]
#[template(path = "item_status.html")]
pub struct ItemStatusTemplate {
    pub session: BlaimSession,

    pub item: Item,
    pub box_contents: ItemTree,
    pub owner: Option<Owner>,
    pub borrow_history: Vec<(Owner, OffsetDateTime)>,
    pub borrow_updates: Option<BorrowUpdates>,
}
