use askama::Template;
use blaim_db::{Item, MemberOwner, Owner};
use sqlx::types::time::OffsetDateTime;

use crate::session::BlaimSession;
use time::format_description::well_known::Rfc2822;

#[derive(Template)]
#[template(path = "item_status.html")]
pub struct ItemStatusTemplate {
    pub session: BlaimSession,

    pub item: Item,
    pub owner: Owner,
    pub borrow_history: Vec<(Owner, OffsetDateTime)>,
}
