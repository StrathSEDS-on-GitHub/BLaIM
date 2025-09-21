use askama::Template;
use blaim_db::{Item, Owner, MemberOwner};
use sqlx::types::chrono::{DateTime, Utc};


#[derive(Template)]
#[template(path = "item_status.html")]
pub struct ItemStatusTemplate {
    pub item: Item,
    pub owner: Owner,
    pub borrow_history: Vec<(Owner, DateTime<Utc>)>
}
