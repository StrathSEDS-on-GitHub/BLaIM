use askama::Template;
use blaim_db::Item;

#[derive(Template)]
#[template(path = "index.html")]
pub struct Home {
    pub items: Vec<Item>,
}
