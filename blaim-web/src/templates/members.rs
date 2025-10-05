use askama::Template;
use blaim_db::{Member, Owner, MemberOwner};
use askama::filters;

#[derive(Template)]
#[template(path = "autocomplete.js", escape = "none")]
pub struct MembersTemplate {
    pub all_members: Vec<Member>,
}