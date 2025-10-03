use blaim_db::Member;
use serde::{Deserialize, Serialize};
use time::OffsetDateTime;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum BlaimSession {
    Challenged {
        at: OffsetDateTime,
        state: String,
        redirect: String,
    },
    Authenticated {
        member: Member,
    },
}
