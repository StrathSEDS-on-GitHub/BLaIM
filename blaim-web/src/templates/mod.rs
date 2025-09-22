use rand::seq::IndexedRandom;
use sqlx::types::chrono::{DateTime, Utc};

pub(crate) mod item_status;
pub(crate) mod home;

const APPEARANCE_IDIOMS: &[&str] = &[
    "Spontaneously coalesces from the quantum foam",
    "Materializes via supply-chain tunneling",
    "Detected as a faint blip on the cosmic event horizon",
    "Appears as though teleported by an inattentive fresher",
    "Observed collapsing the probability wave of “we might have one.”",
    "Transitions from hypothetical asset to measurable deliverable",
    "Undergoes catalog speciation",
    "Transitions from dark to baryonic matter",
    "Crosses the threshold of theoretical availability into operational reality",
];

pub(crate) fn time_since_human_readable(since: &DateTime<Utc>) -> String {
    macro_rules! pluralize {
        ($duration:ident, $word:literal) => {
            if $duration == 1 {
                format!("One {} ago", $word)
            } else {
                format!("{} {}s ago", $duration, $word)
            }
        };
    }

    let now = Utc::now();
    let duration = now.signed_duration_since(since);
    if duration.num_days() > 365 {
        let years = duration.num_days() / 365;
        pluralize!(years, "year")
    } else if duration.num_days() > 30 {
        let months = duration.num_days() / 30;
        pluralize!(months, "month")
    } else if duration.num_days() > 7 {
        let weeks = duration.num_days() / 7;
        pluralize!(weeks, "week")
    } else if duration.num_days() > 0 {
        let days = duration.num_days();
        pluralize!(days, "day")
    } else if duration.num_hours() > 0 {
        let hours = duration.num_hours();
        pluralize!(hours, "hour")
    } else if duration.num_minutes() > 0 {
        let minutes = duration.num_minutes();
        pluralize!(minutes, "minute")
    } else {
        let seconds = duration.num_seconds();
        pluralize!(seconds, "second")
    }
}

pub(crate) fn random_appearance_idiom() -> &'static str {
    let mut rng = rand::rng();
    APPEARANCE_IDIOMS.choose(&mut rng).copied().unwrap()
}
