use rand::seq::IndexedRandom;
use sqlx::types::time::OffsetDateTime;

pub(crate) mod home;
pub(crate) mod item_status;

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

pub(crate) fn time_since_human_readable(since: &OffsetDateTime) -> String {
    macro_rules! pluralize {
        ($duration:ident, $word:literal) => {
            if $duration == 1 {
                format!("One {} ago", $word)
            } else {
                format!("{} {}s ago", $duration, $word)
            }
        };
    }

    let now = OffsetDateTime::now_utc();
    let duration = now - *since;
    let days = duration.whole_days();
    let hours = duration.whole_hours();
    let minutes = duration.whole_minutes();
    let seconds = duration.whole_seconds();
    if days > 365 {
        let years = days / 365;
        pluralize!(years, "year")
    } else if days > 30 {
        let months = days / 30;
        pluralize!(months, "month")
    } else if days > 7 {
        let weeks = days / 7;
        pluralize!(weeks, "week")
    } else if days > 0 {
        pluralize!(days, "day")
    } else if hours > 0 {
        pluralize!(hours, "hour")
    } else if minutes > 0 {
        pluralize!(minutes, "minute")
    } else {
        pluralize!(seconds, "second")
    }
}

pub(crate) fn random_appearance_idiom() -> &'static str {
    let mut rng = rand::rng();
    APPEARANCE_IDIOMS.choose(&mut rng).copied().unwrap()
}

pub const OAUTH_REDIRECT_URI: &str = {
    #[cfg(not(debug_assertions))]
    {
        "https://blaim.strathseds.org/authorize"
    }

    #[cfg(debug_assertions)]
    {
        "http://localhost:8080/authorize"
    }
};
