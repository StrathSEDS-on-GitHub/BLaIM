use askama::Template;
use blaim_db::{
    ItemTree, ItemTreeImpl, ItemTreeNode, ItemTreeNodeImpl, ItemTreeOwned,
    MemberOwner, Owner,
};
use rand::seq::IndexedRandom;
use sqlx::types::time::OffsetDateTime;

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

#[macro_export]
macro_rules! closure {
    ($args:pat, $body:expr) => {
        |$args| $body
    };
}

#[derive(Template)]
#[template(
    source = r#"
{% import "macros.html" as macros %}
{% if children.len() > 0 %}
   <item-name> 
        <a href="/item/{{ item.item.name }}/{{ item.item.id }}">
            <i class="fa fa-archive" aria-hidden="true"></i>
            {% if item.present %}
                {{ item.item.name }}
            {% else %}
                <span class="not-within-box">{{ item.item.name }} </span>
            {% endif %}
        </a> 
    </item-name>
<ul>
    {% for item in children %}
            <li> {{ item.render()? }} </li>
    {% endfor %}
</ul>
{% else %}
   <item-name> 
        <a href="/item/{{ item.item.name }}/{{ item.item.id }}">
            {% if item.present %}
                <i class="fa fa-check-circle" title="Within box" aria-hidden="true"></i>
                {{ item.item.name }} 
            {% else %}
                <i class="fa fa-circle-o" title="Not within box" aria-hidden="true"></i>
                <span class="not-within-box">{{ item.item.name }} </span>
            {% endif %}
        </a> 
    </item-name>
{% endif %}
"#,
    ext = "html",
    escape = ""
)]
pub struct ItemTreeTemplate {
    pub item: ItemTreeNode,
    pub children: Vec<ItemTreeTemplate>,
}

impl From<&ItemTree> for ItemTreeTemplate {
    fn from(value: &ItemTree) -> Self {
        ItemTreeTemplate {
            item: value.item.clone(),
            children: value.children.iter().map(Into::into).collect(),
        }
    }
}

#[derive(Template)]
#[template(
    source = r#"
{% import "macros.html" as macros %}
{% if tree.children.len() > 0 %}
   <item-name data-tree-depth="{{ tree.item.associated.1 }}"> 
        <a href="/item/{{ tree.item.item.name }}/{{ tree.item.item.id }}">
            <i class="fa fa-archive" aria-hidden="true"></i>
            {% if tree.item.present %}
                {{ tree.item.item.name }}
            {% else %}
                <span class="not-within-box">{{ tree.item.item.name }} </span>
            {% endif %}
        </a> 
    </item-name>
    {% call macros::show_owner(tree.item.associated.0) %}
<ul>
    {% for child in tree.children %}
            <li> {{ ItemTreeOwnedTemplate::new(child).render()? }} </li>
    {% endfor %}
</ul>
{% else %}
   <item-name data-tree-depth="{{ tree.item.associated.1 }}"> 
        <a href="/item/{{ tree.item.item.name }}/{{ tree.item.item.id }}">
            {% if tree.item.present %}
                <i class="fa fa-check-circle" title="Within box" aria-hidden="true"></i>
                {{ tree.item.item.name }} 
            {% else %}
                <i class="fa fa-circle-o" title="Not within box" aria-hidden="true"></i>
                <span class="not-within-box">{{ tree.item.item.name }} </span>
            {% endif %}
        </a> 
    </item-name>
    {% call macros::show_owner(tree.item.associated.0) %}
{% endif %}
"#,
    ext = "html",
    escape = ""
)]
pub struct ItemTreeOwnedTemplate {
    pub tree: ItemTreeImpl<(Option<Owner>, usize)>,
}

impl ItemTreeOwnedTemplate {
    pub fn new(tree: &ItemTreeImpl<(Option<Owner>, usize)>) -> Self {
        Self { tree: tree.clone() }
    }
}

impl From<&ItemTreeOwned> for ItemTreeOwnedTemplate {
    fn from(value: &ItemTreeOwned) -> Self {
        let iter = value
            .clone()
            .into_iter_depth_first()
            .map(|(depth, node, last)| {
                (
                    depth,
                    ItemTreeNodeImpl {
                        item: node.item,
                        present: node.present,
                        associated: (node.associated, depth),
                    },
                    last,
                )
            });

        ItemTreeOwnedTemplate {
            tree: ItemTreeImpl::from_iter(iter),
        }
    }
}
