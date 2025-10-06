{% import "macros.html" as macros %}

var REGISTRY = {};
const MEMBERS = [
    {% for (i, m) in all_members.iter().enumerate() %}
        {% let member_json = filters::json(m.to_js_safe()).unwrap() %}
        {   
            data: {{ member_json | safe }},
            rendered: (idx) => 
            `<li onclick='assignInput(event, {{member_json | safe}}, ` + idx + `)'>{% call macros::show_owner(Some(Owner::Member(MemberOwner::Resolved(m.clone()))))  %}</li>`
        },
    {% endfor %}
];
