{% import "macros.html" as macros %}

var REGISTRY = {};
const ITEMS = [
    {% for item in all_items.iter() %}
        {% let item_json = filters::json(item).unwrap() %}
        {   
            data: {{ item_json | safe }},
            rendered: (idx) => 
            `<li onclick='assignInput(event, {{item_json | safe}}, ` + idx + `)'>{{ item.name }}</li>`
        },
    {% endfor %}
];
