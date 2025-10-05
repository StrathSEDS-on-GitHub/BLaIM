{% import "macros.html" as macros %}

const members = [
    {% for (i, m) in all_members.iter().enumerate() %}
        {% let member_json = filters::json(m.to_js_safe()).unwrap() %}
        {   
            data: {{ member_json | safe }},
            rendered:
            `<li onclick='assignInput(event, {{member_json | safe}})'>{% call macros::show_owner(Owner::Member(MemberOwner::Resolved(m.clone())) )  %}</li>`
        },
    {% endfor %}
];

var input;
var idInput;
var avatarImg;

function assignInput(event, member) {
    event.preventDefault();
    input.value = member.nick || member.username;
    idInput.value = member.id;

    if (member.avatar) {
        avatarImg.src = `https://cdn.discordapp.com/avatars/${member.id}/${member.avatar}.png`;
    } else {
        let id = BigInt(member.id);
        let defaultAvatarNumber = Number((id >> BigInt(22)) % BigInt(6));
        avatarImg.src = `https://cdn.discordapp.com/embed/avatars/${defaultAvatarNumber}.png`;
    }
}

function autocomplete(input_, optionList, idInput_, avatarImg_) {
    input = input_;
    idInput = idInput_;
    avatarImg = avatarImg_;

    input.addEventListener("input", function () {
        const value = this.value.toLowerCase();
        idInput.value = "";
        avatarImg.src = ``;

        if (!value) {
            return;
        }

        const filteredOptions = members.filter(member =>
            member.data.nick?.toLowerCase()?.includes(value) ||
            member.data.username.toLowerCase()?.includes(value)
        );

        if (filteredOptions.length < 10) {
            optionList.innerHTML = filteredOptions.map(member => member.rendered).join("");
        }
    });


}