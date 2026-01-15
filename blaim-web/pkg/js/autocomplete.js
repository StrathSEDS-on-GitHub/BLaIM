function assignInput(event, member, idx) {
    event.preventDefault();
    REGISTRY[idx].input.value = member.nick || member.username || member.name;


    if (!REGISTRY[idx].idInput) {
        document.querySelector('form > button').click();
        return;
    }

    REGISTRY[idx].idInput.value = member.id;

    if (member.avatar) {
        REGISTRY[idx].avatarImg.src = `https://cdn.discordapp.com/avatars/${member.id}/${member.avatar}.png`;
    } else {
        let id = BigInt(member.id);
        let defaultAvatarNumber = Number((id >> BigInt(22)) % BigInt(6));
        REGISTRY[idx].avatarImg.src = `https://cdn.discordapp.com/embed/avatars/${defaultAvatarNumber}.png`;
    }
}

function autocomplete(input, optionList, idInput, avatarImg, data, filter, maxOptions = 10) {
    const idx = Object.keys(REGISTRY).length;
    REGISTRY[idx] = { input, optionList, idInput, avatarImg };

    input.addEventListener("input", function () {
        const value = this.value.toLowerCase();
        if (idInput) idInput.value = "";
        if (avatarImg) avatarImg.src = ``;

        if (!value) {
            return;
        }

        const filteredOptions = data.filter(it => filter(it, value));

        if (filteredOptions.length < maxOptions) {
            optionList.innerHTML = filteredOptions.map(member => member.rendered(idx)).join("");
        }
    });
}