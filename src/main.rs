#![feature(iter_partition_in_place)]
#![feature(iter_intersperse)]
#![feature(let_chains)]
#![feature(closure_lifetime_binder)]

use std::env;
use std::future::Future;
use std::pin::Pin;

use db::ItemTree;
use poise::{CreateReply, ReplyHandle};
use serenity::all::{
    ButtonStyle, ClientBuilder, ComponentInteractionCollector, CreateActionRow, CreateButton,
    CreateEmbed, CreateEmbedFooter, CreateInteractionResponse, CreateSelectMenu,
    CreateSelectMenuKind, CreateSelectMenuOption, EditMessage, GuildId, User,
};
use serenity::futures::future::try_join_all;
use serenity::prelude::*;
use sqlx::sqlite::SqlitePoolOptions;
use sqlx::{Acquire, SqliteConnection, SqlitePool};

mod db;

struct Data {
    pool: SqlitePool,
}

type Error = anyhow::Error;
type Context<'a> = poise::Context<'a, Data, Error>;

const ALLOWED_GUILDS: &[u64] = &[755426438185877614, 366211396511334420];

async fn autocomplete_item<'a>(
    ctx: Context<'_>,
    partial: &'a str,
) -> Box<dyn Iterator<Item = String> + Send + 'a> {
    let conn = &mut ctx.data().pool.acquire().await;
    if let Ok(conn) = conn {
        let conn = conn as &mut SqliteConnection;
        Box::new(
            db::lookup_item(conn, partial)
                .await
                .unwrap_or_default()
                .into_iter()
                .map(|item| item.name),
        )
    } else {
        Box::new([].into_iter())
    }
}

async fn format_owner(connection: &mut SqliteConnection, owner: &str) -> String {
    if let Some(owner) = owner.strip_prefix("loc:") {
        format!(
            "{}",
            db::find_location_name(connection, owner)
                .await
                .unwrap()
                .unwrap()
                .name
        )
    } else {
        format!("<@{}>", owner)
    }
}

async fn autocomplete_store<'a>(
    ctx: Context<'_>,
    partial: &'a str,
) -> Box<dyn Iterator<Item = String> + Send + 'a> {
    let conn = &mut ctx.data().pool.acquire().await;
    if let Ok(conn) = conn {
        let conn = conn as &mut SqliteConnection;
        Box::new(
            db::lookup_storage(conn, partial)
                .await
                .unwrap_or_default()
                .into_iter()
                .map(|item| item.name),
        )
    } else {
        Box::new([].into_iter())
    }
}

async fn make_embed(
    transaction: &mut SqliteConnection,
    selected: &db::Item,
    from: &Option<String>,
    to: &str,
    alternatives: &[db::Item],
    (updated_items, present_updates): &(ItemTree, Vec<(db::Item, db::Item, bool)>),
) -> (CreateEmbed, Vec<CreateActionRow>) {
    let previous_owner = if let Some(ref owner) = from {
        format_owner(transaction, owner).await
    } else {
        "👻 No one".to_string()
    };

    let new_owner = format_owner(&mut *transaction, to).await;

    let mut embed = CreateEmbed::new()
        .title(format!(
            ":white_check_mark: Borrowed item {}",
            selected.name
        ))
        .description(format!(
            ":red_square: **-** {} \n:arrow_down:\n:green_square: **+** {}",
            previous_owner, new_owner
        ))
        .footer(CreateEmbedFooter::new(chrono::Utc::now().to_rfc2822()));

    let boxed_items_count = updated_items.iter_depth_first().collect::<Vec<_>>().len() - 1;
    if boxed_items_count > 0 {
        embed = embed.field(
            "Boxed items",
            format!(
                "Transfered **{}** boxed items \n```{}```",
                boxed_items_count, updated_items
            ),
            false,
        );
    }

    if present_updates.len() > 0 {
        let updates = format!("**{}** items updated\n```", present_updates.len())
            + &present_updates
                .iter()
                .map(|(parent, item, present)| {
                    let emoji = if *present { "🟢" } else { "🔴" };
                    format!(" 🗃️ {} {} {}", parent.name, emoji, item.name)
                })
                .collect::<Vec<_>>()
                .join("\n")
            + "```";

        embed = embed
            .field("Box contents updates", updates, false)
            .footer(CreateEmbedFooter::new(
                "🟢 = added to box. 🔴 = removed from box.",
            ));
    }

    let mut components = vec![CreateActionRow::Buttons(vec![
        CreateButton::new("cancel")
            .label("Cancel")
            .style(ButtonStyle::Danger),
        CreateButton::new("commit")
            .label("Confirm")
            .style(ButtonStyle::Primary),
    ])];

    if alternatives.len() > 0 {
        components.insert(
            0,
            CreateActionRow::SelectMenu(
                CreateSelectMenu::new(
                    "replace",
                    CreateSelectMenuKind::String {
                        options: alternatives
                            .into_iter()
                            .map(|item| {
                                CreateSelectMenuOption::new(item.name.clone(), item.id.to_string())
                            })
                            .collect(),
                    },
                )
                .placeholder("🔁 Pick a different item"),
            ),
        );
    }

    (embed, components)
}

async fn handle_edits<T>(
    ctx: Context<'_>,
    state: T,
    handle: &ReplyHandle<'_>,
    recreate: impl for<'a> Fn(
        &'a T,
        &'a mut SqliteConnection,
        i64,
    ) -> Pin<
        Box<dyn Future<Output = anyhow::Result<(CreateEmbed, Vec<CreateActionRow>)>> + Send + 'a>,
    >,
    mut embed: CreateEmbed,
    connection: &mut SqliteConnection,
    mut transaction: sqlx::Transaction<'_, sqlx::Sqlite>,
) -> anyhow::Result<()> {
    let message_id = handle.message().await?.id;
    let mut deleted = false;

    while let Some(ref mut mci) = ComponentInteractionCollector::new(ctx)
        .author_id(ctx.author().id)
        .timeout(std::time::Duration::from_secs(15))
        .channel_id(ctx.channel_id())
        .message_id(message_id)
        .filter(|i| {
            i.data.custom_id == "cancel"
                || i.data.custom_id == "replace"
                || i.data.custom_id == "commit"
        })
        .await
    {
        match mci.data.custom_id.as_str() {
            "cancel" => {
                mci.message.delete(&ctx).await?;
                mci.create_response(ctx, CreateInteractionResponse::Acknowledge)
                    .await?;
                deleted = true;
                break;
            }
            "replace" => {
                let item_id = match &mci.data.kind {
                    serenity::all::ComponentInteractionDataKind::StringSelect { values } => values
                        .first()
                        .expect("No value")
                        .parse()
                        .expect("Invalid value"),
                    _ => unreachable!(),
                };
                transaction.rollback().await?;
                transaction = connection.begin().await?;

                let (embed_, components) = recreate(&state, &mut *transaction, item_id).await?;
                embed = embed_;

                mci.message
                    .edit(
                        ctx,
                        EditMessage::default()
                            .embed(embed.clone())
                            .components(components),
                    )
                    .await?;
                mci.create_response(ctx, CreateInteractionResponse::Acknowledge)
                    .await?;
            }
            "commit" => {
                mci.create_response(ctx, CreateInteractionResponse::Acknowledge)
                    .await?;
                break;
            }
            _ => unreachable!(),
        }
    }

    if !deleted {
        handle
            .edit(
                ctx,
                CreateReply::default().embed(embed).components(Vec::new()),
            )
            .await?;
        transaction.commit().await?;
    } else {
        transaction.rollback().await?;
    }

    Ok(())
}

/// Borrows ownership of an item
#[poise::command(slash_command)]
async fn borrow(
    ctx: Context<'_>,
    #[description = "Item"]
    #[autocomplete = autocomplete_item]
    item: String,
) -> Result<(), Error> {
    if ctx.guild_id().is_none() || !ALLOWED_GUILDS.contains(&ctx.guild_id().unwrap().get()) {
        let embed = CreateEmbed::new()
            .title("Not allowed")
            .description("You are not allowed to use this command in this server.");

        ctx.send(CreateReply::default().embed(embed)).await?;
        return Ok(());
    }
    let mut connection = ctx.data().pool.acquire().await?;
    let mut transaction = connection.begin().await?;
    let items = db::lookup_item(&mut *transaction, &item).await?;

    let selected = match items.first() {
        Some(selected) => selected,
        None => {
            let embed = CreateEmbed::new()
                .title("No items found")
                .description("No items found with that name. Please try again.");

            let reply = CreateReply::default().embed(embed);

            ctx.send(reply).await?;
            return Ok(());
        }
    };

    let previous_owner = db::get_last_holder(&mut *transaction, selected.id).await?;

    if let Some(ref owner) = previous_owner
        && owner == &ctx.author().id.to_string()
    {
        let embed = CreateEmbed::new()
            .title("You already own this item")
            .image("https://i.kym-cdn.com/entries/icons/original/000/023/397/C-658VsXoAo3ovC.jpg");

        let reply = CreateReply::default().embed(embed);
        ctx.send(reply).await?;
        return Ok(());
    }

    struct State {
        previous_owner: Option<String>,
        author_id: String,
        items: Vec<db::Item>,
    }

    let task = for<'a> |state: &'a State,
                        connection: &'a mut SqliteConnection,
                        item_id: i64|
             -> Pin<
        Box<dyn Future<Output = anyhow::Result<(CreateEmbed, Vec<CreateActionRow>)>> + Send + 'a>,
    > {
        Box::pin(async move {
            let selected = state.items.iter().find(|it| it.id == item_id).unwrap();
            let alternatives = state
                .items
                .iter()
                .filter(|it| it.id != item_id)
                .cloned()
                .collect::<Vec<_>>();
            let results = db::borrow_item(&mut *connection, selected, &state.author_id).await?;
            let (embed, components) = make_embed(
                connection,
                &selected,
                &state.previous_owner,
                &state.author_id,
                &alternatives,
                &results,
            )
            .await;
            Ok((embed, components))
        })
    };

    let selected_id = selected.id;
    let state = State {
        previous_owner,
        author_id: ctx.author().id.to_string(),
        items,
    };
    let (embed, components) = task(&state, &mut *transaction, selected_id).await?;

    let reply = CreateReply::default()
        .embed(embed.clone())
        .components(components);

    let handle = ctx.send(reply).await?;

    let mut redo_conn = ctx.data().pool.acquire().await?;

    handle_edits(
        ctx,
        state,
        &handle,
        task,
        embed,
        &mut redo_conn,
        transaction,
    )
    .await?;

    Ok(())
}

/// List items held by a user or all items
#[poise::command(slash_command)]
async fn items(ctx: Context<'_>, user: Option<User>) -> Result<(), Error> {
    if ctx.guild_id().is_none() || !ALLOWED_GUILDS.contains(&ctx.guild_id().unwrap().get()) {
        let embed = CreateEmbed::new()
            .title("Not allowed")
            .description("You are not allowed to use this command in this server.");

        ctx.send(CreateReply::default().embed(embed)).await?;
        return Ok(());
    }
    let mut connection = ctx.data().pool.acquire().await?;
    let embed = if let Some(ref user) = user {
        let items = db::get_itemtrees(&mut *connection, user.id.to_string()).await?;
        let message = "```".to_string()
            + &items
                .into_iter()
                .map(|it| {
                    it.into_iter_depth_first()
                        .filter(|(_, item, _)| item.present) // Include not present items if user is not specified
                        .collect::<ItemTree>()
                        .to_string()
                })
                .collect::<String>()
            + "```";
        CreateEmbed::new()
            .title(format!("📦 Items held by {}", user.name))
            .description(message)
    } else {
        let items = db::get_all_itemtrees(&mut *connection).await?;
        let description = try_join_all(items.iter().map(async |(owner, trees)| {
            Ok::<_, anyhow::Error>(if trees.is_empty() {
                "Nothing to see here".to_string()
            } else {
                format!(
                    "\n### 📦 **{}**\n```\n{}```",
                    if let Some(owner) = owner {
                        format_owner(&mut *ctx.data().pool.acquire().await?, owner).await
                    } else {
                        "No one :ghost:".to_string()
                    },
                    trees
                        .iter()
                        .map(|it| it.to_string())
                        .fold(String::new(), |acc, it| acc + &it)
                )
            })
        }))
        .await?;
        CreateEmbed::new()
            .title(":wrench: Items")
            .description(description.concat())
    };

    ctx.send(CreateReply::default().embed(embed)).await?;

    Ok(())
}

/// Shows the borrow history of an item
#[poise::command(slash_command)]
async fn blame(
    ctx: Context<'_>,
    #[description = "Item"]
    #[autocomplete = autocomplete_item]
    item: String,
) -> Result<(), Error> {
    if ctx.guild_id().is_none() || !ALLOWED_GUILDS.contains(&ctx.guild_id().unwrap().get()) {
        let embed = CreateEmbed::new()
            .title("Not allowed")
            .description("You are not allowed to use this command in this server.");

        ctx.send(CreateReply::default().embed(embed)).await?;
        return Ok(());
    }
    let mut connection = ctx.data().pool.acquire().await?;
    let item = db::lookup_item(&mut *connection, &item).await?;
    let item = match item.first() {
        Some(item) => item,
        None => {
            let embed = CreateEmbed::new()
                .title("No items found")
                .description("No items found with that name. Please try again.");

            let reply = CreateReply::default().embed(embed);

            ctx.send(reply).await?;
            return Ok(());
        }
    };

    let history = db::borrow_history(&mut *connection, item.id).await?;
    if history.is_empty() {
        let embed = CreateEmbed::new()
            .title("No history found")
            .description("No history found for this item.");

        let reply = CreateReply::default().embed(embed);

        ctx.send(reply).await?;
        return Ok(());
    }

    let mut borrowers = Vec::new();
    for (borrower, _) in &history {
        borrowers.push(format_owner(&mut *connection, borrower).await + "\n")
    }
    let borrowers = borrowers.into_iter();

    let parent_box = db::parent_box(&mut *connection, item).await?;

    let mut embed = CreateEmbed::new()
        .title(format!("Borrow history for {}", item.name))
        .field(
            ":green_square: Current holder",
            format!(
                "{} since <t:{}:R>",
                format_owner(&mut *connection, &history.first().unwrap().0).await,
                history.first().unwrap().1.timestamp()
            ),
            true,
        )
        .field("🆔 Identifier", &item.strid, true)
        .field(
            "📦 Parent box",
            parent_box.map_or("Not in a box".to_owned(), |it| it.name),
            true,
        );

    if borrowers.len() > 1 {
        let from_column: String = borrowers
            .clone()
            .skip(1)
            .chain(std::iter::once("👻".to_string()))
            .collect();
        println!("from_col: {:?}", from_column.clone());
        let to_column: String = borrowers
            .clone()
            .map(|x| format!(":right_arrow:{}{}", "\x7f ".repeat(6), x))
            .collect();
        let time_column = history
            .iter()
            .map(|(_, time)| format!("<t:{}:R>\n", time.timestamp()))
            .collect::<String>();
        embed = embed
            .field("From", from_column, true)
            .field(format!("{}To", "\x7f ".repeat(12)), to_column, true)
            .field("Time", time_column, true);
    }

    ctx.send(CreateReply::default().embed(embed)).await?;

    Ok(())
}

/// Show the contents of storage locations
#[poise::command(slash_command)]
async fn storage(
    ctx: Context<'_>,
    #[autocomplete = autocomplete_store] location: Option<String>,
) -> Result<(), Error> {
    if ctx.guild_id().is_none() || !ALLOWED_GUILDS.contains(&ctx.guild_id().unwrap().get()) {
        let embed = CreateEmbed::new()
            .title("Not allowed")
            .description("You are not allowed to use this command in this server.");

        ctx.send(CreateReply::default().embed(embed)).await?;
        return Ok(());
    }

    let location = if let Some(location) = location {
        let Some(location) = db::lookup_storage(&mut *ctx.data().pool.acquire().await?, &location)
            .await
            .unwrap_or_default()
            .into_iter()
            .map(|item| item.strid)
            .next()
        else {
            let embed = CreateEmbed::new()
                .title("No storage found")
                .description("No storage found with that name. Please try again.");
            let reply = CreateReply::default().embed(embed);
            ctx.send(reply).await?;
            return Ok(());
        };
        Some(location)
    } else {
        None
    };

    let items = db::list_storage(&ctx.data().pool, location.clone()).await?;

    let embed = CreateEmbed::new()
        .title("📦 Storage contents")
        .fields(items.iter().map(|(location, trees)| {
            (
                format!("📍 **{}**\n", location),
                if trees.is_empty() {
                    "Nothing to see here".to_string()
                } else {
                    format!(
                        "```\n{}```",
                        trees
                            .iter()
                            .map(|it| it.to_string())
                            .fold(String::new(), |acc, it| acc + &it + "\n")
                    )
                },
                false,
            )
        }));

    ctx.send(CreateReply::default().embed(embed)).await?;

    Ok(())
}

///Move an item into a storage location
#[poise::command(slash_command)]
async fn store(
    ctx: Context<'_>,
    #[description = "Item"]
    #[autocomplete = autocomplete_item]
    item: String,
    #[autocomplete = autocomplete_store] location: Option<String>,
) -> Result<(), Error> {
    if ctx.guild_id().is_none() || !ALLOWED_GUILDS.contains(&ctx.guild_id().unwrap().get()) {
        let embed = CreateEmbed::new()
            .title("Not allowed")
            .description("You are not allowed to use this command in this server.");

        ctx.send(CreateReply::default().embed(embed)).await?;
        return Ok(());
    }
    let mut connection = ctx.data().pool.acquire().await?;
    let mut transaction = connection.begin().await?;
    let items = db::lookup_item(&mut *transaction, &item).await?;

    let location = location.unwrap_or("jw9".to_string());

    let selected = match items.first() {
        Some(selected) => selected,
        None => {
            let embed = CreateEmbed::new()
                .title("No items found")
                .description("No items found with that name. Please try again.");

            let reply = CreateReply::default().embed(embed);

            ctx.send(reply).await?;
            return Ok(());
        }
    };

    let Some(location) = db::lookup_storage(&mut *transaction, &location)
        .await?
        .into_iter()
        .next()
    else {
        let embed = CreateEmbed::new()
            .title("No storage found")
            .description("No storage found with that name. Please try again.");

        let reply = CreateReply::default().embed(embed);

        ctx.send(reply).await?;
        return Ok(());
    };

    let owner = db::get_last_holder(&mut *transaction, selected.id).await?;

    if let Some(ref owner) = owner
        && owner != &ctx.author().id.to_string()
    {
        let embed = CreateEmbed::new()
            .title("You don't own this item")
            .description(format!("This item is currently owned by <@{}>", owner));

        let reply = CreateReply::default().embed(embed);
        ctx.send(reply).await?;
        return Ok(());
    }

    if let Some(ref owner) = owner
        && owner.strip_prefix("loc:").unwrap_or(owner) == &location.strid
    {
        let embed = CreateEmbed::new()
            .title(format!("This item is already in {}", location.name))
            .image("https://i.kym-cdn.com/entries/icons/original/000/023/397/C-658VsXoAo3ovC.jpg");

        let reply = CreateReply::default().embed(embed);
        ctx.send(reply).await?;
        return Ok(());
    }

    struct State {
        previous_owner: Option<String>,
        items: Vec<db::Item>,
        location: String,
    }

    let task = for<'a> |state: &'a State,
                        connection: &'a mut SqliteConnection,
                        item_id: i64|
             -> Pin<
        Box<dyn Future<Output = anyhow::Result<(CreateEmbed, Vec<CreateActionRow>)>> + Send + 'a>,
    > {
        Box::pin(async move {
            let selected = state.items.iter().find(|it| it.id == item_id).unwrap();
            let alternatives = state
                .items
                .iter()
                .filter(|it| it.id != item_id)
                .cloned()
                .collect::<Vec<_>>();
            let results = db::borrow_item(&mut *connection, selected, &state.location).await?;
            let (embed, components) = make_embed(
                &mut *connection,
                &selected,
                &state.previous_owner,
                &state.location,
                &alternatives,
                &results,
            )
            .await;
            Ok((embed, components))
        })
    };

    let state = State {
        previous_owner: owner,
        items: items.clone(),
        location: format!("loc:{}", location.strid),
    };

    let (embed, components) = task(&state, &mut *transaction, selected.id).await?;
    let reply = CreateReply::default()
        .embed(embed.clone())
        .components(components);

    let handle = ctx.send(reply).await?;

    let mut redo_conn = ctx.data().pool.acquire().await?;

    handle_edits(
        ctx,
        state,
        &handle,
        task,
        embed,
        &mut redo_conn,
        transaction,
    )
    .await?;

    Ok(())
}
/// Gives ownership of an item to another user
#[poise::command(slash_command)]
async fn give(
    ctx: Context<'_>,
    user: User,
    #[description = "Item"]
    #[autocomplete = autocomplete_item]
    item: String,
) -> Result<(), Error> {
    if ctx.guild_id().is_none() || !ALLOWED_GUILDS.contains(&ctx.guild_id().unwrap().get()) {
        let embed = CreateEmbed::new()
            .title("Not allowed")
            .description("You are not allowed to use this command in this server.");

        ctx.send(CreateReply::default().embed(embed)).await?;
        return Ok(());
    }
    let mut connection = ctx.data().pool.acquire().await?;
    let mut transaction = connection.begin().await?;
    let items = db::lookup_item(&mut *transaction, &item).await?;

    let selected = match items.first() {
        Some(selected) => selected,
        None => {
            let embed = CreateEmbed::new()
                .title("No items found")
                .description("No items found with that name. Please try again.");

            let reply = CreateReply::default().embed(embed);

            ctx.send(reply).await?;
            return Ok(());
        }
    };

    let owner = db::get_last_holder(&mut *transaction, selected.id).await?;

    if let Some(ref owner) = owner
        && owner != &ctx.author().id.to_string()
    {
        let embed = CreateEmbed::new()
            .title("You don't own this item")
            .description(format!("This item is currently owned by <@{}>", owner));

        let reply = CreateReply::default().embed(embed);
        ctx.send(reply).await?;
        return Ok(());
    }

    if let Some(ref owner) = owner
        && owner == &user.id.to_string()
    {
        let embed = CreateEmbed::new()
            .title("You already own this item")
            .image("https://i.kym-cdn.com/entries/icons/original/000/023/397/C-658VsXoAo3ovC.jpg");

        let reply = CreateReply::default().embed(embed);
        ctx.send(reply).await?;
        return Ok(());
    }

    struct State {
        previous_owner: Option<String>,
        items: Vec<db::Item>,
        user_id: String,
    }

    let task = for<'a> |state: &'a State,
                        connection: &'a mut SqliteConnection,
                        item_id: i64|
             -> Pin<
        Box<dyn Future<Output = anyhow::Result<(CreateEmbed, Vec<CreateActionRow>)>> + Send + 'a>,
    > {
        Box::pin(async move {
            let selected = state.items.iter().find(|it| it.id == item_id).unwrap();
            let alternatives = state
                .items
                .iter()
                .filter(|it| it.id != item_id)
                .cloned()
                .collect::<Vec<_>>();
            let results = db::borrow_item(&mut *connection, selected, &state.user_id).await?;
            let (embed, components) = make_embed(
                &mut *connection,
                &selected,
                &state.previous_owner,
                &state.user_id,
                &alternatives,
                &results,
            )
            .await;
            Ok((embed, components))
        })
    };

    let state = State {
        previous_owner: owner,
        items: items.clone(),
        user_id: user.id.to_string(),
    };
    let (embed, components) = task(&state, &mut *transaction, selected.id).await?;
    let reply = CreateReply::default()
        .embed(embed.clone())
        .components(components);

    let handle = ctx.send(reply).await?;

    let mut redo_conn = ctx.data().pool.acquire().await?;

    handle_edits(
        ctx,
        state,
        &handle,
        task,
        embed,
        &mut redo_conn,
        transaction,
    )
    .await?;

    Ok(())
}

#[poise::command(slash_command, subcommands("item_register", "item_delete"))]
async fn item(_ctx: Context<'_>) -> anyhow::Result<()> {
    Ok(())
}

/// Register an item in the database
#[poise::command(slash_command, rename = "register")]
async fn item_register(
    ctx: Context<'_>,
    #[description = "Short ID (e.g ffmini2)"] strid: String,
    #[description = "Long descriptor of the item"] name: String,
) -> anyhow::Result<()> {
    if ctx.guild_id().is_none() || !ALLOWED_GUILDS.contains(&ctx.guild_id().unwrap().get()) {
        let embed = CreateEmbed::new()
            .title("Not allowed")
            .description("You are not allowed to use this command in this server.");

        ctx.send(CreateReply::default().embed(embed)).await?;
        return Ok(());
    }
    db::register_item(&mut *ctx.data().pool.acquire().await?, &strid, &name).await?;
    ctx.reply(":white_check_mark: Item registered").await?;

    Ok(())
}

/// Delete an item from the database
#[poise::command(slash_command, rename = "delete")]
async fn item_delete(
    ctx: Context<'_>,
    #[description = "Item"]
    #[autocomplete = autocomplete_item]
    item: String,
) -> anyhow::Result<()> {
    if ctx.guild_id().is_none() || !ALLOWED_GUILDS.contains(&ctx.guild_id().unwrap().get()) {
        let embed = CreateEmbed::new()
            .title("Not allowed")
            .description("You are not allowed to use this command in this server.");

        ctx.send(CreateReply::default().embed(embed)).await?;
        return Ok(());
    }
    let mut connection = ctx.data().pool.acquire().await?;
    let mut transaction = connection.begin().await?;
    let items = db::lookup_item(&mut *transaction, &item).await?;

    let selected = match items.first() {
        Some(selected) => selected,
        None => {
            let embed = CreateEmbed::new()
                .title("No items found")
                .description("No items found with that name. Please try again.");

            let reply = CreateReply::default().embed(embed);

            ctx.send(reply).await?;
            return Ok(());
        }
    };

    let task = for<'a> |state: &'a State,
                        connection: &'a mut SqliteConnection,
                        item_id: i64|
             -> Pin<
        Box<dyn Future<Output = anyhow::Result<(CreateEmbed, Vec<CreateActionRow>)>> + Send + 'a>,
    > {
        Box::pin(async move {
            let selected = state.items.iter().find(|it| it.id == item_id).unwrap();
            let alternatives = state
                .items
                .iter()
                .filter(|it| it.id != item_id)
                .cloned()
                .collect::<Vec<_>>();
            let orphans = db::delete_item(&mut *connection, selected).await?;
            let mut components = vec![CreateActionRow::Buttons(vec![
                CreateButton::new("cancel")
                    .label("Cancel")
                    .style(ButtonStyle::Danger),
                CreateButton::new("commit")
                    .label("Confirm")
                    .style(ButtonStyle::Primary),
            ])];

            if alternatives.len() > 0 {
                components.insert(
                    0,
                    CreateActionRow::SelectMenu(
                        CreateSelectMenu::new(
                            "replace",
                            CreateSelectMenuKind::String {
                                options: alternatives
                                    .into_iter()
                                    .map(|item| {
                                        CreateSelectMenuOption::new(
                                            item.name.clone(),
                                            item.id.to_string(),
                                        )
                                    })
                                    .collect(),
                            },
                        )
                        .placeholder("🔁 Pick a different item"),
                    ),
                );
            }

            let mut desc = format!("Item {} has been deleted.", selected.name);

            let orphan_count = orphans.iter_depth_first().count() - 1;
            if orphan_count > 0 {
                desc.push_str(&format!(
                    " **{}** items have been orphaned.\n```\n{}\n```",
                    orphan_count, orphans
                ));
            }

            let embed = CreateEmbed::new()
                .title(":white_check_mark: Item deleted")
                .description(desc);
            Ok((embed, components))
        })
    };

    struct State {
        items: Vec<db::Item>,
    }
    let state = State {
        items: items.clone(),
    };
    let (embed, components) = task(&state, &mut *transaction, selected.id).await?;
    let reply = CreateReply::default()
        .embed(embed.clone())
        .components(components);

    let handle = ctx.send(reply).await?;

    let mut redo_conn = ctx.data().pool.acquire().await?;

    handle_edits(
        ctx,
        state,
        &handle,
        task,
        embed,
        &mut redo_conn,
        transaction,
    )
    .await?;

    Ok(())
}

#[poise::command(
    slash_command,
    rename = "box",
    subcommands("box_info", "box_add", "box_rm")
)]
pub async fn r#box(_ctx: Context<'_>) -> anyhow::Result<()> {
    Ok(())
}

#[poise::command(slash_command, rename = "info")]
pub async fn box_info(
    ctx: Context<'_>,
    #[description = "Box item"]
    #[autocomplete = autocomplete_item]
    r#box: String,
) -> anyhow::Result<()> {
    if ctx.guild_id().is_none() || !ALLOWED_GUILDS.contains(&ctx.guild_id().unwrap().get()) {
        let embed = CreateEmbed::new()
            .title("Not allowed")
            .description("You are not allowed to use this command in this server.");

        ctx.send(CreateReply::default().embed(embed)).await?;
        return Ok(());
    }
    let mut connection = ctx.data().pool.acquire().await?;
    let item = db::lookup_item(&mut connection, &r#box).await?;
    let item = match item.first() {
        Some(item) => item,
        None => {
            let embed = CreateEmbed::new()
                .title("No items found")
                .description("No items found with that name. Please try again.");

            let reply = CreateReply::default().embed(embed);

            ctx.send(reply).await?;
            return Ok(());
        }
    };

    let items = db::box_contents(&mut connection, item).await?;
    let message = "```".to_string() + &items.to_string() + "```";
    let embed = CreateEmbed::new()
        .title(format!(":white_check_mark: Box contents for {}", item.name))
        .description(message)
        .footer(CreateEmbedFooter::new(
            "🟢 = tracked within box. 🔴 = not within box.",
        ));

    ctx.send(CreateReply::default().embed(embed)).await?;

    Ok(())
}

async fn lookup_item_retaining_query(
    item: String,
    pool: &SqlitePool,
) -> anyhow::Result<(String, Option<db::Item>)> {
    let found = db::lookup_item(&mut *pool.acquire().await?, &item)
        .await?
        .into_iter()
        .nth(0);
    anyhow::Ok((item, found))
}

#[poise::command(slash_command, rename = "add")]
pub async fn box_add(
    ctx: Context<'_>,
    #[description = "Box item"]
    #[autocomplete = autocomplete_item]
    r#box: String,
    #[description = "Item"]
    #[autocomplete = autocomplete_item]
    item: String,
    #[description = "Item"]
    #[autocomplete = autocomplete_item]
    item2: Option<String>,
    #[description = "Item"]
    #[autocomplete = autocomplete_item]
    item3: Option<String>,
    #[description = "Item"]
    #[autocomplete = autocomplete_item]
    item4: Option<String>,
) -> anyhow::Result<()> {
    if ctx.guild_id().is_none() || !ALLOWED_GUILDS.contains(&ctx.guild_id().unwrap().get()) {
        let embed = CreateEmbed::new()
            .title("Not allowed")
            .description("You are not allowed to use this command in this server.");

        ctx.send(CreateReply::default().embed(embed)).await?;
        return Ok(());
    }
    let mut connection = ctx.data().pool.acquire().await?;
    let mut transaction = connection.begin().await?;
    let items: Vec<_> = try_join_all(
        [Some(r#box), Some(item), item2, item3, item4]
            .into_iter()
            .flatten()
            .map(|it| lookup_item_retaining_query(it, &mut &ctx.data().pool)),
    )
    .await?;

    // It does not compile if I inline it. wtf.
    let result_is_none = |it: &&(String, Option<db::Item>)| -> bool { it.1.is_none() };
    let format_error = |it: &(String, Option<db::Item>)| -> String {
        format!(":red_circle: Couldn't find anything matching '{}'", it.0)
    };

    let mut errors = items
        .iter()
        .filter(result_is_none)
        .map(format_error)
        .peekable();

    if let Some(_) = errors.peek() {
        let embed = CreateEmbed::new()
            .title("Couldn't find items")
            .description(errors.collect::<Vec<_>>().join("\n"));

        ctx.send(CreateReply::default().embed(embed)).await?;
        return Ok(());
    }

    let items = items
        .into_iter()
        .map(|(_, b)| b.unwrap())
        .collect::<Vec<_>>();

    let (box_item, rest) = items.split_first().unwrap();

    if let Err(err) = db::box_all(
        &mut *transaction,
        &ctx.author().id.to_string(),
        box_item,
        rest,
    )
    .await
    {
        ctx.send(
            CreateReply::default().embed(
                CreateEmbed::new()
                    .title(":x: Error")
                    .description(err.to_string()),
            ),
        )
        .await?;

        transaction.rollback().await?;
    } else {
        ctx.send(
            CreateReply::default()
                .embed(CreateEmbed::new().title(":white_check_mark: Items added to box")),
        )
        .await?;
        transaction.commit().await?;
    }

    Ok(())
}

#[poise::command(slash_command, rename = "rm")]
pub async fn box_rm(
    ctx: Context<'_>,
    r#box: String,
    #[description = "Item"]
    #[autocomplete = autocomplete_item]
    item: String,
    #[description = "Item"]
    #[autocomplete = autocomplete_item]
    item2: Option<String>,
    #[description = "Item"]
    #[autocomplete = autocomplete_item]
    item3: Option<String>,
    #[description = "Item"]
    #[autocomplete = autocomplete_item]
    item4: Option<String>,
) -> anyhow::Result<()> {
    if ctx.guild_id().is_none() || !ALLOWED_GUILDS.contains(&ctx.guild_id().unwrap().get()) {
        let embed = CreateEmbed::new()
            .title("Not allowed")
            .description("You are not allowed to use this command in this server.");

        ctx.send(CreateReply::default().embed(embed)).await?;
        return Ok(());
    }
    let mut connection = ctx.data().pool.acquire().await?;
    let mut transaction = connection.begin().await?;

    let items: Vec<_> = try_join_all(
        [Some(r#box), Some(item), item2, item3, item4]
            .into_iter()
            .flatten()
            .map(|it| lookup_item_retaining_query(it, &mut &ctx.data().pool)),
    )
    .await?;

    let result_is_none = |it: &&(String, Option<db::Item>)| -> bool { it.1.is_none() };
    let format_error = |it: &(String, Option<db::Item>)| -> String {
        format!(":red_circle: Couldn't find anything matching '{}'", it.0)
    };

    let mut errors = items
        .iter()
        .filter(result_is_none)
        .map(format_error)
        .peekable();

    if let Some(_) = errors.peek() {
        let embed = CreateEmbed::new()
            .title("Couldn't find items")
            .description(errors.collect::<Vec<_>>().join("\n"));

        ctx.send(CreateReply::default().embed(embed)).await?;
        return Ok(());
    }

    let items = items
        .into_iter()
        .map(|(_, b)| b.unwrap())
        .collect::<Vec<_>>();

    let (box_item, items) = items.split_first().unwrap();

    if let Err(err) = db::unbox_all(&mut *transaction, box_item, items).await {
        ctx.send(
            CreateReply::default().embed(
                CreateEmbed::new()
                    .title(":x: Error")
                    .description(err.to_string()),
            ),
        )
        .await?;

        transaction.rollback().await?;
    } else {
        ctx.send(
            CreateReply::default()
                .embed(CreateEmbed::new().title(":white_check_mark: Items removed from box")),
        )
        .await?;

        transaction.commit().await?;
    }

    Ok(())
}

#[tokio::main]
async fn main() -> color_eyre::Result<()> {
    // Login with a bot token from the environment
    let token = env::var("DISCORD_TOKEN").expect("Expected a token in the environment");
    // Set gateway intents, which decides what events the bot will be notified about
    let intents = GatewayIntents::non_privileged();

    let pool = SqlitePoolOptions::new()
        .max_connections(5)
        .connect("sqlite://./db.sqlite")
        .await?;

    let framework = poise::Framework::builder()
        .options(poise::FrameworkOptions {
            commands: vec![
                borrow(),
                blame(),
                give(),
                item(),
                r#box(),
                items(),
                store(),
                storage(),
            ],
            ..Default::default()
        })
        .setup(|ctx, _ready, framework| {
            println!("{} is ready!", _ready.user.name);
            Box::pin(async move {
                for guild_id in ALLOWED_GUILDS {
                    print!("Registering in guild {}..", guild_id);
                    poise::builtins::register_in_guild(
                        ctx,
                        &framework.options().commands,
                        GuildId::from(*guild_id),
                    )
                    .await?;
                    println!(" ok");
                }
                Ok(Data { pool })
            })
        })
        .build();

    let client = ClientBuilder::new(token, intents)
        .framework(framework)
        .await;
    client.unwrap().start().await.unwrap();
    Ok(())
}
