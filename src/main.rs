#![feature(lazy_cell)]
#![feature(iter_intersperse)]
#![feature(let_chains)]

use std::env;

use poise::{CreateReply, ReplyHandle};
use serenity::all::{
    ButtonStyle, ClientBuilder, ComponentInteractionCollector, CreateActionRow, CreateButton,
    CreateEmbed, CreateEmbedFooter, CreateInteractionResponse, CreateSelectMenu,
    CreateSelectMenuKind, CreateSelectMenuOption, EditMessage, User,
};
use serenity::futures::future::try_join_all;
use serenity::prelude::*;
use sqlx::sqlite::SqlitePoolOptions;
use sqlx::SqlitePool;

mod db;

struct Data {
    pool: SqlitePool,
}

type Error = anyhow::Error;
type Context<'a> = poise::Context<'a, Data, Error>;

async fn autocomplete_item<'a>(
    ctx: Context<'_>,
    partial: &'a str,
) -> impl Iterator<Item = String> + 'a {
    db::lookup_item(&ctx.data().pool, partial)
        .await
        .unwrap_or_default()
        .into_iter()
        .map(|item| item.name)
}

async fn make_embed(
    selected: &db::Item,
    from: &Option<String>,
    to: &str,
    alternatives: &[db::Item],
) -> (CreateEmbed, Vec<CreateActionRow>) {
    let previous_owner = from
        .as_ref()
        .map(|f| format!("<@{}>", f))
        .unwrap_or("No one 👻".to_string());

    let new_owner = format!("<@{}>", to);

    let embed = CreateEmbed::new()
        .title(format!(
            ":white_check_mark: Borrowed item {}",
            selected.name
        ))
        .description(format!(
            ":red_square: **-** {} \n:arrow_down:\n:green_square: **+** {}",
            previous_owner, new_owner
        ))
        .footer(CreateEmbedFooter::new(chrono::Utc::now().to_rfc2822()));

    let mut components = vec![CreateActionRow::Buttons(vec![CreateButton::new("cancel")
        .label("Cancel")
        .style(ButtonStyle::Danger)])];

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

async fn handle_edits(
    ctx: Context<'_>,
    embed: CreateEmbed,
    inserted_id: i32,
    items: &Vec<db::Item>,
    handle: &ReplyHandle<'_>,
    new_owner: &str,
) -> anyhow::Result<()> {
    let message_id = handle.message().await?.id;
    let mut deleted = false;

    while let Some(ref mut mci) = ComponentInteractionCollector::new(ctx)
        .author_id(ctx.author().id)
        .timeout(std::time::Duration::from_secs(60))
        .channel_id(ctx.channel_id())
        .message_id(message_id)
        .filter(|i| i.data.custom_id == "cancel" || i.data.custom_id == "replace")
        .await
    {
        match mci.data.custom_id.as_str() {
            "cancel" => {
                db::delete_borrow(&ctx.data().pool, inserted_id).await?;
                mci.message.delete(&ctx).await?;
                mci.create_response(ctx, CreateInteractionResponse::Acknowledge)
                    .await?;
                deleted = true;
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
                let item = db::get_item_by_id(&ctx.data().pool, item_id)
                    .await?
                    .expect("Item not found");
                let previous_owner = db::get_last_holder(&ctx.data().pool, item.id).await?;
                let (embed, components) = make_embed(
                    &item,
                    &previous_owner,
                    new_owner,
                    &items
                        .clone()
                        .into_iter()
                        .filter(|it| it != &item)
                        .collect::<Vec<_>>(),
                )
                .await;

                db::update_borrow_item(&ctx.data().pool, inserted_id, item.id).await?;

                mci.message
                    .edit(
                        ctx,
                        EditMessage::default().embed(embed).components(components),
                    )
                    .await?;
                mci.create_response(ctx, CreateInteractionResponse::Acknowledge)
                    .await?;
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
    let items = db::lookup_item(&ctx.data().pool, &item).await?;

    let (selected, alternatives) = match items.split_first() {
        Some((selected, alternatives)) => (selected, alternatives),
        None => {
            let embed = CreateEmbed::new()
                .title("No items found")
                .description("No items found with that name. Please try again.");

            let reply = CreateReply::default().embed(embed);

            ctx.send(reply).await?;
            return Ok(());
        }
    };

    let previous_owner = db::get_last_holder(&ctx.data().pool, selected.id).await?;

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

    let (embed, components) = make_embed(
        selected,
        &previous_owner,
        &ctx.author().id.to_string(),
        alternatives,
    )
    .await;
    let reply = CreateReply::default()
        .embed(embed.clone())
        .components(components);

    let inserted_id =
        db::borrow_item(&ctx.data().pool, selected.id, &ctx.author().id.to_string()).await?;

    let handle = ctx.send(reply).await?;

    handle_edits(
        ctx,
        embed,
        inserted_id,
        &items,
        &handle,
        &ctx.author().id.to_string(),
    )
    .await?;

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
    let item = db::lookup_item(&ctx.data().pool, &item).await?;
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

    let history = db::borrow_history(&ctx.data().pool, item.id).await?;
    if history.is_empty() {
        let embed = CreateEmbed::new()
            .title("No history found")
            .description("No history found for this item.");

        let reply = CreateReply::default().embed(embed);

        ctx.send(reply).await?;
        return Ok(());
    }

    let history_text = history
        .windows(2)
        .map(|pair| {
            let (to, time) = &pair[0];
            let (from, _) = &pair[1];

            format!(
                "<@{}> :right_arrow: <@{}> <t:{}:R>",
                from,
                to,
                time.timestamp()
            )
        })
        .collect::<Vec<_>>()
        .join("\n");

    let embed = CreateEmbed::new()
        .title(format!("Borrow history for {}", item.name))
        .field(
            ":green_square: Current holder",
            format!(
                "<@{}> since <t:{}:R>",
                history.first().unwrap().0,
                history.first().unwrap().1.timestamp()
            ),
            false,
        )
        .description(history_text);

    ctx.send(CreateReply::default().embed(embed)).await?;

    Ok(())
}

/// Gives ownership of an item to another user
#[poise::command(slash_command)]
async fn give(
    ctx: Context<'_>,
    #[description = "User"] user: User,
    #[description = "Item"]
    #[autocomplete = autocomplete_item]
    item: String,
) -> Result<(), Error> {
    let items = db::lookup_item(&ctx.data().pool, &item).await?;

    let (selected, alternatives) = match items.split_first() {
        Some((selected, alternatives)) => (selected, alternatives),
        None => {
            let embed = CreateEmbed::new()
                .title("No items found")
                .description("No items found with that name. Please try again.");

            let reply = CreateReply::default().embed(embed);

            ctx.send(reply).await?;
            return Ok(());
        }
    };

    let owner = db::get_last_holder(&ctx.data().pool, selected.id).await?;

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

    let inserted_id = db::borrow_item(&ctx.data().pool, selected.id, &user.id.to_string()).await?;

    let (embed, components) =
        make_embed(selected, &owner, &user.id.to_string(), alternatives).await;
    let reply = CreateReply::default()
        .embed(embed.clone())
        .components(components);

    let handle = ctx.send(reply).await?;

    handle_edits(
        ctx,
        embed,
        inserted_id,
        &items,
        &handle,
        &user.id.to_string(),
    )
    .await?;

    Ok(())
}

/// Registers an item in the database
#[poise::command(slash_command)]
async fn register_item(
    ctx: Context<'_>,
    #[description = "Short ID (e.g ffmini2)"] strid: String,
    #[description = "Long descriptor of the item"] name: String,
) -> anyhow::Result<()> {
    db::register_item(&ctx.data().pool, &strid, &name).await?;
    ctx.reply(":white_check_mark: Item registered").await?;

    Ok(())
}

#[poise::command(
    slash_command,
    rename = "box",
    subcommands("box_info", "box_add", "box_rm")
)]
pub async fn r#box(ctx: Context<'_>) -> anyhow::Result<()> {
    Ok(())
}

#[poise::command(slash_command, rename = "info")]
pub async fn box_info(
    ctx: Context<'_>,
    #[description = "Box item"]
    #[autocomplete = autocomplete_item]
    r#box: String,
) -> anyhow::Result<()> {
    let item = db::lookup_item(&ctx.data().pool, &r#box).await?;
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

    let items = db::box_contents(&ctx.data().pool, item).await?;
    let message = "```".to_owned()
        + &items
            .iter_depth_first()
            .map(|(depth, it, last_child)| {
                let prefix_size = depth * 4;
                let tree_icon = if last_child { "└─" } else { "├─" };
                let present_icon = if it.item.present { "🟢" } else { "🔴" };
                format!(
                    "{: <prefix_size$} {tree_icon} {present_icon} {}",
                    "", it.item.item.name
                )
            })
            .intersperse('\n'.to_string())
            .collect::<String>()
        + "```";

    let embed = CreateEmbed::new()
        .title(format!(":white_check_mark: Box contents for {}", item.name))
        .description(message);

    ctx.send(CreateReply::default().embed(embed)).await?;

    Ok(())
}

async fn lookup_item_retaining_query(
    item: String,
    pool: &SqlitePool,
) -> anyhow::Result<(String, Option<db::Item>)> {
    let found = db::lookup_item(&pool, &item).await?.into_iter().nth(0);
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
    let items: Vec<_> = try_join_all(
        [Some(r#box), Some(item), item2, item3, item4]
            .into_iter()
            .flatten()
            .map(|it| lookup_item_retaining_query(it, &ctx.data().pool)),
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

    db::box_all(
        &ctx.data().pool,
        &ctx.author().id.to_string(),
        box_item,
        rest,
    )
    .await?;

    Ok(())
}

#[poise::command(slash_command, rename = "rm")]
pub async fn box_rm(
    ctx: Context<'_>,
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
    #[description = "Item"]
    #[autocomplete = autocomplete_item]
    item5: Option<String>,
) -> anyhow::Result<()> {
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
            commands: vec![borrow(), blame(), give(), register_item(), r#box()],
            ..Default::default()
        })
        .setup(|ctx, _ready, framework| {
            Box::pin(async move {
                poise::builtins::register_globally(ctx, &framework.options().commands).await?;
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

#[cfg(test)]
mod tests {
    use super::*;
}
