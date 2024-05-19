# Blaim

StrathSEDS' Bot for Logistics and Inventory Management

![image](https://github.com/StrathSEDS-on-GitHub/BLaIM/assets/28653555/a572b603-0e82-475d-a5cf-3dc6f3453bfb)


## Usage

#### how use
- `/register_item <id> <desc>` add a new item to the tracker
- `/borrow <item>` to have ownership of an item assigned to you
- `/give <user> <item>` to transfer ownership of an item to someone else
- `/blame <item>` show the current owner and transfer history for the item
- `/items [user]` show all items, or all items held by someone

#### advanced stuff
The bot is capable of linking multiple items together for convenience with 'boxes'. An item can have several children linked to it. When the parent 'box' item is transferred, the children follow along. 

It's also possible to split out items from the parent box by borrowing just the item and not the full box. The bot will note this item as 'not present in box'  (though it will remember that the item belongs in the box) and transfers of the box will not transfer this item.  When the removed item is transferred back to the owner of the box, the bot will automatically link them together again.

- `/box add <items...>` mark item as belonging to a box
- `/box rm <items...>` mark item as not belonging to a box
- `/box info <box>` shows all the items within the box

#### so advanced it's cursed
the boxes are recursive. do what you will with this information
