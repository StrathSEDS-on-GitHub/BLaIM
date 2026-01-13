use std::fmt::{self, Display};

use crate::Item;

use crate::Owner;

use super::*;

#[derive(Debug, Clone)]
pub struct ItemTreeNodeImpl<T> {
    pub item: Item,
    pub present: bool,
    pub associated: T,
}

#[derive(Debug, Clone)]
pub struct ItemTreeImpl<T> {
    pub item: ItemTreeNodeImpl<T>,
    pub children: Vec<ItemTreeImpl<T>>,
}

pub type ItemTree = ItemTreeImpl<()>;
pub type ItemTreeNode = ItemTreeNodeImpl<()>;

pub type ItemTreeOwned = ItemTreeImpl<Option<Owner>>;
pub type ItemTreeNodeOwned = ItemTreeNodeImpl<Option<Owner>>;

impl ItemTreeImpl<()> {
    pub fn new(item: Item, present: bool) -> Self {
        Self {
            item: ItemTreeNodeImpl {
                item,
                present,
                associated: (),
            },
            children: Vec::new(),
        }
    }
}

impl<T> ItemTreeImpl<T> {
    pub fn new_associated(item: Item, present: bool, associated: T) -> Self {
        Self {
            item: ItemTreeNodeImpl {
                item,
                present,
                associated,
            },
            children: Vec::new(),
        }
    }

    pub fn add_children(
        &mut self,
        children: impl Iterator<Item = ItemTreeImpl<T>>,
    ) -> impl Iterator<Item = &mut ItemTreeImpl<T>> {
        let start = self.children.len();
        self.children.extend(children);

        self.children[start..].iter_mut()
    }

    pub fn find(&self, id: i32) -> Option<&ItemTreeImpl<T>> {
        if self.item.item.id == id {
            return Some(self);
        }
        for child in &self.children {
            if let Some(tree) = child.find(id) {
                return Some(tree);
            }
        }
        None
    }

    pub fn iter_depth_first(&self) -> impl Iterator<Item = (usize, &ItemTreeNodeImpl<T>, bool)> {
        let mut stack = vec![(0, self, true)];
        std::iter::from_fn(move || {
            if let Some((depth, tree, s)) = stack.pop() {
                if let Some((last, most)) = tree.children.split_last() {
                    stack.push((depth + 1, last, true));
                    most.iter().rev().for_each(|child| {
                        stack.push((depth + 1, child, false));
                    });
                }

                Some((depth, &tree.item, s))
            } else {
                None
            }
        })
    }

    pub fn into_iter_depth_first(self) -> impl Iterator<Item = (usize, ItemTreeNodeImpl<T>, bool)>
    where
        T: Clone,
    {
        let mut stack = vec![(0, self, true)];
        std::iter::from_fn(move || {
            if let Some((depth, tree, s)) = stack.pop() {
                if let Some((last, most)) = tree.children.split_last() {
                    stack.push((depth + 1, last.clone(), true));
                    most.iter().rev().for_each(|child| {
                        stack.push((depth + 1, child.clone(), false));
                    });
                }

                Some((depth, tree.item, s))
            } else {
                None
            }
        })
    }
}

impl<T> Display for ItemTreeImpl<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.iter_depth_first()
            .try_for_each(|(depth, it, last_child)| {
                let prefix_size = depth * 4;
                let tree_icon = if depth == 0 {
                    "  "
                } else if last_child {
                    "└─"
                } else {
                    "├─"
                };
                let present_icon = if it.present { "🟢" } else { "🔴" };
                writeln!(
                    f,
                    "{: <prefix_size$}{tree_icon} {present_icon} {}",
                    "", it.item.name
                )
            })
    }
}

impl<T> FromIterator<(usize, ItemTreeNodeImpl<T>, bool)> for ItemTreeImpl<T> {
    fn from_iter<U: IntoIterator<Item = (usize, ItemTreeNodeImpl<T>, bool)>>(iter: U) -> Self {
        let mut iter = iter.into_iter();
        let (_, root_node, _) = iter.next().unwrap();
        let mut root = ItemTreeImpl::new_associated(root_node.item, root_node.present, root_node.associated);

        let mut stack = vec![&mut root as *mut ItemTreeImpl<T>];
        for (depth, node, _) in iter {
            stack.truncate(depth);
            let tree = ItemTreeImpl::new_associated(node.item, node.present, node.associated);

            unsafe {
                let parent = stack.last_mut().unwrap();
                let ptr = (**parent)
                    .add_children(std::iter::once(tree))
                    .next()
                    .unwrap();
                stack.push(ptr);
            }
        }
        root
    }
}
