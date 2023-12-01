use std::collections::HashMap;
use crate::parser::ast::ast_type::{QualifierName};


pub struct ItemEntry<T> {
    pub name: String,
    val: T,
}

/// A namespace contains items and other nested namespaces.
pub struct Namespace<T> {
    pub name: String,
    children: HashMap<String, Namespace<T>>,
    items: HashMap<String, ItemEntry<T>>,
}

impl<T> Namespace<T> {
    /// Creates a namespace based on the name of the namespace
    pub fn new(name: String) -> Self {
        Namespace {
            name,
            children: HashMap::new(),
            items: HashMap::new(),
        }
    }

    fn find_item(&self, name: &str) -> Option<&T> {
        self.items.get(name).map(|entry| &entry.val)
    }

    /// Adds a child to the namespace.
    pub fn add_child(&mut self, child: Namespace<T>) {
        self.children.insert(child.name.clone(), child);
    }

    /// Returns a shared reference to a namespace and an item name for the specified type name.
    fn get_namespace<'a, 'b>(&'a self, name: &'b QualifierName) -> Option<(&'a Namespace<T>, &'b str)> {
        let mut current = self;
        let mut iter = name.iter();

        let mut next = iter.next();
        loop {
            // gets the next entry
            let entry = if let Some(entry) = next {
                entry
            } else {
                break None;
            };

            next = iter.next();
            if next.is_none() {
                // type name can be queried from the current namespace
                break Some((current, entry.as_str()));
            } else {
                // find child
                if let Some(child) = current.children.get(entry) {
                    current = child;
                } else {
                    break None;
                }
            }
        }
    }

    /// Returns a mutable reference to a namespace and an item name for the specified type name.
    fn get_namespace_mut<'a, 'b>(&'a mut self, name: &'b QualifierName) -> Option<(&'a mut Namespace<T>, &'b str)> {
        let mut current = self;
        let mut iter = name.iter();

        let mut next = iter.next();
        loop {
            // gets the next entry
            let entry = if let Some(entry) = next {
                entry
            } else {
                break None;
            };

            next = iter.next();
            if next.is_none() {
                // type name can be queried from the current namespace
                break Some((current, entry.as_str()));
            } else {
                // find child
                if let Some(child) = current.children.get_mut(entry) {
                    current = child;
                } else {
                    break None;
                }
            }
        }
    }

    fn get_namespace_or_insert<'a, 'b>(&'a mut self, name: &'b QualifierName) -> Option<(&'a mut Namespace<T>, &'b str)> {
        let mut current = self;
        let mut iter = name.iter();

        let mut next = iter.next();
        loop {
            // gets the next entry
            let entry = if let Some(entry) = next {
                entry
            } else {
                break None
            };

            next = iter.next();
            if next.is_none() {
                // target namespace has been reached
                break Some((current, entry.as_str()))
            } else {
                // find child
                current = current.children.entry(entry.clone())
                    .or_insert(Namespace::new(entry.clone()));
            }
        }
    }

    /// Inserts an item into the namespace hierarchy.
    /// If namespaces in the qualifier name of the item are missing in the namespace hierarchy,
    /// the corresponding entries are inserted automatically on the fly.
    ///
    /// Similarly to how the insert function of other collections returns the previous entry if a
    /// conflict occurs, this function works much in the same way.
    pub fn insert_item(&mut self, name: &QualifierName, item: T) -> Option<T> {
        if let Some((namespace, item_name)) = self.get_namespace_or_insert(name) {
            namespace.items.insert(
                item_name.to_string(),
                ItemEntry { val: item, name: item_name.to_string() }
            ).map(|entry| entry.val)
        } else {
            None
        }
    }

    /// Resolves the name of the child recursively down the namespace
    pub fn resolve(&self, name: &QualifierName) -> Option<&T> {
        self.get_namespace(name)
            .map(|(namespace, item_name)| namespace.find_item(item_name))
            .flatten()
    }

    /// Returns true, if the name leads to an item in the namespace tree.
    pub fn is_item(&self, name: &QualifierName) -> bool {
        self.get_namespace(name)
            .map(|(namespace, item_name)|
                namespace.items.contains_key(item_name)).unwrap_or(false)
    }

    /// Returns true, if the name leads to a namespace.
    pub fn is_namespace(&self, name: &QualifierName) -> bool {
        self.get_namespace(name).is_some()
    }
}
