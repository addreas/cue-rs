use crate::ast::Ident;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

pub type MutableScope<T> = Rc<RefCell<Scope<T>>>;

#[derive(Debug, PartialEq, Clone, Default)]
pub struct Scope<T> {
    parent: Option<MutableScope<T>>,
    items: HashMap<Ident, T>,
}

impl<T: Clone> Scope<T> {
    pub fn new() -> MutableScope<T> {
        Rc::new(RefCell::new(Self {
            parent: None,
            items: HashMap::new(),
        }))
    }

    pub fn as_parent(outer: MutableScope<T>) -> MutableScope<T> {
        Rc::new(RefCell::new(Self {
            parent: Some(outer),
            items: HashMap::new(),
        }))
    }

    pub fn get(&self, ident: &Ident) -> Option<T> {
        match self.items.get(ident) {
            Some(object) => Some(object.clone()),
            None => self
                .parent
                .as_ref()
                .and_then(|outer| outer.borrow().get(ident)),
        }
    }

    pub fn set(&mut self, ident: Ident, object: T) {
        self.items.insert(ident, object);
    }
}
