use crate::value::Value;
use crate::ast::Ident;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

type Scoped = Rc<Value>;
pub type MutableScope = Rc<RefCell<Scope>>;

#[derive(Debug, PartialEq, Clone, Default)]
pub struct Scope {
    parent: Option<MutableScope>,
    items: HashMap<Ident, Scoped>,
}

impl Scope {
    pub fn new() -> MutableScope {
        Rc::new(RefCell::new(Self {
            parent: None,
            items: HashMap::new(),
        }))
    }

    pub fn as_parent(outer: MutableScope) -> MutableScope {
        Rc::new(RefCell::new(Self {
            parent: Some(outer),
            items: HashMap::new(),
        }))
    }


    pub fn get(&self, ident: &Ident) -> Option<Scoped> {
        match self.items.get(ident) {
            Some(object) => Some(object.clone()),
            None => self
                .parent
                .as_ref()
                .and_then(|outer| outer.borrow().get(ident)),
        }
    }

    pub fn set(&mut self, ident: Ident, object: Scoped) {
        self.items.insert(ident, object);
    }
}
