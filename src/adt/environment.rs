use crate::ast::Ident;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

pub type MutableEnvironment<T> = Rc<RefCell<Environment<T>>>;

#[derive(Debug, PartialEq, Clone, Default)]
pub struct Environment<T> {
    parent: Option<MutableEnvironment<T>>,
    items: HashMap<Ident, T>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct EnvironmentRef<'a, T> {
    inner: &'a Environment<T>
}

impl<'a, T: Clone> EnvironmentRef<'a, T> {
    pub fn get(&self, ident: &Ident) -> Option<T> {
        self.inner.get(ident)
    }
}

impl<T: Clone> Environment<T> {
    pub fn new() -> MutableEnvironment<T> {
        Rc::new(RefCell::new(Self {
            parent: None,
            items: HashMap::new(),
        }))
    }

    pub fn as_parent(outer: MutableEnvironment<T>) -> MutableEnvironment<T> {
        Rc::new(RefCell::new(Self {
            parent: Some(outer),
            items: HashMap::new(),
        }))
    }

    pub fn as_env_ref(&self) -> EnvironmentRef<T> {
        return EnvironmentRef { inner: &self }
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

