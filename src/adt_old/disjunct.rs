use super::{
    closed::CloseInfo,
    composite::Environment,
    expr::{Disjunction, DisjunctionExpr},
};

pub enum DefaultMode {
    MaybeDefault,
    IsDefault,
    NotDefault,
}
#[allow(dead_code)]
pub struct EnvDisjunct {
    env: Box<Environment>,
    clone_id: CloseInfo,
    expr: Box<DisjunctionExpr>,
    value: Box<Disjunction>,
    has_defaults: bool,
    parent_default_used: bool,
    child_default_used: bool,
}
