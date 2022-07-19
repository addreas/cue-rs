use super::{
    closed::CloseInfo,
    composite::Environment,
    expr::{Disjunction, DisjunctionExpr},
};

pub enum defaultMode {
    maybeDefault,
    isDefault,
    notDefault,
}

pub struct envDisjunct {
    env: Box<Environment>,
    cloneID: CloseInfo,
    expr: Box<DisjunctionExpr>,
    value: Box<Disjunction>,
    hasDefaults: bool,
    parentDefaultUsed: bool,
    childDefaultUsed: bool,
}
