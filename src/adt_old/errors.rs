use super::adt::{Node, Value};
use crate::ast;

pub type Error = usize; //TODO
#[allow(dead_code)]
pub struct Bottom {
    node: Box<dyn ast::Node>,

    err: Error,

    code: ErrorCode,
    has_recursive: bool,
    child_error: bool,
    not_exists: bool,

    value: Box<dyn Value>,
}

impl Node for Bottom {
    fn source(&self) -> &Box<dyn ast::Node> {
        &self.node
    }
}

pub enum ErrorCode {
    EvalError,
    UserError,
    StructuralCycleError,
    IncompleteError,
    CycleError,
}
