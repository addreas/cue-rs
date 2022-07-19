use std::collections::HashMap;

use crate::ast;

use super::{
    adt::{BaseValue, Expr, Node, Value},
    closed::CloseInfo,
    errors::Bottom,
    eval::NodeContext,
    expr::StructLit,
    feature::Feature,
};

pub struct Environment {
    up: Box<Environment>,
    vertex: Box<Vertex>,

    dynamicLabel: Feature,

    cyclic: bool,

    deref: Vec<Vertex>,

    cycles: Vec<Vertex>,

    cache: HashMap<Box<dyn Expr>, Box<dyn Value>>,
}
pub struct Vertex {
    node: Box<dyn ast::Node>,

    parent: Box<Vertex>,
    label: Feature,
    state: Box<NodeContext>,
    status: VertexStatus,
    is_data: bool,
    closed: bool,
    arc_type: ArcType,
    eval_count: usize,
    self_count: usize,
    base_value: Box<dyn BaseValue>,
    child_errors: Box<Bottom>,
    arcs: Vec<Vertex>,
    conjuncts: Vec<Conjunct>,
    structs: Vec<StructInfo>,
}

pub enum VertexStatus {
    Unprocessed,
    Evaluating,
    Partial,
    AllArcs,
    EvaluatingArcs,
    Finalized,
}

enum ArcType {
    ArcMember,
    ArcVoid,
}
pub enum OptionalType {
    HasField,          // X: T
    HasDynamic,        // (X): T or "\(X)": T
    HasPattern,        // [X]: T
    HasComplexPattern, // anything but a basic type
    HasAdditional,     // ...T
    IsOpen,            // Defined for all fields
}
pub struct StructInfo {
    struct_lit: StructLit,
    close_info: CloseInfo,

    env: Box<Environment>,
    disable: bool,
    embedding: bool,
}

pub struct Conjunct {
    env: Box<Environment>,
    x: Box<dyn Node>,
    close_info: CloseInfo,
}

impl Node for Vertex {
    fn source(&self) -> &Box<dyn ast::Node> {
        &self.node
    }
}
