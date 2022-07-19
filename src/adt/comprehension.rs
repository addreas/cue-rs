use super::{
    adt::Node,
    closed::CloseInfo,
    composite::{Environment, Vertex},
    errors::Bottom,
    expr::{Comprehension, StructLit},
};

pub struct envComprehension {
    comp: Box<Comprehension>,
    node: Box<Vertex>,
    err: Box<Bottom>,
    envs: Vec<Box<Environment>>,
    done: bool,
    structs: Vec<Box<StructLit>>,
}

pub struct envYield {
    comprehension: envComprehension,
    env: Box<Environment>,
    id: CloseInfo,
    expr: Box<dyn Node>,
    nest: usize,
}
