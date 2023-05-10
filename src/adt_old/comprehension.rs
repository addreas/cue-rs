use super::{
    adt::Node,
    closed::CloseInfo,
    composite::{Environment, Vertex},
    errors::Bottom,
    expr::{Comprehension, StructLit},
};
#[allow(dead_code)]
pub struct EnvComprehension {
    comp: Box<Comprehension>,
    node: Box<Vertex>,
    err: Box<Bottom>,
    envs: Vec<Box<Environment>>,
    done: bool,
    structs: Vec<Box<StructLit>>,
}
#[allow(dead_code)]
pub struct EnvYield {
    comprehension: EnvComprehension,
    env: Box<Environment>,
    id: CloseInfo,
    expr: Box<dyn Node>,
    nest: usize,
}
