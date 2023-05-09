use std::collections::HashMap;

use super::{
    adt::{Expr, Node},
    closed::{CloseInfo, CloseStats},
    composite::{Environment, Vertex},
    errors::Bottom,
    eval::{NodeContext, Stats},
    feature::StringIndexer,
};
use crate::ast;

pub type ReflectType = usize;

pub trait Runtime: StringIndexer {
    fn load_import(&self, importPath: &str) -> &Vertex;
    fn store_type(&self, t: ReflectType, src: ast::Expr, expr: Box<dyn Expr>);
    fn load_type(&self, t: ReflectType) -> (ast::Expr, Box<dyn Expr>, bool);
}

pub struct OpContext {
    runtime: Box<dyn Runtime>,
    format: Box<dyn Fn(dyn Node) -> String>,

    nest: i32,

    stats: Stats,
    free_list_node: Box<NodeContext>,

    e: Box<Environment>,
    src: Box<dyn ast::Node>,
    errs: Box<Bottom>,

    vertex: Box<Vertex>,

    generation: i32,
    closed: HashMap<Box<CloseInfo>, Box<CloseStats>>,
    todo: Box<CloseStats>,

    in_disjunct: i32,
    in_constraint: i32,
}
