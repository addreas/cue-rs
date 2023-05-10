use super::{
    adt::{Expr, Validator, Value},
    closed::CloseInfo,
    composite::{Conjunct, Environment, Vertex},
    comprehension::EnvYield,
    context::OpContext,
    disjunct::{DefaultMode, EnvDisjunct},
    errors::Bottom,
    expr::{BoundValue, DynamicField, Ellipsis, ListLit},
    kind::Kind,
};
#[allow(dead_code)]
pub struct Stats {
    disjunct_count: i64,
    unify_count: i64,

    freed: i64,
    retained: i64,
    reused: i64,
    allocs: i64,
}
#[allow(dead_code)]
pub struct NodeContext {
    next_free: Box<NodeContext>,
    ref_count: usize,
    ctx: Box<OpContext>,
    node: Box<Vertex>,
    arc_map: Vec<ArcKey>,
    snapshot: Vertex,
    result: Vertex,
    scalar: Box<dyn Value>,
    scalar_id: CloseInfo,
    kind: Kind,
    kind_expr: Box<dyn Expr>,
    kind_id: CloseInfo,
    lower_bound: Box<BoundValue>,
    upper_bound: Box<BoundValue>,
    checks: Vec<Box<dyn Validator>>,
    post_checks: Vec<EnvCheck>,
    errs: Box<Bottom>,
    conjuncts: Vec<Conjunct>,
    notify: Vec<Box<Vertex>>,
    dynamic_fields: Vec<EnvDynamic>,
    comprehensions: Vec<EnvYield>,
    a_struct: Box<dyn Expr>,
    a_struct_id: CloseInfo,
    lists: Vec<EnvList>,
    v_lists: Vec<Box<Vertex>>,
    exprs: Vec<EnvExpr>,
    has_top: bool,
    has_cycle: bool,
    has_non_cycle: bool,
    disjunctions: Vec<EnvDisjunct>,
    used_default: Vec<DefaultInfo>,
    default_mode: DefaultMode,
    disjuncts: Vec<Box<NodeContext>>,
    buffer: Vec<Box<NodeContext>>,
    disjunct_errs: Vec<Box<Bottom>>,
}
#[allow(dead_code)]
pub struct ArcKey {
    arc: Box<Vertex>,
    id: CloseInfo,
}
#[allow(dead_code)]
pub struct EnvExpr {
    c: Conjunct,
    err: Box<Bottom>,
}
#[allow(dead_code)]
pub struct EnvDynamic {
    env: Box<Environment>,
    field: Box<DynamicField>,
    id: CloseInfo,
    err: Box<Bottom>,
}
#[allow(dead_code)]
pub struct EnvList {
    env: Box<Environment>,
    list: Box<ListLit>,
    n: usize,
    elipsis: Box<Ellipsis>,
    id: CloseInfo,
}
#[allow(dead_code)]
pub struct EnvCheck {
    env: Box<Environment>,
    expr: Box<dyn Expr>,
    expect_error: bool,
}
#[allow(dead_code)]
pub struct DefaultInfo {
    parent_mode: DefaultMode,
    nested_mode: DefaultMode,
    orig_mode: DefaultMode,
}
