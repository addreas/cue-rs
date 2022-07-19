use super::{
    adt::{Expr, Validator, Value},
    closed::CloseInfo,
    composite::{Conjunct, Environment, Vertex},
    comprehension::envYield,
    context::OpContext,
    disjunct::{DefaultMode, EnvDisjunct},
    errors::Bottom,
    expr::{BoundValue, DynamicField, Ellipsis, ListLit},
    kind::Kind,
};

pub struct Stats {
    disjunct_count: i64,
    unify_count: i64,

    freed: i64,
    retained: i64,
    reused: i64,
    allocs: i64,
}

pub struct NodeContext {
    next_free: Box<NodeContext>,
    ref_count: usize,
    ctx: Box<OpContext>,
    node: Box<Vertex>,
    arc_map: Vec<arcKey>,
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
    post_checks: Vec<envCheck>,
    errs: Box<Bottom>,
    conjuncts: Vec<Conjunct>,
    notify: Vec<Box<Vertex>>,
    dynamic_fields: Vec<envDynamic>,
    comprehensions: Vec<envYield>,
    a_struct: Box<dyn Expr>,
    a_struct_id: CloseInfo,
    lists: Vec<envList>,
    v_lists: Vec<Box<Vertex>>,
    exprs: Vec<envExpr>,
    has_top: bool,
    has_cycle: bool,
    has_non_cycle: bool,
    disjunctions: Vec<EnvDisjunct>,
    used_default: Vec<defaultInfo>,
    default_mode: DefaultMode,
    disjuncts: Vec<Box<NodeContext>>,
    buffer: Vec<Box<NodeContext>>,
    disjunct_errs: Vec<Box<Bottom>>,
}

pub struct arcKey {
    arc: Box<Vertex>,
    id: CloseInfo,
}

pub struct envExpr {
    c: Conjunct,
    err: Box<Bottom>,
}

pub struct envDynamic {
    env: Box<Environment>,
    field: Box<DynamicField>,
    id: CloseInfo,
    err: Box<Bottom>,
}

pub struct envList {
    env: Box<Environment>,
    list: Box<ListLit>,
    n: usize,
    elipsis: Box<Ellipsis>,
    id: CloseInfo,
}

pub struct envCheck {
    env: Box<Environment>,
    expr: Box<dyn Expr>,
    expect_error: bool,
}

pub struct defaultInfo {
    parent_mode: DefaultMode,
    nested_mode: DefaultMode,
    orig_mode: DefaultMode,
}
