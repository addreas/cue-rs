use super::{
    adt::{Expr, Validator, Value},
    closed::CloseInfo,
    composite::{Conjunct, Environment, Vertex},
    comprehension::envYield,
    context::OpContext,
    disjunct::{defaultMode, envDisjunct},
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
    nextFree: Box<NodeContext>,
    refCount: usize,
    ctx: Box<OpContext>,
    node: Box<Vertex>,
    arcMap: Vec<arcKey>,
    snapshot: Vertex,
    result: Vertex,
    scalar: Box<dyn Value>,
    scalarID: CloseInfo,
    kind: Kind,
    kindExpr: Box<dyn Expr>,
    kindID: CloseInfo,
    lowerBound: Box<BoundValue>,
    upperBound: Box<BoundValue>,
    checks: Vec<Box<dyn Validator>>,
    postChecks: Vec<envCheck>,
    errs: Box<Bottom>,
    conjuncts: Vec<Conjunct>,
    notify: Vec<Box<Vertex>>,
    dynamicFields: Vec<envDynamic>,
    comprehensions: Vec<envYield>,
    aStruct: Box<dyn Expr>,
    aStructID: CloseInfo,
    lists: Vec<envList>,
    vLists: Vec<Box<Vertex>>,
    exprs: Vec<envExpr>,
    hasTop: bool,
    hasCycle: bool,
    hasNonCycle: bool,
    disjunctions: Vec<envDisjunct>,
    usedDefault: Vec<defaultInfo>,
    defaultMode: defaultMode,
    disjuncts: Vec<Box<NodeContext>>,
    buffer: Vec<Box<NodeContext>>,
    disjunctErrs: Vec<Box<Bottom>>,
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
    expectError: bool,
}

pub struct defaultInfo {
    parentMode: defaultMode,
    nestedMode: defaultMode,
    origMode: defaultMode,
}
