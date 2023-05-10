use super::{
    adt::{Decl, Elem, Expr, Node, Value, Yielder},
    composite::{OptionalType, Vertex},
    comprehension::EnvComprehension,
    context::OpContext,
    errors::Bottom,
    feature::Feature,
    kind::Kind,
    op::Op,
};
use crate::ast;
#[allow(dead_code)]
pub struct StructLit {
    node: Box<dyn ast::Node>,

    decls: Vec<Box<dyn Decl>>,
    fields: Vec<FieldInfo>,
    dynamic: Vec<Box<DynamicField>>,
    bulk: Vec<Box<BulkOptionalField>>,
    additional: Vec<Box<Ellipsis>>,
    has_embed: bool,
    is_open: bool,
    initialized: bool,
    types: OptionalType,
}
#[allow(dead_code)]
pub struct FieldInfo {
    label: Feature,
    optional: Box<dyn Node>,
}
#[allow(dead_code)]
pub struct Field {
    node: Box<dyn ast::Node>,

    label: Feature,
    value: Box<dyn Expr>,
}
#[allow(dead_code)]
pub struct OptionalField {
    node: Box<dyn ast::Node>,

    label: Feature,
    value: Box<dyn Expr>,
}
#[allow(dead_code)]
pub struct BulkOptionalField {
    node: Box<dyn ast::Node>,

    filter: Box<dyn Expr>,
    value: Box<dyn Expr>,
    label: Feature,
}
#[allow(dead_code)]
pub struct Ellipsis {
    node: Box<dyn ast::Node>,

    value: Box<dyn Expr>,
}
#[allow(dead_code)]
pub struct DynamicField {
    node: Box<dyn ast::Node>,

    key: Box<dyn Expr>,
    value: Box<dyn Expr>,
}
#[allow(dead_code)]
pub struct ListLit {
    node: Box<dyn ast::Node>,

    elems: Vec<Box<dyn Elem>>,
    info: Box<StructLit>,
}
#[allow(dead_code)]
pub struct Null {
    node: Box<dyn ast::Node>,
}
#[allow(dead_code)]
pub struct Bool {
    node: Box<dyn ast::Node>,

    b: bool,
}
#[allow(dead_code)]
pub struct Num {
    node: Box<dyn ast::Node>,

    k: Kind,
    n: f64,
}
#[allow(dead_code)]
pub struct String {
    node: Box<dyn ast::Node>,

    str: std::string::String,
    // RE:  regex::Regex
}
#[allow(dead_code)]
pub struct Bytes {
    node: Box<dyn ast::Node>,

    b: Box<str>,
    // RE:  regex::Regex
}
#[allow(dead_code)]
pub struct ListMarker {
    node: Box<dyn ast::Node>,

    is_open: bool,
}
#[allow(dead_code)]
pub struct StructMarker {
    needs_close: bool,
}
#[allow(dead_code)]
pub struct Top {
    node: ast::Ident,
}
#[allow(dead_code)]
pub struct BasicType {
    node: Box<dyn ast::Node>,

    k: Kind,
}
#[allow(dead_code)]
pub struct BoundExpr {
    node: Box<dyn ast::Node>,

    op: Op,
    expr: Box<dyn Expr>,
}
#[allow(dead_code)]
pub struct BoundValue {
    node: Box<dyn ast::Node>,

    op: Op,
    value: Box<dyn Value>,
}
#[allow(dead_code)]
pub struct NodeLink {
    node: Box<Vertex>,
}
#[allow(dead_code)]
pub struct FieldReference {
    node: Box<dyn ast::Node>,

    up_count: u32,
    label: Feature,
}
#[allow(dead_code)]
pub struct ValueReference {
    node: Box<dyn ast::Node>,

    up_count: u32,
    label: Feature,
}
#[allow(dead_code)]
pub struct LabelReference {
    node: Box<dyn ast::Node>,

    up_count: u32,
}
#[allow(dead_code)]
pub struct DynamicReference {
    node: Box<dyn ast::Node>,

    up_count: u32,
    label: Box<dyn Expr>,

    alias: Feature,
}
#[allow(dead_code)]
pub struct ImportReference {
    node: Box<dyn ast::Node>,

    import_path: Feature,
    label: Feature,
}
#[allow(dead_code)]
pub struct LetReference {
    node: Box<dyn ast::Node>,

    up_count: u32,
    label: Feature,
    x: Box<dyn Expr>,
}
#[allow(dead_code)]
pub struct SelectorExpr {
    node: Box<dyn ast::Node>,

    x: Box<dyn Expr>,
    sel: Feature,
}
#[allow(dead_code)]
pub struct IndexExpr {
    node: Box<dyn ast::Node>,

    x: Box<dyn Expr>,
    index: Box<dyn Expr>,
}
#[allow(dead_code)]
pub struct SliceExpr {
    node: Box<dyn ast::Node>,

    x: Box<dyn Expr>,
    low: Box<dyn Expr>,
    high: Box<dyn Expr>,
    stride: Box<dyn Expr>,
}
#[allow(dead_code)]
pub struct Interpolation {
    node: Box<dyn ast::Node>,
    k: Kind,
    parts: Vec<Box<dyn Expr>>,
}
#[allow(dead_code)]
pub struct UnaryExpr {
    node: Box<dyn ast::Node>,

    op: Op,
    x: Box<dyn Expr>,
}
#[allow(dead_code)]
pub struct BinaryExpr {
    node: Box<dyn ast::Node>,

    op: Op,
    x: Box<dyn Expr>,
    y: Box<dyn Expr>,
}
#[allow(dead_code)]
pub struct CallExpr {
    node: Box<dyn ast::Node>,

    fun: Box<dyn Expr>,
    args: Vec<Box<dyn Expr>>,
}
#[allow(dead_code)]
pub struct Builtin {
    node: Box<dyn ast::Node>,

    params: Vec<Param>,
    result: Kind,
    func: Box<dyn Fn(Box<OpContext>, Vec<Box<dyn Value>>) -> Box<dyn Expr>>,

    package: Feature,
    name: String,
}
#[allow(dead_code)]
pub struct Param {
    name: Feature,
    value: Box<dyn Value>,
}
#[allow(dead_code)]
pub struct BuiltinValidator {
    node: Box<dyn ast::Node>,

    builtin: Box<Builtin>,
    args: Vec<Box<dyn Value>>,
}
#[allow(dead_code)]
pub struct DisjunctionExpr {
    node: Box<dyn ast::Node>,

    values: Vec<Disjunct>,

    has_defaults: bool,
}
#[allow(dead_code)]
pub struct Disjunct {
    val: Box<dyn Expr>,
    default: bool,
}
#[allow(dead_code)]
pub struct Conjunction {
    node: Box<dyn ast::Node>,

    values: Vec<Box<dyn Value>>,
}
#[allow(dead_code)]
pub struct Disjunction {
    node: Box<dyn ast::Node>,

    values: Vec<Box<Vertex>>,

    errors: Box<Bottom>,

    // NumDefaults indicates the number of default values.
    num_defaults: usize,
    has_defaults: bool,
}
#[allow(dead_code)]
pub struct Comprehension {
    node: Box<dyn ast::Node>,

    value: Box<dyn Node>,

    // Only used for partial comprehensions.
    comp: Box<EnvComprehension>,
    nest: usize,
}
#[allow(dead_code)]
pub struct ForClause {
    node: Box<dyn ast::Node>,

    key: Feature,
    value: Feature,
    src: Box<dyn Expr>,
    dst: Box<dyn Yielder>,
}
#[allow(dead_code)]
pub struct IfClause {
    node: Box<dyn ast::Node>,

    condition: Box<dyn Expr>,
    dst: Box<dyn Yielder>,
}
#[allow(dead_code)]
pub struct LetClause {
    node: Box<dyn ast::Node>,

    label: Feature,
    expr: Box<dyn Expr>,
    dst: Box<dyn Yielder>,
}
pub struct ValueClause {
    struct_lit: StructLit,
}

impl Node for Conjunction {
    fn source(&self) -> &Box<dyn ast::Node> {
        &self.node
    }
}
impl Node for Disjunction {
    fn source(&self) -> &Box<dyn ast::Node> {
        &self.node
    }
}
impl Node for BoundValue {
    fn source(&self) -> &Box<dyn ast::Node> {
        &self.node
    }
}
impl Node for Builtin {
    fn source(&self) -> &Box<dyn ast::Node> {
        &self.node
    }
}
impl Node for BuiltinValidator {
    fn source(&self) -> &Box<dyn ast::Node> {
        &self.node
    }
}
impl Node for Null {
    fn source(&self) -> &Box<dyn ast::Node> {
        &self.node
    }
}
impl Node for Bool {
    fn source(&self) -> &Box<dyn ast::Node> {
        &self.node
    }
}
impl Node for Num {
    fn source(&self) -> &Box<dyn ast::Node> {
        &self.node
    }
}
impl Node for String {
    fn source(&self) -> &Box<dyn ast::Node> {
        &self.node
    }
}
impl Node for Bytes {
    fn source(&self) -> &Box<dyn ast::Node> {
        &self.node
    }
}
impl Node for Top {
    fn source(&self) -> &Box<dyn ast::Node> {
        todo!()
    }
}
impl Node for BasicType {
    fn source(&self) -> &Box<dyn ast::Node> {
        &self.node
    }
}
impl Node for StructLit {
    fn source(&self) -> &Box<dyn ast::Node> {
        &self.node
    }
}
impl Node for ListLit {
    fn source(&self) -> &Box<dyn ast::Node> {
        &self.node
    }
}
impl Node for BoundExpr {
    fn source(&self) -> &Box<dyn ast::Node> {
        &self.node
    }
}
impl Node for NodeLink {
    fn source(&self) -> &Box<dyn ast::Node> {
        self.node.source()
    }
}
impl Node for FieldReference {
    fn source(&self) -> &Box<dyn ast::Node> {
        &self.node
    }
}
impl Node for ValueReference {
    fn source(&self) -> &Box<dyn ast::Node> {
        &self.node
    }
}
impl Node for LabelReference {
    fn source(&self) -> &Box<dyn ast::Node> {
        &self.node
    }
}
impl Node for DynamicReference {
    fn source(&self) -> &Box<dyn ast::Node> {
        &self.node
    }
}
impl Node for ImportReference {
    fn source(&self) -> &Box<dyn ast::Node> {
        &self.node
    }
}
impl Node for LetReference {
    fn source(&self) -> &Box<dyn ast::Node> {
        &self.node
    }
}
impl Node for SelectorExpr {
    fn source(&self) -> &Box<dyn ast::Node> {
        &self.node
    }
}
impl Node for IndexExpr {
    fn source(&self) -> &Box<dyn ast::Node> {
        &self.node
    }
}
impl Node for SliceExpr {
    fn source(&self) -> &Box<dyn ast::Node> {
        &self.node
    }
}
impl Node for Interpolation {
    fn source(&self) -> &Box<dyn ast::Node> {
        &self.node
    }
}
impl Node for UnaryExpr {
    fn source(&self) -> &Box<dyn ast::Node> {
        &self.node
    }
}
impl Node for BinaryExpr {
    fn source(&self) -> &Box<dyn ast::Node> {
        &self.node
    }
}
impl Node for CallExpr {
    fn source(&self) -> &Box<dyn ast::Node> {
        &self.node
    }
}
impl Node for DisjunctionExpr {
    fn source(&self) -> &Box<dyn ast::Node> {
        &self.node
    }
}
impl Node for Field {
    fn source(&self) -> &Box<dyn ast::Node> {
        &self.node
    }
}
impl Node for OptionalField {
    fn source(&self) -> &Box<dyn ast::Node> {
        &self.node
    }
}
impl Node for BulkOptionalField {
    fn source(&self) -> &Box<dyn ast::Node> {
        &self.node
    }
}
impl Node for DynamicField {
    fn source(&self) -> &Box<dyn ast::Node> {
        &self.node
    }
}
impl Node for Ellipsis {
    fn source(&self) -> &Box<dyn ast::Node> {
        &self.node
    }
}
impl Node for Comprehension {
    fn source(&self) -> &Box<dyn ast::Node> {
        &self.node
    }
}
impl Node for ForClause {
    fn source(&self) -> &Box<dyn ast::Node> {
        &self.node
    }
}
impl Node for IfClause {
    fn source(&self) -> &Box<dyn ast::Node> {
        &self.node
    }
}
impl Node for LetClause {
    fn source(&self) -> &Box<dyn ast::Node> {
        &self.node
    }
}
impl Node for ValueClause {
    fn source(&self) -> &Box<dyn ast::Node> {
        self.struct_lit.source()
    }
}
