use super::{
    adt::{Decl, Elem, Expr, Node, Value, Yielder},
    composite::{OptionalType, Vertex},
    comprehension::envComprehension,
    context::OpContext,
    errors::Bottom,
    feature::Feature,
    kind::Kind,
    op::Op,
};
use crate::ast;

pub struct StructLit {
    node: Box<dyn ast::Node>,

    Decls: Vec<Box<dyn Decl>>,
    Fields: Vec<FieldInfo>,
    Dynamic: Vec<Box<DynamicField>>,
    Bulk: Vec<Box<BulkOptionalField>>,
    Additional: Vec<Box<Ellipsis>>,
    HasEmbed: bool,
    IsOpen: bool,
    initialized: bool,
    types: OptionalType,
}
pub struct FieldInfo {
    Label: Feature,
    Optional: Box<dyn Node>,
}
pub struct Field {
    node: Box<dyn ast::Node>,

    Label: Feature,
    Value: Box<dyn Expr>,
}
pub struct OptionalField {
    node: Box<dyn ast::Node>,

    Label: Feature,
    Value: Box<dyn Expr>,
}
pub struct BulkOptionalField {
    node: Box<dyn ast::Node>,

    Filter: Box<dyn Expr>,
    Value: Box<dyn Expr>,
    Label: Feature,
}
pub struct Ellipsis {
    node: Box<dyn ast::Node>,

    Value: Box<dyn Expr>,
}
pub struct DynamicField {
    node: Box<dyn ast::Node>,

    Key: Box<dyn Expr>,
    Value: Box<dyn Expr>,
}
pub struct ListLit {
    node: Box<dyn ast::Node>,

    Elems: Vec<Box<dyn Elem>>,
    info: Box<StructLit>,
}
pub struct Null {
    node: Box<dyn ast::Node>,
}
pub struct Bool {
    node: Box<dyn ast::Node>,

    b: bool,
}
pub struct Num {
    node: Box<dyn ast::Node>,

    k: Kind,
    n: f64,
}
pub struct String {
    node: Box<dyn ast::Node>,

    str: std::string::String,
    // RE:  regex::Regex
}
pub struct Bytes {
    node: Box<dyn ast::Node>,

    b: Box<str>,
    // RE:  regex::Regex
}
pub struct ListMarker {
    node: Box<dyn ast::Node>,

    is_open: bool,
}
pub struct StructMarker {
    needs_close: bool,
}
pub struct Top<'a> {
    node: ast::Ident<'a>,
}

pub struct BasicType {
    node: Box<dyn ast::Node>,

    k: Kind,
}
pub struct BoundExpr {
    node: Box<dyn ast::Node>,

    op: Op,
    expr: Box<dyn Expr>,
}
pub struct BoundValue {
    node: Box<dyn ast::Node>,

    op: Op,
    value: Box<dyn Value>,
}
pub struct NodeLink {
    node: Box<Vertex>,
}
pub struct FieldReference {
    node: Box<dyn ast::Node>,

    UpCount: u32,
    Label: Feature,
}
pub struct ValueReference {
    node: Box<dyn ast::Node>,

    UpCount: u32,
    Label: Feature,
}
pub struct LabelReference {
    node: Box<dyn ast::Node>,

    UpCount: u32,
}
pub struct DynamicReference {
    node: Box<dyn ast::Node>,

    UpCount: u32,
    Label: Box<dyn Expr>,

    Alias: Feature,
}
pub struct ImportReference {
    node: Box<dyn ast::Node>,

    ImportPath: Feature,
    Label: Feature,
}
pub struct LetReference {
    node: Box<dyn ast::Node>,

    UpCount: u32,
    Label: Feature,
    X: Box<dyn Expr>,
}
pub struct SelectorExpr {
    node: Box<dyn ast::Node>,

    X: Box<dyn Expr>,
    Sel: Feature,
}
pub struct IndexExpr {
    node: Box<dyn ast::Node>,

    X: Box<dyn Expr>,
    Index: Box<dyn Expr>,
}
pub struct SliceExpr {
    node: Box<dyn ast::Node>,

    X: Box<dyn Expr>,
    Lo: Box<dyn Expr>,
    Hi: Box<dyn Expr>,
    Stride: Box<dyn Expr>,
}
pub struct Interpolation {
    node: Box<dyn ast::Node>,
    K: Kind,
    Parts: Vec<Box<dyn Expr>>,
}
pub struct UnaryExpr {
    node: Box<dyn ast::Node>,

    Op: Op,
    X: Box<dyn Expr>,
}
pub struct BinaryExpr {
    node: Box<dyn ast::Node>,

    Op: Op,
    X: Box<dyn Expr>,
    Y: Box<dyn Expr>,
}
pub struct CallExpr {
    node: Box<dyn ast::Node>,

    Fun: Box<dyn Expr>,
    Args: Vec<Box<dyn Expr>>,
}
pub struct Builtin {
    node: Box<dyn ast::Node>,

    Params: Vec<Param>,
    Result: Kind,
    Func: Box<dyn Fn(Box<OpContext>, Vec<Box<dyn Value>>) -> Box<dyn Expr>>,

    PackageL: Feature,
    Name: String,
}
pub struct Param {
    Name: Feature,
    Value: Box<dyn Value>,
}
pub struct BuiltinValidator {
    node: Box<dyn ast::Node>,

    Builtin: Box<Builtin>,
    Args: Vec<Box<dyn Value>>,
}
pub struct DisjunctionExpr {
    node: Box<dyn ast::Node>,

    Values: Vec<Disjunct>,

    HasDefaults: bool,
}
pub struct Disjunct {
    val: Box<dyn Expr>,
    default: bool,
}
pub struct Conjunction {
    node: Box<dyn ast::Node>,

    Values: Vec<Box<dyn Value>>,
}
pub struct Disjunction {
    node: Box<dyn ast::Node>,

    Values: Vec<Box<Vertex>>,

    Errors: Box<Bottom>,

    // NumDefaults indicates the number of default values.
    NumDefaults: usize,
    HasDefaults: bool,
}
pub struct Comprehension {
    node: Box<dyn ast::Node>,

    Value: Box<dyn Node>,

    // Only used for partial comprehensions.
    comp: Box<envComprehension>,
    Nest: usize,
}
pub struct ForClause {
    node: Box<dyn ast::Node>,

    Key: Feature,
    Value: Feature,
    Src: Box<dyn Expr>,
    Dst: Box<dyn Yielder>,
}
pub struct IfClause {
    node: Box<dyn ast::Node>,

    Condition: Box<dyn Expr>,
    Dst: Box<dyn Yielder>,
}
pub struct LetClause {
    node: Box<dyn ast::Node>,

    Label: Feature,
    Expr: Box<dyn Expr>,
    Dst: Box<dyn Yielder>,
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
impl<'a> Node for Top<'a> {
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
        &self.node.source()
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
        &self.struct_lit.source()
    }
}
