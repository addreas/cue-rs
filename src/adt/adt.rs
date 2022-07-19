use super::{
    super::ast,
    composite::{Environment, Vertex, VertexStatus},
    context::OpContext,
    errors::Bottom,
    expr::*,
    kind::Kind,
};

pub trait Node {
    fn source(&self) -> &Box<dyn ast::Node>;
}
pub trait Decl: Node {}
pub trait Elem: Decl {}
pub trait Expr: Elem {}
pub trait BaseValue {
    fn kind(&self) -> Kind;
}
pub trait Value: Expr + BaseValue {
    fn concreteness(&self) {}
}
pub trait Evaluator: Node {
    fn evaluate(&self, ctx: &OpContext) -> Box<dyn Value>;
}
pub trait Resolver: Node {
    fn resolve(&self, ctx: &OpContext, state: VertexStatus) -> &Vertex;
}
pub trait Yielder: Node {
    fn yyield(&self, ctx: &OpContext, function: Box<dyn Fn(Environment)>);
}
pub trait Validator: Value {
    fn validate(&self, ctx: &OpContext, v: Box<dyn Value>) -> &Bottom;
}

impl Expr for StructLit {}
impl Expr for ListLit {}
impl Expr for DisjunctionExpr {}

impl Expr for Bottom {}
impl Expr for Null {}
impl Expr for Bool {}
impl Expr for Num {}
impl Expr for String {}
impl Expr for Bytes {}
impl Expr for Top<'static> {}
impl Expr for BasicType {}
impl Expr for Vertex {}
// impl Expr for ListMarker {}
// impl Expr for StructMarker {}
impl Expr for Conjunction {}
impl Expr for Disjunction {}
impl Expr for BoundValue {}
impl Expr for BuiltinValidator {}
impl Expr for Builtin {}

impl Expr for NodeLink {}
impl Expr for FieldReference {}
impl Expr for ValueReference {}
impl Expr for LabelReference {}
impl Expr for DynamicReference {}
impl Expr for ImportReference {}
impl Expr for LetReference {}

impl Expr for BoundExpr {}
impl Expr for SelectorExpr {}
impl Expr for IndexExpr {}
impl Expr for SliceExpr {}
impl Expr for Interpolation {}
impl Expr for UnaryExpr {}
impl Expr for BinaryExpr {}
impl Expr for CallExpr {}

impl Decl for Field {}
impl Decl for OptionalField {}
impl Decl for BulkOptionalField {}
impl Decl for DynamicField {}

impl Elem for Ellipsis {}
impl Decl for Ellipsis {}

impl Decl for LetClause {}

impl Decl for StructLit {}
impl Elem for StructLit {}
impl Decl for ListLit {}
impl Elem for ListLit {}
impl Decl for Bottom {}
impl Elem for Bottom {}
impl Decl for Null {}
impl Elem for Null {}
impl Decl for Bool {}
impl Elem for Bool {}
impl Decl for Num {}
impl Elem for Num {}
impl Decl for String {}
impl Elem for String {}
impl Decl for Bytes {}
impl Elem for Bytes {}
impl Decl for Top<'static> {}
impl Elem for Top<'static> {}
impl Decl for BasicType {}
impl Elem for BasicType {}
impl Decl for BoundExpr {}
impl Elem for BoundExpr {}
impl Decl for Vertex {}
impl Elem for Vertex {}
// impl Decl for ListMarker {}
// impl Elem for ListMarker {}
// impl Decl for StructMarker {}
// impl Elem for StructMarker {}
impl Decl for Conjunction {}
impl Elem for Conjunction {}
impl Decl for Disjunction {}
impl Elem for Disjunction {}
impl Decl for BoundValue {}
impl Elem for BoundValue {}
impl Decl for BuiltinValidator {}
impl Elem for BuiltinValidator {}
impl Decl for NodeLink {}
impl Elem for NodeLink {}
impl Decl for FieldReference {}
impl Elem for FieldReference {}
impl Decl for ValueReference {}
impl Elem for ValueReference {}
impl Decl for LabelReference {}
impl Elem for LabelReference {}
impl Decl for DynamicReference {}
impl Elem for DynamicReference {}
impl Decl for ImportReference {}
impl Elem for ImportReference {}
impl Decl for LetReference {}
impl Elem for LetReference {}
impl Decl for SelectorExpr {}
impl Elem for SelectorExpr {}
impl Decl for IndexExpr {}
impl Elem for IndexExpr {}
impl Decl for SliceExpr {}
impl Elem for SliceExpr {}
impl Decl for Interpolation {}
impl Elem for Interpolation {}
impl Decl for UnaryExpr {}
impl Elem for UnaryExpr {}
impl Decl for BinaryExpr {}
impl Elem for BinaryExpr {}
impl Decl for CallExpr {}
impl Elem for CallExpr {}
impl Decl for Builtin {}
impl Elem for Builtin {}
impl Decl for DisjunctionExpr {}
impl Elem for DisjunctionExpr {}

impl Decl for Comprehension {}
impl Elem for Comprehension {}
