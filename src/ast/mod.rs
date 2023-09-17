use std::rc::Rc;

pub mod parser;

pub trait Node {
    fn source(&self) -> &pest::Span;
}

#[derive(Debug, PartialEq, Clone)]
pub enum Operator {
    Add,      // +
    Subtract, // -
    Multiply, // *
    Divide,   // /
    Power,    // ^

    Default, // *

    Conjunct, // &
    Disjunct, // |

    And, // &&
    Or,  // ||

    Bind,    // =
    Equal,   // ==
    Less,    // <
    Greater, // >
    Not,     // !
    Arrow,   // <-

    NotEqual,     // !=
    LessEqual,    // <=
    GreaterEqual, // >=

    Match,    // =~
    NotMatch, // !~

    Div,
    Mod,
    Quo,
    Rem,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Comment {
    pub text: Rc<str>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct CommentGroup {
    pub doc: bool,
    pub line: bool,

    pub position: u8,
    pub comments: Vec<Comment>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Ident {
    pub name: Rc<str>,
    // scope: Node,
    // node: Node,
}

impl Ident {
    pub fn from(name: &str) -> Self {
        Self {
            name: name.into(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Ellipsis {
    pub inner: Option<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum BasicLit {
    Bottom,
    Null,
    Bool(bool),
    Int(i64),
    Float(f64),
    String(Interpolation),
    Bytes(Interpolation),
    // Duration,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Interpolation {
    Simple(Rc<str>),
    Interpolated(Vec<Rc<str>>, Vec<Expr>)
}

#[derive(Debug, PartialEq, Clone)]
pub struct ImportSpec {
    pub alias: Option<Ident>,
    pub path: Rc<str>,
    pub package: Option<Ident>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct SourceFile {
    pub package: Option<Ident>,
    pub declarations: Vec<Declaration>,
    pub imports: Vec<Declaration>,
    pub attributes: Vec<Attribute>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Attribute {
    pub text: Rc<str>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Alias {
    pub ident: Ident,
    pub expr: Expr,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Field {
    pub label: Label,
    pub value: Expr,
    pub attributes: Option<Vec<Attribute>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Comprehension {
    pub clauses: Vec<Clause>,
    pub expr: Expr,
}

#[derive(Debug, PartialEq, Clone)]
pub struct LetClause {
    pub alias: Ident,
    pub value: Expr,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ListLit {
    pub elements: Vec<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct StructLit {
    pub elements: Vec<Declaration>,
}



#[derive(Debug, PartialEq, Clone)]
pub enum Label {
    // TODO: optionals
    Ident(Ident, Option<LabelModifier>),
    Alias(Ident, Box<Label>, Option<LabelModifier>), // todo: better representation of this?
    String(Interpolation, Option<LabelModifier>),
    Paren(Expr, Option<LabelModifier>),
    Bracket(Expr),
}

#[derive(Debug, PartialEq, Clone)]
pub enum LabelModifier {
    Optional,
    Required
}

impl Label {
    pub fn ident(x: Ident) -> Self {
        Self::Ident(x, None)
    }
    pub fn alias(ident: Ident, expr: Label) -> Self {
        Self::Alias(ident, Box::new(expr), None)
    }
    pub fn string(s: Rc<str>) -> Self {
        Self::String(Interpolation::Simple(s), None)
    }
    pub fn string_interpolation(strings: Vec<Rc<str>>, interpolations: Vec<Expr>) -> Self {
        Self::String(Interpolation::Interpolated(strings, interpolations), None)
    }
    pub fn paren(x: Expr) -> Self {
        Self::Paren(x, None)
    }
    pub fn bracket(x: Expr) -> Self {
        Self::Bracket(x)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Declaration {
    Bad,
    CommentGroup(CommentGroup),
    Attribute(Attribute),
    Field(Field),
    Alias(Alias),
    Comprehension(Comprehension),
    Ellipsis(Ellipsis),
    LetClause(LetClause),
    ImportDecl(Vec<ImportSpec>),
    Embedding(Expr),
}

impl Declaration {
    pub fn bad() -> Self {
        Self::Bad
    }
    pub fn comment_group(cg: CommentGroup) -> Self {
        Self::CommentGroup(cg)
    }
    pub fn attribute(text: Rc<str>) -> Self {
        Self::Attribute(Attribute { text })
    }
    pub fn field(label: Label, value: Expr) -> Self {
        Self::Field(Field {
            label,
            value,
            attributes: None,
        })
    }
    pub fn alias(ident: Ident, expr: Expr) -> Self {
        Self::Alias(Alias { ident, expr })
    }
    pub fn comprehension(clauses: Vec<Clause>, expr: Expr) -> Self {
        Self::Comprehension(Comprehension { clauses, expr })
    }
    pub fn ellipsis(inner: Option<Expr>) -> Self {
        Self::Ellipsis(Ellipsis { inner })
    }
    pub fn let_clause(alias: Ident, value: Expr) -> Self {
        Self::LetClause(LetClause { alias, value })
    }
    pub fn import_decl(x: Vec<ImportSpec>) -> Self {
        Self::ImportDecl(x)
    }
    pub fn embedding(x: Expr) -> Self {
        Self::Embedding(x)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Clause {
    If(Expr),
    For {
        key: Option<Ident>,
        value: Ident,
        source: Expr,
    },
    Let(LetClause),
}

impl Clause {
    pub fn if_clause(e: Expr) -> Self {
        Self::If(e)
    }
    pub fn for_clause(key: Option<Ident>, value: Ident, source: Expr) -> Self {
        Self::For { key, value, source }
    }
    pub fn let_clause(alias: Ident, value: Expr) -> Self {
        Self::Let(LetClause { alias, value })
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct UnaryExpr {
    op: Operator,
    child: Box<Expr>,
}
#[derive(Debug, PartialEq, Clone)]
pub struct BinaryExpr {
    lhs: Box<Expr>,
    op: Operator,
    rhs: Box<Expr>,
}
#[derive(Debug, PartialEq, Clone)]
pub struct Parens {
    inner: Box<Expr>,
}
#[derive(Debug, PartialEq, Clone)]
pub struct Selector {
    source: Box<Expr>,
    field: Box<Label>,
}
#[derive(Debug, PartialEq, Clone)]
pub struct Index {
    source: Box<Expr>,
    index: Box<Expr>,
}
#[derive(Debug, PartialEq, Clone)]
pub struct Slice {
    source: Box<Expr>,
    low: Option<Box<Expr>>,
    high: Option<Box<Expr>>,
}
#[derive(Debug, PartialEq, Clone)]
pub struct Call {
    source: Box<Expr>,
    args: Vec<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Bad,
    Alias(Box<Alias>),
    Comprehension(Box<Comprehension>),
    Ident(Ident),
    QualifiedIdent(Ident, Ident),
    BasicLit(BasicLit),
    Struct(StructLit),
    List(ListLit),
    Ellipsis(Box<Ellipsis>),
    UnaryExpr(UnaryExpr),
    BinaryExpr(BinaryExpr),
    Parens(Parens),
    Selector(Selector),
    Index(Index),
    Slice(Slice),
    Call(Call),
}

impl Expr {
    pub fn basic_string(x: Rc<str>) -> Self {
        Self::BasicLit(BasicLit::String(Interpolation::Simple(x)))
    }
    pub fn string_interpolation(strings: Vec<Rc<str>>, interpolations: Vec<Expr>) -> Self {
        Self::BasicLit(BasicLit::String(Interpolation::Interpolated(strings, interpolations)))
    }
    pub fn basic_bytes(x: Rc<str>) -> Self {
        Self::BasicLit(BasicLit::Bytes(Interpolation::Simple(x)))
    }
    pub fn bytes_interpolation(strings: Vec<Rc<str>>, interpolations: Vec<Expr>) -> Self {
        Self::BasicLit(BasicLit::Bytes(Interpolation::Interpolated(strings, interpolations)))
    }
    pub fn int(x: i64) -> Self {
        Self::BasicLit(BasicLit::Int(x))
    }
    pub fn bool(x: bool) -> Self {
        Self::BasicLit(BasicLit::Bool(x))
    }
    pub fn null() -> Self {
        Self::BasicLit(BasicLit::Null)
    }
    pub fn bottom() -> Self {
        Self::BasicLit(BasicLit::Bottom)
    }
    pub fn alias(ident: Ident, expr: Expr) -> Self {
        Self::Alias(Box::new(Alias { ident, expr }))
    }
    pub fn comprehension(clauses: Vec<Clause>, expr: Expr) -> Self {
        Self::Comprehension(Box::new(Comprehension { clauses, expr }))
    }
    pub fn ident(name: Rc<str>) -> Self {
        Self::Ident(Ident { name })
    }
    pub fn qualified_ident(package: Ident, ident: Ident) -> Self {
        Self::QualifiedIdent(package, ident)
    }
    pub fn struct_lit(elements: Vec<Declaration>) -> Self {
        Self::Struct(StructLit { elements })
    }
    pub fn list(elements: Vec<Expr>) -> Self {
        Self::List(ListLit { elements })
    }
    pub fn ellipsis(inner: Option<Expr>) -> Self {
        Self::Ellipsis(Box::new(Ellipsis { inner }))
    }
    pub fn unary_expr<'a>(op: Operator, child: Expr) -> Self {
        Self::UnaryExpr(UnaryExpr {
            op,
            child: Box::new(child),
        })
    }
    pub fn binary_expr(lhs: Expr, op: Operator, rhs: Expr) -> Self {
        Self::BinaryExpr(BinaryExpr {
            lhs: Box::new(lhs),
            op,
            rhs: Box::new(rhs),
        })
    }
    pub fn parens(inner: Expr) -> Self {
        Self::Parens(Parens {
            inner: Box::new(inner),
        })
    }
    pub fn selector(source: Expr, field: Label) -> Self {
        Self::Selector(Selector {
            source: Box::new(source),
            field: Box::new(field),
        })
    }
    pub fn index(source: Expr, index: Expr) -> Self {
        Self::Index(Index {
            source: Box::new(source),
            index: Box::new(index),
        })
    }
    pub fn slice(source: Expr, low: Option<Expr>, high: Option<Expr>) -> Self {
        Self::Slice(Slice {
            source: Box::new(source),
            low: low.map(|l| Box::new(l)),
            high: high.map(|h| Box::new(h)),
        })
    }
    pub fn call(source: Expr, args: Vec<Expr>) -> Self {
        Self::Call(Call {
            source: Box::new(source),
            args,
        })
    }
}

pub fn new_str(s: Rc<str>) -> Expr {
    Expr::BasicLit(BasicLit::String(Interpolation::Simple(s)))
}
