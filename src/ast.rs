use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Bottom,
    Alias(Box<Alias>),
    Comprehension(Box<Comprehension>),
    Ident(Ident),
    QualifiedIdent(Ident, Ident),
    Null,
    Bool(bool),
    Int(i64),
    Float(f64),
    String(Interpolation),
    Bytes(Interpolation),
    Struct(Rc<[Declaration]>),
    List(Rc<[Expr]>),
    Ellipsis(Box<Ellipsis>),
    UnaryExpr(Operator, Box<Expr>),
    BinaryExpr(Box<Expr>, Operator, Box<Expr>),
    Parens(Box<Expr>),
    Selector(Box<Expr>, Box<Selector>),
    Index(Box<Expr>, Box<Expr>),
    Slice(Box<Expr>, Option<Box<Expr>>, Option<Box<Expr>>),
    Call(Box<Expr>, Rc<[Expr]>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Declaration {
    Attribute(Attribute),
    Field(Field),
    Alias(Alias),
    Comprehension(Comprehension),
    Ellipsis(Ellipsis),
    LetClause(LetClause),
    Embedding(Expr),
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

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Ident {
    pub name: Rc<str>,
    pub kind: Option<IdentKind>,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum IdentKind {
    Hidden,
    Definition,
    HiddenDefinition,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Ellipsis {
    pub inner: Option<Expr>,
}


#[derive(Debug, PartialEq, Clone)]
pub enum Interpolation {
    Simple(Rc<str>),
    Interpolated(Rc<[Rc<str>]>, Rc<[Expr]>),
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
    pub declarations: Rc<[Declaration]>,
    pub imports: Rc<[Rc<[ImportSpec]>]>,
    pub attributes: Rc<[Attribute]>,
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
    pub attributes: Option<Rc<[Attribute]>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Comprehension {
    pub clauses: Rc<[Clause]>,
    pub expr: Expr,
}

#[derive(Debug, PartialEq, Clone)]
pub struct LetClause {
    pub alias: Ident,
    pub value: Expr,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Label {
    Alias(Ident, Box<Label>),
    Ident(Ident, Option<FieldConstraint>),
    String(Interpolation, Option<FieldConstraint>),
    Dynamic(Expr, Option<FieldConstraint>),
    Pattern(Expr),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Selector {
    Ident(Ident),
    String(Interpolation),
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum FieldConstraint {
    Optional,
    Required,
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

    // Bind,    // =
    Equal,   // ==
    Less,    // <
    Greater, // >
    Not,     // !
    // Arrow,   // <-
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

impl Label {
    pub fn ident(x: Ident) -> Self {
        Self::Ident(x, None)
    }
    pub fn alias(ident: Ident, expr: Label) -> Self {
        Self::Alias(ident, Box::new(expr))
    }
    pub fn string(s: Rc<str>) -> Self {
        Self::String(Interpolation::Simple(s), None)
    }
    pub fn string_interpolation(strings: Rc<[Rc<str>]>, interpolations: Rc<[Expr]>) -> Self {
        Self::String(Interpolation::Interpolated(strings, interpolations), None)
    }
    pub fn dynamic(x: Expr) -> Self {
        Self::Dynamic(x, None)
    }
    pub fn pattern(x: Expr) -> Self {
        Self::Pattern(x)
    }
}

impl Declaration {
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
    pub fn comprehension(clauses: Rc<[Clause]>, expr: Expr) -> Self {
        Self::Comprehension(Comprehension { clauses, expr })
    }
    pub fn ellipsis(inner: Option<Expr>) -> Self {
        Self::Ellipsis(Ellipsis { inner })
    }
    pub fn let_clause(alias: Ident, value: Expr) -> Self {
        Self::LetClause(LetClause { alias, value })
    }
    pub fn embedding(x: Expr) -> Self {
        Self::Embedding(x)
    }
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

impl Expr {
    pub fn basic_string(x: Rc<str>) -> Self {
        Self::String(Interpolation::Simple(x))
    }
    pub fn string_interpolation(strings: Rc<[Rc<str>]>, interpolations: Rc<[Expr]>) -> Self {
        Self::String(Interpolation::Interpolated(
            strings,
            interpolations,
        ))
    }
    pub fn basic_bytes(x: Rc<str>) -> Self {
        Self::Bytes(Interpolation::Simple(x))
    }
    pub fn bytes_interpolation(strings: Rc<[Rc<str>]>, interpolations: Rc<[Expr]>) -> Self {
        Self::Bytes(Interpolation::Interpolated(
            strings,
            interpolations,
        ))
    }
    pub fn int(x: i64) -> Self {
        Self::Int(x)
    }
    pub fn bool(x: bool) -> Self {
        Self::Bool(x)
    }
    pub fn null() -> Self {
        Self::Null
    }
    pub fn bottom() -> Self {
        Self::Bottom
    }
    pub fn alias(ident: Ident, expr: Expr) -> Self {
        Self::Alias(Box::new(Alias { ident, expr }))
    }
    pub fn comprehension(clauses: Rc<[Clause]>, expr: Expr) -> Self {
        Self::Comprehension(Box::new(Comprehension { clauses, expr }))
    }
    pub fn ident(name: Rc<str>) -> Self {
        Self::Ident(Ident::from(name.as_ref()))
    }
    pub fn qualified_ident(package: Ident, ident: Ident) -> Self {
        Self::QualifiedIdent(package, ident)
    }
    pub fn struct_lit(elements: Rc<[Declaration]>) -> Self {
        Self::Struct(elements)
    }
    pub fn list(elements: Rc<[Expr]>) -> Self {
        Self::List(elements)
    }
    pub fn ellipsis(inner: Option<Expr>) -> Self {
        Self::Ellipsis(Box::new(Ellipsis { inner }))
    }
    pub fn unary_expr<'a>(op: Operator, child: Expr) -> Self {
        Self::UnaryExpr(op, Box::new(child))
    }
    pub fn binary_expr(lhs: Expr, op: Operator, rhs: Expr) -> Self {
        Self::BinaryExpr(Box::new(lhs), op, Box::new(rhs))
    }
    pub fn parens(inner: Expr) -> Self {
        Self::Parens(Box::new(inner))
    }
    pub fn selector(source: Expr, field: Selector) -> Self {
        Self::Selector(Box::new(source), Box::new(field))
    }
    pub fn index(source: Expr, index: Expr) -> Self {
        Self::Index(Box::new(source), Box::new(index))
    }
    pub fn slice(source: Expr, low: Option<Expr>, high: Option<Expr>) -> Self {
        Self::Slice(
            Box::new(source),
            low.map(|l| Box::new(l)),
            high.map(|h| Box::new(h)),
        )
    }
    pub fn call(source: Expr, args: Rc<[Expr]>) -> Self {
        Self::Call(Box::new(source), args)
    }
}

impl From<&str> for Ident {
    fn from(name: &str) -> Self {
        let (kind, name) = match name {
            s if s.starts_with("_#") => (Some(IdentKind::HiddenDefinition), &s[2..]),
            s if s.starts_with("_") => (Some(IdentKind::Hidden), &s[1..]),
            s if s.starts_with("#") => (Some(IdentKind::Definition), &s[1..]),
            s => (None, s),
        };
        Self {
            name: name.into(),
            kind,
        }
    }
}

impl Ident {
    pub fn as_str(&self) -> Rc<str> {
        let kind = match self.kind {
            None => "",
            Some(IdentKind::Hidden) => "_",
            Some(IdentKind::Definition) => "#",
            Some(IdentKind::HiddenDefinition) => "_#",
        };

        format!("{kind}{}", self.name).into()
    }
}
