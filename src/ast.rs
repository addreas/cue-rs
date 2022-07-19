pub trait Node {}

#[derive(Debug, PartialEq, Clone)]
pub enum Operator {
    Add,      // +
    Subtract, // -
    Multiply, // *
    Divide,   // /
    Power,    // ^

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
}

#[derive(Debug, PartialEq, Clone)]
pub struct Comment<'a> {
    pub text: &'a str,
}

#[derive(Debug, PartialEq, Clone)]
pub struct CommentGroup<'a> {
    pub doc: bool,
    pub line: bool,

    pub position: u8,
    pub comments: Vec<Comment<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Ident<'a> {
    pub name: &'a str,
    // scope: Node,
    // node: Node,
}

impl<'a> Ident<'a> {
    pub fn new(name: &'a str) -> Self {
        Self { name }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Ellipsis<'a> {
    pub inner: Expr<'a>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum BasicLit<'a> {
    Bottom,
    Null,
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),
    Bytes(&'a str),
    // Duration,
}

// #[derive(Debug, PartialEq, Clone)]
// pub struct BasicLit<'a> {
//     pub kind: BasicLitType<'a>,
//     pub value: String,
// }

#[derive(Debug, PartialEq, Clone)]
pub struct ImportSpec<'a> {
    pub alias: Ident<'a>,
    pub path: BasicLit<'a>,
    pub package: Option<Ident<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct File<'a> {
    pub name: &'a str,
    pub package: Option<Ident<'a>>,
    pub declarations: Vec<Declaration<'a>>,
    pub imports: Vec<Declaration<'a>>,
    pub attributes: Vec<Attribute<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Attribute<'a> {
    pub text: &'a str,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Alias<'a> {
    pub ident: Ident<'a>,
    pub expr: Expr<'a>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Field<'a> {
    pub label: Label<'a>,
    pub value: Expr<'a>,
    pub attributes: Option<Vec<Attribute<'a>>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Comprehension<'a> {
    pub clauses: Vec<Clause<'a>>,
    pub expr: Expr<'a>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct LetClause<'a> {
    pub alias: Ident<'a>,
    pub value: Expr<'a>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ListLit<'a> {
    pub elements: Vec<Expr<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct StructLit<'a> {
    pub elements: Vec<Declaration<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Interpolation<'a> {
    pub is_bytes: bool,
    pub elements: Vec<Expr<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Label<'a> {
    Ident(Ident<'a>),
    Alias(Alias<'a>),
    Basic(Interpolation<'a>),
    Paren(Expr<'a>),
    Bracket(Expr<'a>),
}

impl<'a> Label<'a> {
    pub fn ident(x: Ident<'a>) -> Self {
        Self::Ident(x)
    }
    pub fn alias(ident: Ident<'a>, expr: Expr<'a>) -> Self {
        Self::Alias(Alias { ident, expr })
    }
    pub fn basic(elements: Vec<Expr<'a>>) -> Self {
        Self::Basic(Interpolation {
            is_bytes: false,
            elements,
        })
    }
    pub fn paren(x: Expr<'a>) -> Self {
        Self::Paren(x)
    }
    pub fn bracket(x: Expr<'a>) -> Self {
        Self::Bracket(x)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Declaration<'a> {
    Bad,
    CommentGroup(CommentGroup<'a>),
    Attribute(Attribute<'a>),
    Field(Field<'a>),
    Alias(Alias<'a>),
    Comprehension(Comprehension<'a>),
    Ellipsis(Ellipsis<'a>),
    LetClause(LetClause<'a>),
    ImportDecl(Vec<ImportSpec<'a>>),
    Embedding(Expr<'a>),
}

impl<'a> Declaration<'a> {
    pub fn bad() -> Self {
        Self::Bad
    }
    pub fn comment_group(cg: CommentGroup<'a>) -> Self {
        Self::CommentGroup(cg)
    }
    pub fn attribute(text: &'a str) -> Self {
        Self::Attribute(Attribute { text })
    }
    pub fn field(label: Label<'a>, value: Expr<'a>) -> Self {
        Self::Field(Field {
            label,
            value,
            attributes: None,
        })
    }
    pub fn alias(ident: Ident<'a>, expr: Expr<'a>) -> Self {
        Self::Alias(Alias { ident, expr })
    }
    pub fn comprehension(clauses: Vec<Clause<'a>>, expr: Expr<'a>) -> Self {
        Self::Comprehension(Comprehension { clauses, expr })
    }
    pub fn ellipsis(inner: Expr<'a>) -> Self {
        Self::Ellipsis(Ellipsis { inner })
    }
    pub fn let_clause(alias: Ident<'a>, value: Expr<'a>) -> Self {
        Self::LetClause(LetClause { alias, value })
    }
    pub fn import_decl(x: Vec<ImportSpec<'a>>) -> Self {
        Self::ImportDecl(x)
    }
    pub fn embedding(x: Expr<'a>) -> Self {
        Self::Embedding(x)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Clause<'a> {
    If(Expr<'a>),
    For {
        key: Option<Ident<'a>>,
        value: Ident<'a>,
        source: Expr<'a>,
    },
    Let(LetClause<'a>),
}

impl<'a> Clause<'a> {
    pub fn if_clause(e: Expr<'a>) -> Self {
        Self::If(e)
    }
    pub fn for_clause(key: Option<Ident<'a>>, value: Ident<'a>, source: Expr<'a>) -> Self {
        Self::For { key, value, source }
    }
    pub fn let_clause(alias: Ident<'a>, value: Expr<'a>) -> Self {
        Self::Let(LetClause { alias, value })
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr<'a> {
    Bad,
    Alias(Box<Alias<'a>>),
    Comprehension(Box<Comprehension<'a>>),
    Ident(Ident<'a>),
    QualifiedIdent(Ident<'a>, Ident<'a>),
    BasicLit(BasicLit<'a>),
    Interpolation(Interpolation<'a>),
    Struct(StructLit<'a>),
    List(ListLit<'a>),
    Ellipsis(Box<Ellipsis<'a>>),
    UnaryExpr {
        op: Operator,
        child: Box<Expr<'a>>,
    },
    BinaryExpr {
        op: Operator,
        lhs: Box<Expr<'a>>,
        rhs: Box<Expr<'a>>,
    },
    Parens {
        inner: Box<Expr<'a>>,
    },
    Selector {
        source: Box<Expr<'a>>,
        field: Box<Label<'a>>,
    },
    Index {
        source: Box<Expr<'a>>,
        index: Box<Expr<'a>>,
    },
    Slice {
        source: Box<Expr<'a>>,
        low: Box<Expr<'a>>,
        high: Box<Expr<'a>>,
    },
    Call {
        source: Box<Expr<'a>>,
        args: Vec<Expr<'a>>,
    },
}

impl<'a> Expr<'a> {
    pub fn string(x: String) -> Self {
        Self::BasicLit(BasicLit::String(x))
    }
    pub fn string_interpolation(elements: Vec<Expr<'a>>) -> Self {
        Self::Interpolation(Interpolation {
            is_bytes: false,
            elements,
        })
    }
    pub fn bytes(x: &'a str) -> Self {
        Self::BasicLit(BasicLit::Bytes(x))
    }
    pub fn bytes_interpolation(elements: Vec<Expr<'a>>) -> Self {
        Self::Interpolation(Interpolation {
            is_bytes: true,
            elements,
        })
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
    pub fn alias(ident: Ident<'a>, expr: Expr<'a>) -> Self {
        Self::Alias(Box::new(Alias { ident, expr }))
    }
    pub fn comprehension(clauses: Vec<Clause<'a>>, expr: Expr<'a>) -> Self {
        Self::Comprehension(Box::new(Comprehension { clauses, expr }))
    }
    pub fn ident(name: &'a str) -> Self {
        Self::Ident(Ident { name })
    }
    pub fn qualified_ident(package: Ident<'a>, ident: Ident<'a>) -> Self {
        Self::QualifiedIdent(package, ident)
    }
    pub fn struct_lit(elements: Vec<Declaration<'a>>) -> Self {
        Self::Struct(StructLit { elements })
    }
    pub fn list(elements: Vec<Expr<'a>>) -> Self {
        Self::List(ListLit { elements })
    }
    pub fn ellipsis(inner: Expr<'a>) -> Self {
        Self::Ellipsis(Box::new(Ellipsis { inner }))
    }
}

pub fn new_str<'a>(s: String) -> Expr<'a> {
    Expr::BasicLit(BasicLit::String(s))
}
