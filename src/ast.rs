#[derive(Debug, PartialEq, Clone)]
pub enum Operator {
    Add,      // +
    Subtract, // -
    Multiply, // *
    Divide,   // /
    Power,    // ^

    And, // &
    Or,  // |

    LogicAnd, // &&
    LogicOr,  // ||

    Bind,    // =
    Equal,   // ==
    Less,    // <
    Greater, // >
    Not,     // !
    Arror,   // <-

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
    Str(&'a str),
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
    pub attributes: Vec<Attribute<'a>>,
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

#[derive(Debug, PartialEq, Clone)]
pub enum Declaration<'a> {
    CommentGroup(CommentGroup<'a>),
    Attribute(Attribute<'a>),
    Field(Field<'a>),
    Alias(Alias<'a>),
    Comprehension(Comprehension<'a>),
    Ellipsis(Ellipsis<'a>),
    LetClause(LetClause<'a>),
    BadDecl,
    ImportDecl(Vec<ImportSpec<'a>>),
    Embedding(Expr<'a>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr<'a> {
    Alias(Box<Alias<'a>>),
    Comprehension(Box<Comprehension<'a>>),
    Bad,
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

// pub fn parse(name: &str, source: &str) -> std::result::Result<File, pest::error::Error<Rule>> {
//     let pairs: pest::iterators::Pairs<Rule> = CUEParser::parse(Rule::SourceFile, source)?;

//     let mut attributes = vec![];
//     let mut package: Option<String> = None;
//     let mut imports = vec![];
//     let mut declarations = vec![];
//     let unresolved = vec![];

//     for pair in pairs {
//         match pair.as_rule() {
//             Rule::attribute => attributes.push(Attribute {
//                 text: "".to_string(),
//             }),
//             Rule::PackageClause => package = Some(pair.into_inner().next().unwrap().to_string()),
//             Rule::ImportDecl => imports.push(ImportSpec {
//                 name: Ident {
//                     name: "".to_string(),
//                 },
//                 path: BasicLit {
//                     kind: BasicLitType::String,
//                     value: "encoding/hex".to_string(),
//                 },
//             }),
//             Rule::Declaration => declarations.push(Declaration::BadDecl),
//             _ => unreachable!(),
//         }
//     }
//     Ok(File {
//         name: String::from(name),
//         package,
//         declarations,
//         imports,
//         unresolved,
//     })
// }

// fn build_ast_from_expr(pair: pest::iterators::Pair<Rule>) -> Expr {
//     match pair.as_rule() {
//         Rule::Expression => build_ast_from_expr(pair.into_inner().next().unwrap()),
//         Rule::UnaryExpr => {
//             let mut pair = pair.into_inner();
//             let op = pair.next().unwrap();
//             let child = pair.next().unwrap();
//             let child = build_ast_from_term(child);
//             parse_unary_expr(op, child)
//         }
//         Rule::BinaryExpr => {
//             let mut pair = pair.into_inner();
//             let lhspair = pair.next().unwrap();
//             let mut lhs = build_ast_from_term(lhspair);
//             let mut op = pair.next().unwrap();
//             let rhspair = pair.next().unwrap();
//             let mut rhs = build_ast_from_term(rhspair);
//             let mut retval = parse_binary_expr(op, lhs, rhs);
//             loop {
//                 let pair_buf = pair.next();
//                 if pair_buf != None {
//                     op = pair_buf.unwrap();
//                     lhs = retval;
//                     rhs = build_ast_from_term(pair.next().unwrap());
//                     retval = parse_binary_expr(op, lhs, rhs);
//                 } else {
//                     return retval;
//                 }
//             }
//         }
//         unknown => panic!("Unknown expr: {:?}", unknown),
//     }
// }

// fn build_ast_from_term(pair: pest::iterators::Pair<Rule>) -> Node {
//     match pair.as_rule() {
//         Rule::Int => {
//             let istr = pair.as_str();
//             let (sign, istr) = match &istr[..1] {
//                 "-" => (-1, &istr[1..]),
//                 _ => (1, istr),
//             };
//             let int: i32 = istr.parse().unwrap();
//             Node::Int(sign * int)
//         }
//         Rule::Expr => build_ast_from_expr(pair),
//         unknown => panic!("Unknown term: {:?}", unknown),
//     }
// }

// fn parse_unary_expr(pair: pest::iterators::Pair<Rule>, child: Node) -> Node {
//     Node::UnaryExpr {
//         op: match pair.as_str() {
//             "+" => Operator::Add,
//             "-" => Operator::Subtract,
//             _ => unreachable!(),
//         },
//         child: Box::new(child),
//     }
// }

// fn parse_binary_expr(pair: pest::iterators::Pair<Rule>, lhs: Node, rhs: Node) -> Node {
//     Node::BinaryExpr {
//         op: match pair.as_str() {
//             "+" => Operator::Add,
//             "-" => Operator::Subtract,
//             _ => unreachable!(),
//         },
//         lhs: Box::new(lhs),
//         rhs: Box::new(rhs),
//     }
// }

pub fn new_str<'a>(s: String) -> Expr<'a> {
    Expr::BasicLit(BasicLit::String(s))
}
