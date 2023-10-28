
type Innermediate = Rc<RefCell<Intermediate>>;

#[derive(Debug, PartialEq, Clone)]
pub enum Intermediate {
    Alias(ast::Ident, Innermediate),

    If(Innermediate, Innermediate),
    For(Box<ast::Label>, Innermediate, Innermediate),
    Let(ast::Ident, Innermediate),

    Ellipsis(Box<Option<Innermediate>>),
    UnaryExpr(ast::Operator, Innermediate),
    BinaryExpr(Innermediate, ast::Operator, Innermediate),

    Selector(Innermediate, Innermediate),
    Index(Innermediate, Innermediate),
    Slice(Innermediate, Option<Innermediate>, Option<Innermediate>),
    // Call(Function, Rc<[Innermediate]>),
    Struct(Rc<[(ast::Label, Innermediate)]>),
    List(Rc<[Innermediate]>),

    Conjunction(Rc<[Innermediate]>),

    Expr(MutableScope, ast::Expr),
    Reference(MutableScope, ast::Ident),

    Value(value::Value),
}
