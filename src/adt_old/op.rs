#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Op {
    Noop,

    And,
    Or,

    Selector,
    Index,
    Slice,
    Call,

    BoolAnd,
    BoolOr,

    Equal,
    Not,

    RelOp(RelOp),

    Add,
    Subtract,
    Multiply,
    FloatQuotient,
    IntQuotient,
    IntRemainder,
    IntDivide,
    IntModulo,

    Interpolation,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum RelOp {
    NotEqual,

    LessThan,
    LessEqual,

    GreaterThan,
    GreaterEqual,

    Match,
    NotMatch,
}
