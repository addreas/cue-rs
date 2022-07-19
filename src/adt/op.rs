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
    NotEqual,
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,

    Match,
    NotMatch,

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
