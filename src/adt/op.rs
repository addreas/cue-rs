#[derive(Debug, PartialEq)]
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

impl PartialOrd for Op {
    fn partial_cmp(&self, _: &Self) -> Option<std::cmp::Ordering> {
        return Some(std::cmp::Ordering::Equal);
    }
}
