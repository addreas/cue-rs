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
