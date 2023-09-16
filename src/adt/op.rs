use std::fmt::Display;

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

impl Display for RelOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let op = match self {
            RelOp::GreaterThan => ">",
            RelOp::GreaterEqual => ">=",
            RelOp::LessThan => "<",
            RelOp::LessEqual => "<=",
            RelOp::NotEqual => "!=",
            RelOp::Match => "=~",
            RelOp::NotMatch => "!~",
        };

        write!(f, "{op}")
    }
}
