use std::{fmt::Debug, rc::Rc};

use regex::Regex;

use super::op::RelOp;

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Top,

    Disjunction(Vec<Value>),
    Conjunction(Vec<Value>),

    Struct(Vec<Field>),
    List(Vec<Value>),

    String(Basic<RelOp, Rc<str>>),
    Bytes(Basic<RelOp, Rc<str>>),

    Float(Basic<RelOp, f64>),
    Int(Basic<RelOp, i64>),

    Bool(Option<bool>),

    Null,

    Bottom,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Basic<Op, T> {
    Type,
    Relation(Op, T),
    Value(T)
}

#[derive(Debug, PartialEq, Clone)]
pub struct Field {
    label: String,
    optional: bool,
    value: Value,
}

impl Value {
    // infimum, greatest lower bound, unification (&)
    pub fn meet(self: Self, other: Self) -> Self {
        match (self, other) {
            (a, b) if a == b => a,

            (Self::Top, b) => b,
            (a, Self::Top) => a,

            (Self::Bottom, _) => Self::Bottom,
            (_, Self::Bottom) => Self::Bottom,

            (Self::Bool(None), Self::Bool(b)) => Self::Bool(b),
            (Self::Bool(a), Self::Bool(None)) => Self::Bool(a),

            (Self::Bool(Some(a)), Self::Bool(Some(b))) if a == b => Self::Bool(Some(a)),
            (Self::Bool(Some(a)), Self::Bool(Some(b))) if a != b => Self::Bottom,

            (Self::Int(lhs), Self::Int(rhs)) => Self::meet_basic(lhs, rhs, Self::Int, Self::rel_op_ord, Self::meet_rel_op_ord),
            (Self::Float(lhs), Self::Float(rhs)) => Self::meet_basic(lhs, rhs, Self::Float, Self::rel_op_ord, Self::meet_rel_op_ord),

            (Self::String(lhs), Self::String(rhs)) => Self::meet_basic(lhs, rhs, Self::String, |a, op, b| Self::rel_op_str(a, op, b), Self::meet_rel_op_str),
            (Self::Bytes(lhs), Self::Bytes(rhs)) => Self::meet_basic(lhs, rhs, Self::Bytes, |a, op, b| Self::rel_op_str(a, op, b), Self::meet_rel_op_str),

            (Self::Struct(lhs), Self::Struct(rhs)) => Self::meet_structs(lhs, rhs),
            (Self::List(lhs), Self::List(rhs)) => Self::meet_lists(lhs, rhs),

            _ => Self::Bottom, // probably some TODOs here
        }
    }

    // supremum, least upper bound, anti-unification (|)
    pub fn join(self: Self, other: Self) -> Self {
        match (self, other) {
            (a, b) if a == b => a,

            (Self::Top, _) => Self::Top,
            (_, Self::Top) => Self::Top,

            (Self::Bottom, b) => b,
            (a, Self::Bottom) => a,

            (Self::Bool(None), Self::Bool(_)) => Self::Bool(None),
            (Self::Bool(_), Self::Bool(None)) => Self::Bool(None),

            (Self::Bool(Some(a)), Self::Bool(Some(b))) if a == b => Self::Bool(Some(a)),
            (Self::Bool(Some(a)), Self::Bool(Some(b))) if a != b => Self::Bool(None),

            (Self::Int(lhs), Self::Int(rhs)) => Self::join_basic(lhs, rhs, Self::Int),
            (Self::Float(lhs), Self::Float(rhs)) => Self::join_basic(lhs, rhs, Self::Float),

            (Self::String(lhs), Self::String(rhs)) => Self::join_basic(lhs, rhs, Self::String),
            (Self::Bytes(lhs), Self::Bytes(rhs)) => Self::join_basic(lhs, rhs, Self::Bytes),

            (Self::Disjunction(a), b) => Self::extend_disjunction(a, b),
            (a, Self::Disjunction(b)) => Self::extend_disjunction(b, a),

            (a, b) => Self::Disjunction(vec![a, b]),
        }
    }


    fn meet_basic<Op: Copy, T: PartialEq>(
        lhs: Basic<Op, T>,
        rhs: Basic<Op, T>,
        construct: fn(Basic<Op, T>) -> Self,
        rel_op: fn(&T, Op, &T) -> bool,
        meet_rel_op: fn(Op, &T, Op, &T, fn(Basic<Op, T>) -> Self) -> Self,
    ) -> Self {
        match (&lhs, &rhs) {
            (Basic::Type, _) => construct(rhs),
            (_, Basic::Type) => construct(lhs),

            (Basic::Value(a), Basic::Value(b)) if a == b => construct(lhs),
            (Basic::Value(_), Basic::Value(_)) => Self::Bottom,

            (Basic::Value(a), Basic::Relation(op, b)) if rel_op(a, *op, b) => construct(lhs),
            (Basic::Value(_), Basic::Relation(_, _)) => Self::Bottom,
            (Basic::Relation(op, a), Basic::Value(b)) if rel_op(a, *op, b) => construct(rhs),
            (Basic::Relation(_, _), Basic::Value(_)) => Self::Bottom,

            (Basic::Relation(opa, a), Basic::Relation(opb, b)) => meet_rel_op(*opa, a, *opb, b, construct),
        }
    }

    fn join_basic<Op: Copy, T: PartialEq>(
        lhs: Basic<Op, T>,
        rhs: Basic<Op, T>,
        construct: fn(Basic<Op, T>) -> Self,
    ) -> Self {
        match (&lhs, &rhs) {
            (Basic::Type, _) => construct(lhs),
            (_, Basic::Type) => construct(rhs),

            (Basic::Value(a), Basic::Value(b)) if a == b => construct(lhs),
            (_, _) => Self::Disjunction(vec![construct(lhs), construct(rhs)]),
        }
    }

    fn meet_rel_op_str<T: Eq + Clone>(opa: RelOp, a: &T, opb: RelOp, b: &T, construct: fn(Basic<RelOp, T>) -> Self) -> Self {
        match (opa, opb) {
            (RelOp::GreaterEqual, RelOp::LessEqual) if a == b => construct(Basic::Value(a.clone())),
            (RelOp::LessEqual, RelOp::GreaterEqual) if a == b => construct(Basic::Value(a.clone())),
            (RelOp::NotEqual, RelOp::NotEqual)      if a == b => construct(Basic::Relation(opa, a.clone())),
            (RelOp::Match, RelOp::Match)            if a == b => construct(Basic::Relation(opa, a.clone())),
            (RelOp::NotMatch, RelOp::NotMatch)      if a == b => construct(Basic::Relation(opa, a.clone())),
            (RelOp::Match, RelOp::NotMatch)         if a == b => Self::Bottom,
            (RelOp::NotMatch, RelOp::Match)         if a == b => Self::Bottom,

            // ...

            _ => Value::Conjunction(vec![construct(Basic::Relation(opa, a.clone())), construct(Basic::Relation(opb, b.clone()))]),
        }
     }

    fn meet_rel_op_ord<T: PartialEq + PartialOrd + Copy>(opa: RelOp, a: &T, opb: RelOp, b: &T, construct: fn(Basic<RelOp, T>) -> Self) -> Self {
        // TODO: could this be implemented by applying rel_op_ord in some clever way?
        match (opa, opb) {
            (RelOp::GreaterEqual, RelOp::LessEqual) if a == b => construct(Basic::Value(*a)),
            (RelOp::LessEqual, RelOp::GreaterEqual) if a == b => construct(Basic::Value(*a)),


            (RelOp::GreaterThan, RelOp::GreaterThan)  if a >= b => construct(Basic::Relation(opa, *a)),
            (RelOp::GreaterThan, RelOp::GreaterThan)  if a <  b => construct(Basic::Relation(opb, *b)),
            (RelOp::GreaterThan, RelOp::GreaterEqual) if a >= b => construct(Basic::Relation(opa, *a)),
            (RelOp::GreaterThan, RelOp::GreaterEqual) if a <  b => construct(Basic::Relation(opb, *b)),
            (RelOp::GreaterThan, RelOp::LessThan)     if a >= b => Self::Bottom,
            (RelOp::GreaterThan, RelOp::LessEqual)    if a >= b => Self::Bottom,
            (RelOp::GreaterThan, RelOp::NotEqual)     if a <= b => construct(Basic::Relation(opa, *a)),

            (RelOp::GreaterEqual, RelOp::GreaterEqual) if a >= b => construct(Basic::Relation(opa, *a)),
            (RelOp::GreaterEqual, RelOp::GreaterEqual) if a <  b => construct(Basic::Relation(opb, *b)),
            (RelOp::GreaterEqual, RelOp::GreaterThan)  if a >  b => construct(Basic::Relation(opa, *a)),
            (RelOp::GreaterEqual, RelOp::GreaterThan)  if a <= b => construct(Basic::Relation(opb, *b)),
            (RelOp::GreaterEqual, RelOp::LessEqual)    if a >= b => Self::Bottom,
            (RelOp::GreaterEqual, RelOp::LessThan)     if a >= b => Self::Bottom,
            (RelOp::GreaterEqual, RelOp::NotEqual)     if a <  b => construct(Basic::Relation(opa, *a)),


            (RelOp::LessThan, RelOp::LessThan)     if a <= b => construct(Basic::Relation(opa, *a)),
            (RelOp::LessThan, RelOp::LessThan)     if a >  b => construct(Basic::Relation(opb, *b)),
            (RelOp::LessThan, RelOp::LessEqual)    if a <= b => construct(Basic::Relation(opa, *a)),
            (RelOp::LessThan, RelOp::LessEqual)    if a >  b => construct(Basic::Relation(opb, *b)),
            (RelOp::LessThan, RelOp::GreaterThan)  if a <= b => Self::Bottom,
            (RelOp::LessThan, RelOp::GreaterEqual) if a <= b => Self::Bottom,
            (RelOp::LessThan, RelOp::NotEqual)     if a >= b => construct(Basic::Relation(opa, *a)),

            (RelOp::LessEqual, RelOp::LessEqual)    if a <= b => construct(Basic::Relation(opa, *a)),
            (RelOp::LessEqual, RelOp::LessEqual)    if a >  b => construct(Basic::Relation(opb, *b)),
            (RelOp::LessEqual, RelOp::LessThan)     if a <  b => construct(Basic::Relation(opa, *a)),
            (RelOp::LessEqual, RelOp::LessThan)     if a >= b => construct(Basic::Relation(opb, *b)),
            (RelOp::LessEqual, RelOp::GreaterThan)  if a <= b => Self::Bottom,
            (RelOp::LessEqual, RelOp::GreaterEqual) if a <= b => Self::Bottom,
            (RelOp::LessEqual, RelOp::NotEqual)     if a >  b => construct(Basic::Relation(opa, *a)),


            (RelOp::NotEqual, RelOp::NotEqual)     if a == b => construct(Basic::Relation(opb, *b)),
            (RelOp::NotEqual, RelOp::GreaterThan)  if a <= b => construct(Basic::Relation(opb, *b)),
            (RelOp::NotEqual, RelOp::GreaterEqual) if a <  b => construct(Basic::Relation(opb, *b)),
            (RelOp::NotEqual, RelOp::LessThan)     if a >= b => construct(Basic::Relation(opb, *b)),
            (RelOp::NotEqual, RelOp::LessEqual)    if a >  b => construct(Basic::Relation(opb, *b)),

            _ => Value::Conjunction(vec![construct(Basic::Relation(opa, *a)), construct(Basic::Relation(opb, *b))]),
        }
    }

    fn rel_op_str(lhs: &str, op: RelOp, rhs: &str) -> bool {
        match op {
            RelOp::NotEqual => lhs != rhs,

            RelOp::GreaterEqual => lhs >= rhs,
            RelOp::GreaterThan => lhs > rhs,
            RelOp::LessEqual => lhs <= rhs,
            RelOp::LessThan => lhs < rhs,

            RelOp::Match => Regex::new(rhs).unwrap().is_match(lhs),
            RelOp::NotMatch => !Regex::new(rhs).unwrap().is_match(lhs),
        }
    }

    fn rel_op_ord<T: PartialEq + PartialOrd>(lhs: &T, op: RelOp, rhs: &T) -> bool {
        match op {
            RelOp::NotEqual => lhs != rhs,

            RelOp::GreaterEqual => lhs >= rhs,
            RelOp::GreaterThan => lhs > rhs,
            RelOp::LessEqual => lhs <= rhs,
            RelOp::LessThan => lhs < rhs,

            _ => false,
        }
    }

    fn meet_structs(lhs: Vec<Field>, rhs: Vec<Field>) -> Value {
        Value::Bottom
    }

    fn meet_lists(lhs: Vec<Value>, rhs: Vec<Value>) -> Value {
        if lhs.len() != rhs.len() {
            Value::Bottom
        } else {
            Value::List(
                lhs.into_iter()
                    .zip(rhs.into_iter())
                    .map(|(l, r)| l.meet(r))
                    .collect(),
            )
        }
    }

    fn extend_disjunction(mut existing: Vec<Value>, extension: Value) -> Value {
        for val in existing.iter() {
            if extension.clone() == *val {
                return Self::Disjunction(existing)
            }
        }

        existing.push(extension);
        Self::Disjunction(existing)
    }
}

#[test]
fn test_basic_meets() {
    assert_eq!(
        Value::Top.meet(Value::Bottom),
        Value::Bottom,
        "_ & _|_ == _|_"
    );

    assert_eq!(
        Value::Top.meet(Value::Null),
        Value::Null,
        "_ & null == null"
    );

    assert_eq!(
        Value::Top.meet(Value::Bool(None)),
        Value::Bool(None),
        "_ & bool == bool"
    );
    assert_eq!(
        Value::Top.meet(Value::Bool(Some(true))),
        Value::Bool(Some(true)),
        "_ & true == true"
    );
    assert_eq!(
        Value::Bool(None).meet(Value::Bool(Some(true))),
        Value::Bool(Some(true)),
        "bool & true == true"
    );

    assert_eq!(
        Value::Int(Basic::Type)
            .meet(Value::Int(Basic::Relation(RelOp::GreaterThan, 3))),
        Value::Int(Basic::Relation(RelOp::GreaterThan, 3)),
        "int & >3 == >3"
    );
}

#[allow(unused_macros)]
macro_rules! int_val {
    (_) => { Value::Top };
    (_|_) => { Value::Bottom };
    (int) => { Value::Int(Basic::Type) };
    (   $a:literal) => { Value::Int(Basic::Value($a)) };
    (>  $a:literal) => { Value::Int(Basic::Relation(RelOp::GreaterThan, $a)) };
    (>= $a:literal) => { Value::Int(Basic::Relation(RelOp::GreaterEqual, $a)) };
    (<  $a:literal) => { Value::Int(Basic::Relation(RelOp::LessThan, $a)) };
    (<= $a:literal) => { Value::Int(Basic::Relation(RelOp::LessEqual, $a)) };
    (!= $a:literal) => { Value::Int(Basic::Relation(RelOp::NotEqual, $a)) };
    (($($a:tt)+) & ($($b:tt)+)) => { Value::Conjunction(vec![int_val!($($a)+), int_val!($($b)+)]) };
    (($($a:tt)+) | ($($b:tt)+)) => { Value::Disjunction(vec![int_val!($($a)+), int_val!($($b)+)]) };
}

#[allow(unused_macros)]
macro_rules! assert_int {
    (($($a:tt)+) & ($($b:tt)+) == ($($c:tt)+)) => {
        let a = int_val!($($a)+);
        let b = int_val!($($b)+);
        let c = int_val!($($c)+);
        assert_eq!(
            a.meet(b),
            c,
            "expect that {} & {} == {}",
            stringify!($($a)+),
            stringify!($($b)+),
            stringify!($($c)+),
        )
    };
    (($($a:tt)+) | ($($b:tt)+) == ($($c:tt)+)) => {
        let a = int_val!($($a)+);
        let b = int_val!($($b)+);
        let c = int_val!($($c)+);
        assert_eq!(
            a.join(b),
            c,
            "expect that {} | {} == {}",
            stringify!($($a)+),
            stringify!($($b)+),
            stringify!($($c)+),
        )
    };
}
#[test]
fn test_int_infimum() {
    assert_int!((int) & (int) == (int));
    assert_int!((int) & (1) == (1));
    assert_int!((int) & (>1) == (>1));

    assert_int!((0) & (>1) == (_|_));
    assert_int!((1) & (>1) == (_|_));
    assert_int!((2) & (>1) == (2));

    assert_int!((0) & (<1) == (0));
    assert_int!((1) & (<1) == (_|_));
    assert_int!((2) & (<1) == (_|_));

    assert_int!((0) & (>=1) == (_|_));
    assert_int!((1) & (>=1) == (1));
    assert_int!((2) & (>=1) == (2));

    assert_int!((0) & (<=1) == (0));
    assert_int!((1) & (<=1) == (1));
    assert_int!((2) & (<=1) == (_|_));

    assert_int!((0) & (!=1) == (0));
    assert_int!((1) & (!=1) == (_|_));
    assert_int!((2) & (!=1) == (2));

    assert_int!((>10) & (>1) == (>10));
    assert_int!((>10) & (>10) == (>10));
    assert_int!((>10) & (>100) == (>100));

    assert_int!((>10) & (>=1) == (>10));
    assert_int!((>10) & (>=10) == (>10));
    assert_int!((>10) & (>=100) == (>=100));

    assert_int!((>10) & (!=1) == ((>10) & (!=1)));
    assert_int!((>10) & (!=10) == (>10));
    assert_int!((>10) & (!=100) == (>10));

    assert_int!((>10) & (<1) == (_|_));
    assert_int!((>10) & (<10) == (_|_));
    assert_int!((>10) & (<100) == ((>10) & (<100)));

    assert_int!((>10) & (<=1) == (_|_));
    assert_int!((>10) & (<=10) == (_|_));
    assert_int!((>10) & (<=100) == ((>10) & (<=100)));


    assert_int!((>=10) & (>=1) == (>=10));
    assert_int!((>=10) & (>=10) == (>=10));
    assert_int!((>=10) & (>=100) == (>=100));

    assert_int!((>=10) & (>1) == (>=10));
    assert_int!((>=10) & (>10) == (>10));
    assert_int!((>=10) & (>100) == (>100));

    assert_int!((>=10) & (!=1) == ((>=10) & (!=1)));
    assert_int!((>=10) & (!=10) == ((>=10) & (!=10)));
    assert_int!((>=10) & (!=100) == (>=10));

    assert_int!((>=10) & (<1) == (_|_));
    assert_int!((>=10) & (<10) == (_|_));
    assert_int!((>=10) & (<100) == ((>=10) & (<100)));

    assert_int!((>=10) & (<=1) == (_|_));
    assert_int!((>=10) & (<=10) == (10));
    assert_int!((>=10) & (<=100) == ((>=10) & (<=100)));


    assert_int!((<10) & (<1) == (<1));
    assert_int!((<10) & (<10) == (<10));
    assert_int!((<10) & (<100) == (<10));

    assert_int!((<10) & (<=1) == (<=1));
    assert_int!((<10) & (<=10) == (<10));
    assert_int!((<10) & (<=100) == (<10));

    assert_int!((<10) & (!=1) == (<10));
    assert_int!((<10) & (!=10) == (<10));
    assert_int!((<10) & (!=100) == ((<10) & (!=100)));

    assert_int!((<10) & (>1) == ((<10) & (>1)));
    assert_int!((<10) & (>10) == (_|_));
    assert_int!((<10) & (>100) == (_|_));

    assert_int!((<10) & (>=1) == ((<10) & (>=1)));
    assert_int!((<10) & (>=10) == (_|_));
    assert_int!((<10) & (>=100) == (_|_));


    assert_int!((<=10) & (<=1) == (<=1));
    assert_int!((<=10) & (<=10) == (<=10));
    assert_int!((<=10) & (<=100) == (<=10));

    assert_int!((<=10) & (<1) == (<1));
    assert_int!((<=10) & (<10) == (<10));
    assert_int!((<=10) & (<100) == (<=10));

    assert_int!((<=10) & (!=1) == (<=10));
    assert_int!((<=10) & (!=10) == ((<=10) & (!=10)));
    assert_int!((<=10) & (!=100) == ((<=10) & (!=100)));

    assert_int!((<=10) & (>1) == ((<=10) & (>1)));
    assert_int!((<=10) & (>10) == (_|_));
    assert_int!((<=10) & (>100) == (_|_));

    assert_int!((<=10) & (>=1) == ((<=10) & (>=1)));
    assert_int!((<=10) & (>=10) == (10));
    assert_int!((<=10) & (>=100) == (_|_));


    assert_int!((!=10) & (>1) == ((!=10) & (>1)));
    assert_int!((!=10) & (>10) == (>10));
    assert_int!((!=10) & (>100) == (>100));

    assert_int!((!=10) & (>=1) == ((!=10) & (>=1)));
    assert_int!((!=10) & (>=10) == ((!=10) & (>=10)));
    assert_int!((!=10) & (>=100) == (>=100));

    assert_int!((!=10) & (<1) == (<1));
    assert_int!((!=10) & (<10) == (<10));
    assert_int!((!=10) & (<100) == ((!=10) & (<100)));

    assert_int!((!=10) & (<=1) == (<=1));
    assert_int!((!=10) & (<=10) == ((!=10) & (<=10)));
    assert_int!((!=10) & (<=100) == ((!=10) & (<=100)));
}

#[test]
fn test_int_supremum() {
    assert_int!((int) | (int) == (int));
    assert_int!((int) | (1) == (int));
    assert_int!((int) | (>1) == (int));

    assert_int!((0) | (>1) == ((0) | (>1)));
    assert_int!((1) | (>1) == ((1) | (>1)));
    assert_int!((2) | (>1) == (>1));

    assert_int!((0) | (<1) == (<1));
    assert_int!((1) | (<1) == ((1) | (<1)));
    assert_int!((2) | (<1) == ((2) | (<1)));

    assert_int!((0) | (>=1) == ((0) | (>=1)));
    assert_int!((1) | (>=1) == (>=1));
    assert_int!((2) | (>=1) == (>=1));

    assert_int!((0) | (<=1) == (<=1));
    assert_int!((1) | (<=1) == (>=1));
    assert_int!((2) | (<=1) == ((2) | (<=1)));

    assert_int!((0) | (!=1) == (!=1));
    assert_int!((1) | (!=1) == (int));
    assert_int!((2) | (!=1) == (!=1));

    assert_int!((>10) | (>1) == (>1));
    assert_int!((>10) | (>10) == (>10));
    assert_int!((>10) | (>100) == (>10));

    assert_int!((>10) | (>=1) == (>=1));
    assert_int!((>10) | (>=10) == (>=10));
    assert_int!((>10) | (>=100) == (>10));

    assert_int!((>10) | (!=1) == (!=1));
    assert_int!((>10) | (!=10) == (!=10));
    assert_int!((>10) | (!=100) == (int));

    assert_int!((>10) | (<1) == ((>10) | (<1)));
    assert_int!((>10) | (<10) == (!=10)); // not true for floats
    assert_int!((>10) | (<100) == (int));

    assert_int!((>10) | (<=1) == ((>10) | (<=1)));
    assert_int!((>10) | (<=10) == (int));
    assert_int!((>10) | (<=100) == (int));

    assert_int!((>=10) | (>=1) == (>=1));
    assert_int!((>=10) | (>=10) == (>=10));
    assert_int!((>=10) | (>=100) == (>=10));

    assert_int!((>=10) | (>1) == (>1));
    assert_int!((>=10) | (>10) == (>=10));
    assert_int!((>=10) | (>100) == (>=10));

    assert_int!((>=10) | (!=1) == (!=1));
    assert_int!((>=10) | (!=10) == (int));
    assert_int!((>=10) | (!=100) == (int));

    assert_int!((>=10) | (<1) == ((>=10) | (<1)));
    assert_int!((>=10) | (<10) == (int));
    assert_int!((>=10) | (<100) == (int));

    assert_int!((>=10) | (<=1) == ((>=10) | (<=1)));
    assert_int!((>=10) | (<=10) == (int));
    assert_int!((>=10) | (<=100) == (int));

    assert_int!((<10) | (<1) == (<10));
    assert_int!((<10) | (<10) == (<10));
    assert_int!((<10) | (<100) == (<100));

    assert_int!((<10) | (<=1) == (<10));
    assert_int!((<10) | (<=10) == (<=10));
    assert_int!((<10) | (<=100) == (<=100));

    assert_int!((<10) | (!=1) == (int));
    assert_int!((<10) | (!=10) == (!=10));
    assert_int!((<10) | (!=100) == (!=100));

    assert_int!((<10) | (>1) == (int));
    assert_int!((<10) | (>10) == (!=10)); // not true for floats
    assert_int!((<10) | (>100) == ((<10) | (>100)));

    assert_int!((<10) | (>=1) == (int));
    assert_int!((<10) | (>=10) == (int));
    assert_int!((<10) | (>=100) == ((<10) | (>=100)));

    assert_int!((<=10) | (<=1) == (<=10));
    assert_int!((<=10) | (<=10) == (<=10));
    assert_int!((<=10) | (<=100) == (<=100));

    assert_int!((<=10) | (<1) == (<=10));
    assert_int!((<=10) | (<10) == (<=10));
    assert_int!((<=10) | (<100) == (<100));

    assert_int!((<=10) | (!=1) == (int));
    assert_int!((<=10) | (!=10) == (int));
    assert_int!((<=10) | (!=100) == (!=100));

    assert_int!((<=10) | (>1) == (int));
    assert_int!((<=10) | (>10) == (int));
    assert_int!((<=10) | (>100) == ((<=10) | (>100)));

    assert_int!((<=10) | (>=1) == (int));
    assert_int!((<=10) | (>=10) == (int));
    assert_int!((<=10) | (>=100) == ((<=10) | (>=100)));

    assert_int!((!=10) | (>1) == (int));
    assert_int!((!=10) | (>10) == (!=10));
    assert_int!((!=10) | (>100) == ((!=10) | (>100)));

    assert_int!((!=10) | (>=1) == (int));
    assert_int!((!=10) | (>=10) == (int));
    assert_int!((!=10) | (>=100) == ((!=10) | (>=100)));

    assert_int!((!=10) | (<1) == ((!=10) | (<1)));
    assert_int!((!=10) | (<10) == (!=10));
    assert_int!((!=10) | (<100) == (int));

    assert_int!((!=10) | (<=1) == ((!=10) | (<=1)));
    assert_int!((!=10) | (<=10) == (int));
    assert_int!((!=10) | (<=100) == (int));
}