use std::{fmt::Debug, rc::Rc};

use regex::Regex;

use super::op::RelOp;

type Basic<Op, T> = Option<(Option<Op>, T)>;

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
pub struct Field {
    label: String,
    optional: bool,
    value: Value,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BasicValue<T> {
    Type,
    Constraint(RelOp, T),
    Concrete(T),
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


fn rel_op_str(lhs: &str, op: RelOp, rhs: &str) -> bool {
    match op {
        RelOp::NotEqual => lhs != rhs,

        RelOp::GreaterEqual => lhs >= rhs,
        RelOp::GreaterThan => lhs > rhs,
        RelOp::LessEqual => lhs <= rhs,
        RelOp::LessThan => lhs < rhs,

        RelOp::Match => Regex::new(rhs).unwrap().is_match(lhs),
        RelOp::NotMatch => !Regex::new(rhs).unwrap().is_match(lhs),

        _ => false,
    }
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

            (Self::Int(lhs), Self::Int(rhs)) => Self::meet_basic(lhs, rhs, Self::Int, rel_op_ord, Self::meet_rel_op_ord),
            (Self::Float(lhs), Self::Float(rhs)) => Self::meet_basic(lhs, rhs, Self::Float, rel_op_ord, Self::meet_rel_op_ord),

            (Self::String(lhs), Self::String(rhs)) => Self::meet_basic(lhs, rhs, Self::String, |a, op, b| rel_op_str(a, op, b), Self::meet_rel_op_str),
            (Self::Bytes(lhs), Self::Bytes(rhs)) => Self::meet_basic(lhs, rhs, Self::Bytes, |a, op, b| rel_op_str(a, op, b), Self::meet_rel_op_str),

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
        merge_rel_op: fn(Op, &T, Op, &T, fn(Basic<Op, T>) -> Self) -> Self,
    ) -> Self {
        match (&lhs, &rhs) {
            (None, _) => construct(rhs),
            (_, None) => construct(lhs),

            (Some((None, a)), Some((None, b))) if a == b => construct(lhs),
            (Some((None, _)), Some((None, _))) => Self::Bottom,

            (Some((None, a)), Some((Some(op), b))) if rel_op(a, *op, b) => construct(lhs),
            (Some((None, _)), Some((Some(_), _))) => Self::Bottom,
            (Some((Some(op), a)), Some((None, b))) if rel_op(a, *op, b) => construct(rhs),
            (Some((Some(_), _)), Some((None, _))) => Self::Bottom,

            (Some((Some(opa), a)), Some((Some(opb), b))) => merge_rel_op(*opa, a, *opb, b, construct),
        }
    }

    fn join_basic<Op: Copy, T: PartialEq>(
        lhs: Basic<Op, T>,
        rhs: Basic<Op, T>,
        construct: fn(Basic<Op, T>) -> Self,
    ) -> Self {
        match (&lhs, &rhs) {
            (None, _) => construct(lhs),
            (_, None) => construct(rhs),

            (Some((None, a)), Some((None, b))) if a == b => construct(lhs),
            (_, _) => Self::Disjunction(vec![construct(lhs), construct(rhs)]),
        }
    }

    fn meet_rel_op_str<T: Eq + Clone>(opa: RelOp, a: &T, opb: RelOp, b: &T, construct: fn(Basic<RelOp, T>) -> Self) -> Self {
        match (opa, opb) {
            (RelOp::GreaterEqual, RelOp::LessEqual) if a == b => construct(Some((None, a.clone()))),
            (RelOp::LessEqual, RelOp::GreaterEqual) if a == b => construct(Some((None, a.clone()))),
            (RelOp::NotEqual, RelOp::NotEqual)      if a == b => construct(Some((Some(opa), a.clone()))),
            (RelOp::Match, RelOp::Match)            if a == b => construct(Some((Some(opa), a.clone()))),
            (RelOp::NotMatch, RelOp::NotMatch)      if a == b => construct(Some((Some(opa), a.clone()))),
            (RelOp::Match, RelOp::NotMatch)         if a == b => Self::Bottom,
            (RelOp::NotMatch, RelOp::Match)         if a == b => Self::Bottom,

            // ...

            _ => Value::Conjunction(vec![construct(Some((Some(opa), a.clone()))), construct(Some((Some(opb), b.clone())))]),
        }
     }

    fn meet_rel_op_ord<T: PartialEq + PartialOrd + Copy>(opa: RelOp, a: &T, opb: RelOp, b: &T, construct: fn(Basic<RelOp, T>) -> Self) -> Self {
        // TODO: could this be implemented by applying rel_op_ord in some clever way?
        match (opa, opb) {
            (RelOp::GreaterEqual, RelOp::LessEqual) if a == b => construct(Some((None, *a))),
            (RelOp::LessEqual, RelOp::GreaterEqual) if a == b => construct(Some((None, *a))),


            (RelOp::GreaterThan, RelOp::GreaterThan)  if a >= b => construct(Some((Some(opa), *a))),
            (RelOp::GreaterThan, RelOp::GreaterThan)  if a <  b => construct(Some((Some(opb), *b))),
            (RelOp::GreaterThan, RelOp::GreaterEqual) if a >= b => construct(Some((Some(opa), *a))),
            (RelOp::GreaterThan, RelOp::GreaterEqual) if a <  b => construct(Some((Some(opb), *b))),
            (RelOp::GreaterThan, RelOp::LessThan)     if a >= b => Self::Bottom,
            (RelOp::GreaterThan, RelOp::LessEqual)    if a >= b => Self::Bottom,
            (RelOp::GreaterThan, RelOp::NotEqual)     if a <= b => construct(Some((Some(opa), *a))),

            (RelOp::GreaterEqual, RelOp::GreaterEqual) if a >= b => construct(Some((Some(opa), *a))),
            (RelOp::GreaterEqual, RelOp::GreaterEqual) if a <  b => construct(Some((Some(opb), *b))),
            (RelOp::GreaterEqual, RelOp::GreaterThan)  if a >  b => construct(Some((Some(opa), *a))),
            (RelOp::GreaterEqual, RelOp::GreaterThan)  if a <= b => construct(Some((Some(opb), *b))),
            (RelOp::GreaterEqual, RelOp::LessEqual)    if a >= b => Self::Bottom,
            (RelOp::GreaterEqual, RelOp::LessThan)     if a >= b => Self::Bottom,
            (RelOp::GreaterEqual, RelOp::NotEqual)     if a <  b => construct(Some((Some(opa), *a))),


            (RelOp::LessThan, RelOp::LessThan)     if a <= b => construct(Some((Some(opa), *a))),
            (RelOp::LessThan, RelOp::LessThan)     if a >  b => construct(Some((Some(opb), *b))),
            (RelOp::LessThan, RelOp::LessEqual)    if a <= b => construct(Some((Some(opa), *a))),
            (RelOp::LessThan, RelOp::LessEqual)    if a >  b => construct(Some((Some(opb), *b))),
            (RelOp::LessThan, RelOp::GreaterThan)  if a <= b => Self::Bottom,
            (RelOp::LessThan, RelOp::GreaterEqual) if a <= b => Self::Bottom,
            (RelOp::LessThan, RelOp::NotEqual)     if a >= b => construct(Some((Some(opa), *a))),

            (RelOp::LessEqual, RelOp::LessEqual)    if a <= b => construct(Some((Some(opa), *a))),
            (RelOp::LessEqual, RelOp::LessEqual)    if a >  b => construct(Some((Some(opb), *b))),
            (RelOp::LessEqual, RelOp::LessThan)     if a <  b => construct(Some((Some(opa), *a))),
            (RelOp::LessEqual, RelOp::LessThan)     if a >= b => construct(Some((Some(opb), *b))),
            (RelOp::LessEqual, RelOp::GreaterThan)  if a <= b => Self::Bottom,
            (RelOp::LessEqual, RelOp::GreaterEqual) if a <= b => Self::Bottom,
            (RelOp::LessEqual, RelOp::NotEqual)     if a >  b => construct(Some((Some(opa), *a))),


            (RelOp::NotEqual, RelOp::NotEqual)     if a == b => construct(Some((Some(opb), *b))),
            (RelOp::NotEqual, RelOp::GreaterThan)  if a <= b => construct(Some((Some(opb), *b))),
            (RelOp::NotEqual, RelOp::GreaterEqual) if a <  b => construct(Some((Some(opb), *b))),
            (RelOp::NotEqual, RelOp::LessThan)     if a >= b => construct(Some((Some(opb), *b))),
            (RelOp::NotEqual, RelOp::LessEqual)    if a >  b => construct(Some((Some(opb), *b))),

            _ => Value::Conjunction(vec![construct(Some((Some(opa), *a))), construct(Some((Some(opb), *b)))]),
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
        Value::Int(None)
            .meet(Value::Int(Some((Some(RelOp::GreaterThan), 3)))),
        Value::Int(Some((Some(RelOp::GreaterThan), 3))),
        "int & >3 == >3"
    );
}

#[test]
fn test_int() {
    macro_rules! int_val {
        (_) => { Value::Top };
        (_|_) => { Value::Bottom };
        (int) => { Value::Int(None) };
        (   $a:literal) => { Value::Int(Some((None, $a))) };
        (>  $a:literal) => { Value::Int(Some((Some(RelOp::GreaterThan), $a))) };
        (>= $a:literal) => { Value::Int(Some((Some(RelOp::GreaterEqual), $a))) };
        (<  $a:literal) => { Value::Int(Some((Some(RelOp::LessThan), $a))) };
        (<= $a:literal) => { Value::Int(Some((Some(RelOp::LessEqual), $a))) };
        (!= $a:literal) => { Value::Int(Some((Some(RelOp::NotEqual), $a))) };
        (($($a:tt)+) & ($($b:tt)+)) => { Value::Conjunction(vec![int_val!($($a)+), int_val!($($b)+)]) };
    }

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
    }

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
