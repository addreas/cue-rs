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
    Value(T),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Field {
    label: Rc<str>,
    optional: bool,
    definition: bool,
    hidden: bool,
    value: Value,
}

impl Value {
    // infimum, greatest lower bound, unification (&)
    pub fn meet(self: Self, other: Self) -> Self {
        #[rustfmt::skip]
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

            (Self::Struct(fields), embedded) if fields.iter().all(|f| f.hidden | f.definition) => Self::Disjunction(vec![Self::Struct(fields), embedded]),
            (embedded, Self::Struct(fields)) if fields.iter().all(|f| f.hidden | f.definition) => Self::Disjunction(vec![Self::Struct(fields), embedded]),

            (Self::Disjunction(a), b) => Self::meet_disjunction(a, b),
            (a, Self::Disjunction(b)) => Self::meet_disjunction(b, a),

            _ => Self::Bottom, // probably some TODOs here
        }
    }

    // supremum, least upper bound, anti-unification (|)
    pub fn join(self: Self, other: Self) -> Self {
        #[rustfmt::skip]
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

            (Self::Int(lhs  ), Self::Int(rhs  )) => Self::join_basic(lhs, rhs, Self::Int),
            (Self::Float(lhs), Self::Float(rhs)) => Self::join_basic(lhs, rhs, Self::Float),

            (Self::Bytes(lhs ), Self::Bytes(rhs )) => Self::join_basic(lhs, rhs, Self::Bytes),
            (Self::String(lhs), Self::String(rhs)) => Self::join_basic(lhs, rhs, Self::String),

            (Self::Disjunction(a), b) => Self::join_disjunction(a, b),
            (a, Self::Disjunction(b)) => Self::join_disjunction(b, a),

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

            (Basic::Relation(opa, a), Basic::Relation(opb, b)) => {
                meet_rel_op(*opa, a, *opb, b, construct)
            }
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

    fn meet_rel_op_str<T: Eq + Clone>(
        opa: RelOp,
        a: &T,
        opb: RelOp,
        b: &T,
        construct: fn(Basic<RelOp, T>) -> Self,
    ) -> Self {
        #[rustfmt::skip]
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

    fn meet_rel_op_ord<T: PartialEq + PartialOrd + Copy>(
        opa: RelOp,
        a: &T,
        opb: RelOp,
        b: &T,
        construct: fn(Basic<RelOp, T>) -> Self,
    ) -> Self {
        // TODO: could this be implemented by applying rel_op_ord in some clever way?
        #[rustfmt::skip]
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
        let mut fields: Vec<Field> = vec![];
        for f in lhs.iter() {
            let rhs_value = rhs
                .iter()
                .find(|ff| ff.label == f.label)
                .map_or(Self::Top, |ff| ff.value.clone());
            fields.push(Field {
                label: f.label.clone(),
                optional: f.optional,
                definition: f.definition,
                hidden: f.hidden,
                value: f.value.clone().meet(rhs_value),
            });
        }
        for f in rhs.iter() {
            if lhs.iter().any(|ff| ff.label == f.label) {
                continue;
            }
            fields.push(f.clone())
        }

        Self::Struct(fields)
    }

    fn meet_lists(lhs: Vec<Value>, rhs: Vec<Value>) -> Value {
        if lhs.len() != rhs.len() {
            Self::Bottom
        } else {
            Self::List(
                lhs.into_iter()
                    .zip(rhs.into_iter())
                    .map(|(l, r)| l.meet(r))
                    .collect(),
            )
        }
    }

    fn meet_disjunction(items: Vec<Value>, operand: Value) -> Value {
        let res: Vec<_> = items
            .into_iter()
            .map(|i| i.meet(operand.clone()))
            .collect();

        let non_bottoms: Vec<_> = res.into_iter().filter(|i| !i.is_bottom()).collect();

        match non_bottoms.as_slice() {
            [] => Self::Bottom,
            [i] => i.clone(),
            _ => Self::Disjunction(non_bottoms)
        }

    }

    fn join_disjunction(mut existing: Vec<Value>, extension: Value) -> Value {
        for val in existing.iter() {
            if extension.clone() == *val {
                return Self::Disjunction(existing);
            }
        }

        existing.push(extension);
        Self::Disjunction(existing)
    }

    pub fn is_bottom(self: &Self) -> bool {
        return *self == Self::Bottom
    }

    pub fn to_option(self: Self) -> Option<Self> {
        match self {
            Self::Bottom => None,
            other => Some(other),
        }
    }
}

trait ToValue {
    fn to_value(self) -> Value;
    fn to_value_relation(self, op: RelOp) -> Value;
}

impl ToValue for &str {
    fn to_value(self) -> Value {
        Value::String(Basic::Value(Rc::from(self)))
    }
    fn to_value_relation(self, op: RelOp) -> Value {
        Value::String(Basic::Relation(op, Rc::from(self)))
    }
}

impl ToValue for i64 {
    fn to_value(self) -> Value {
        Value::Int(Basic::Value(self))
    }
    fn to_value_relation(self, op: RelOp) -> Value {
        Value::Int(Basic::Relation(op, self))
    }
}

impl ToValue for f64 {
    fn to_value(self) -> Value {
        Value::Float(Basic::Value(self))
    }
    fn to_value_relation(self, op: RelOp) -> Value {
        Value::Float(Basic::Relation(op, self))
    }
}



#[allow(unused_macros)]
macro_rules! cue_val {
    (_) => { Value::Top };
    (_|_) => { Value::Bottom };

    (null) => { Value::Null };
    (bool) => { Value::Bool(None) };
    (true) => { Value::Bool(Some(true)) };
    (false) => { Value::Bool(Some(false)) };

    (int) => { Value::Int(Basic::Type) };
    (float) => { Value::Float(Basic::Type) };
    (bytes) => { Value::Bytes(Basic::Type) };
    (string) => { Value::String(Basic::Type) };


    (   $a:literal) => { $a.to_value() };
    (>  $a:literal) => { $a.to_value_relation(RelOp::GreaterThan) };
    (>= $a:literal) => { $a.to_value_relation(RelOp::GreaterEqual) };
    (<  $a:literal) => { $a.to_value_relation(RelOp::LessThan) };
    (<= $a:literal) => { $a.to_value_relation(RelOp::LessEqual) };
    (!= $a:literal) => { $a.to_value_relation(RelOp::NotEqual) };

    (=~ $a:literal) => { $a.to_value_relation(RelOp::Match) };
    (!~ $a:literal) => { $a.to_value_relation(RelOp::NotMatch) };

    ( $(($($a:tt)+))&+ ) => { Value::Conjunction(vec![$(cue_val!($($a)+)),+ ]) };
    ( $(($($a:tt)+))|+ ) => { Value::Disjunction(vec![$(cue_val!($($a)+)),+ ]) };

    ({ $($k:ident: ($($v:tt)+)),* }) => {
        Value::Struct(vec![
            $(Field {
                label: Rc::from(stringify!($k)),
                optional: false,
                definition: false,
                hidden: false,
                value: cue_val!($($v)+),
            }),*
        ])
    };

    ([ $( ($($v:tt)+) ),* ]) => {
        Value::List(vec![
            $( cue_val!($($v)+) ),*
        ])
    };
}

#[allow(unused_macros)]
macro_rules! assert_cue {
    (($($a:tt)+) & ($($b:tt)+) == ($($c:tt)+)) => {
        let a = cue_val!($($a)+);
        let b = cue_val!($($b)+);
        let c = cue_val!($($c)+);
        assert_eq!(
            a.meet(b),
            c,
            "expect that ({}) & ({}) == ({})",
            stringify!($($a)+),
            stringify!($($b)+),
            stringify!($($c)+),
        )
    };

    (($($a:tt)+) | ($($b:tt)+) == ($($c:tt)+)) => {
        let a = cue_val!($($a)+);
        let b = cue_val!($($b)+);
        let c = cue_val!($($c)+);
        assert_eq!(
            a.join(b),
            c,
            "expect that ({}) | ({}) == ({})",
            stringify!($($a)+),
            stringify!($($b)+),
            stringify!($($c)+),
        )
    };
}
#[test]
fn test_basic_meets() {
    assert_cue!((_) & (_|_) == (_|_));
    assert_cue!((_) & (null) == (null));
    assert_cue!((_) & (bool) == (bool));
    assert_cue!((_) & (true) == (true));
    assert_cue!((bool) & (true) == (true));
}
#[test]
fn test_int_infimum() {
    assert_cue!((int) & (int) == (int));
    assert_cue!((int) & (1) == (1));
    assert_cue!((int) & (>1) == (>1));

    assert_cue!((0) & (>1) == (_|_));
    assert_cue!((1) & (>1) == (_|_));
    assert_cue!((2) & (>1) == (2));

    assert_cue!((0) & (<1) == (0));
    assert_cue!((1) & (<1) == (_|_));
    assert_cue!((2) & (<1) == (_|_));

    assert_cue!((0) & (>=1) == (_|_));
    assert_cue!((1) & (>=1) == (1));
    assert_cue!((2) & (>=1) == (2));

    assert_cue!((0) & (<=1) == (0));
    assert_cue!((1) & (<=1) == (1));
    assert_cue!((2) & (<=1) == (_|_));

    assert_cue!((0) & (!=1) == (0));
    assert_cue!((1) & (!=1) == (_|_));
    assert_cue!((2) & (!=1) == (2));

    assert_cue!((>10) & (>1) == (>10));
    assert_cue!((>10) & (>10) == (>10));
    assert_cue!((>10) & (>100) == (>100));

    assert_cue!((>10) & (>=1) == (>10));
    assert_cue!((>10) & (>=10) == (>10));
    assert_cue!((>10) & (>=100) == (>=100));

    assert_cue!((>10) & (!=1) == ((>10) & (!=1)));
    assert_cue!((>10) & (!=10) == (>10));
    assert_cue!((>10) & (!=100) == (>10));

    assert_cue!((>10) & (<1) == (_|_));
    assert_cue!((>10) & (<10) == (_|_));
    assert_cue!((>10) & (<100) == ((>10) & (<100)));

    assert_cue!((>10) & (<=1) == (_|_));
    assert_cue!((>10) & (<=10) == (_|_));
    assert_cue!((>10) & (<=100) == ((>10) & (<=100)));

    assert_cue!((>=10) & (>=1) == (>=10));
    assert_cue!((>=10) & (>=10) == (>=10));
    assert_cue!((>=10) & (>=100) == (>=100));

    assert_cue!((>=10) & (>1) == (>=10));
    assert_cue!((>=10) & (>10) == (>10));
    assert_cue!((>=10) & (>100) == (>100));

    assert_cue!((>=10) & (!=1) == ((>=10) & (!=1)));
    assert_cue!((>=10) & (!=10) == ((>=10) & (!=10)));
    assert_cue!((>=10) & (!=100) == (>=10));

    assert_cue!((>=10) & (<1) == (_|_));
    assert_cue!((>=10) & (<10) == (_|_));
    assert_cue!((>=10) & (<100) == ((>=10) & (<100)));

    assert_cue!((>=10) & (<=1) == (_|_));
    assert_cue!((>=10) & (<=10) == (10));
    assert_cue!((>=10) & (<=100) == ((>=10) & (<=100)));

    assert_cue!((<10) & (<1) == (<1));
    assert_cue!((<10) & (<10) == (<10));
    assert_cue!((<10) & (<100) == (<10));

    assert_cue!((<10) & (<=1) == (<=1));
    assert_cue!((<10) & (<=10) == (<10));
    assert_cue!((<10) & (<=100) == (<10));

    assert_cue!((<10) & (!=1) == (<10));
    assert_cue!((<10) & (!=10) == (<10));
    assert_cue!((<10) & (!=100) == ((<10) & (!=100)));

    assert_cue!((<10) & (>1) == ((<10) & (>1)));
    assert_cue!((<10) & (>10) == (_|_));
    assert_cue!((<10) & (>100) == (_|_));

    assert_cue!((<10) & (>=1) == ((<10) & (>=1)));
    assert_cue!((<10) & (>=10) == (_|_));
    assert_cue!((<10) & (>=100) == (_|_));

    assert_cue!((<=10) & (<=1) == (<=1));
    assert_cue!((<=10) & (<=10) == (<=10));
    assert_cue!((<=10) & (<=100) == (<=10));

    assert_cue!((<=10) & (<1) == (<1));
    assert_cue!((<=10) & (<10) == (<10));
    assert_cue!((<=10) & (<100) == (<=10));

    assert_cue!((<=10) & (!=1) == (<=10));
    assert_cue!((<=10) & (!=10) == ((<=10) & (!=10)));
    assert_cue!((<=10) & (!=100) == ((<=10) & (!=100)));

    assert_cue!((<=10) & (>1) == ((<=10) & (>1)));
    assert_cue!((<=10) & (>10) == (_|_));
    assert_cue!((<=10) & (>100) == (_|_));

    assert_cue!((<=10) & (>=1) == ((<=10) & (>=1)));
    assert_cue!((<=10) & (>=10) == (10));
    assert_cue!((<=10) & (>=100) == (_|_));

    assert_cue!((!=10) & (>1) == ((!=10) & (>1)));
    assert_cue!((!=10) & (>10) == (>10));
    assert_cue!((!=10) & (>100) == (>100));

    assert_cue!((!=10) & (>=1) == ((!=10) & (>=1)));
    assert_cue!((!=10) & (>=10) == ((!=10) & (>=10)));
    assert_cue!((!=10) & (>=100) == (>=100));

    assert_cue!((!=10) & (<1) == (<1));
    assert_cue!((!=10) & (<10) == (<10));
    assert_cue!((!=10) & (<100) == ((!=10) & (<100)));

    assert_cue!((!=10) & (<=1) == (<=1));
    assert_cue!((!=10) & (<=10) == ((!=10) & (<=10)));
    assert_cue!((!=10) & (<=100) == ((!=10) & (<=100)));
}

#[test]
fn test_int_supremum() {
    assert_cue!((int) | (int) == (int));
    assert_cue!((int) | (1) == (int));
    assert_cue!((int) | (>1) == (int));

    assert_cue!((0) | (>1) == ((0) | (>1)));
    assert_cue!((1) | (>1) == ((1) | (>1)));
    assert_cue!((2) | (>1) == (>1));

    assert_cue!((0) | (<1) == (<1));
    assert_cue!((1) | (<1) == ((1) | (<1)));
    assert_cue!((2) | (<1) == ((2) | (<1)));

    assert_cue!((0) | (>=1) == ((0) | (>=1)));
    assert_cue!((1) | (>=1) == (>=1));
    assert_cue!((2) | (>=1) == (>=1));

    assert_cue!((0) | (<=1) == (<=1));
    assert_cue!((1) | (<=1) == (>=1));
    assert_cue!((2) | (<=1) == ((2) | (<=1)));

    assert_cue!((0) | (!=1) == (!=1));
    assert_cue!((1) | (!=1) == (int));
    assert_cue!((2) | (!=1) == (!=1));

    assert_cue!((>10) | (>1) == (>1));
    assert_cue!((>10) | (>10) == (>10));
    assert_cue!((>10) | (>100) == (>10));

    assert_cue!((>10) | (>=1) == (>=1));
    assert_cue!((>10) | (>=10) == (>=10));
    assert_cue!((>10) | (>=100) == (>10));

    assert_cue!((>10) | (!=1) == (!=1));
    assert_cue!((>10) | (!=10) == (!=10));
    assert_cue!((>10) | (!=100) == (int));

    assert_cue!((>10) | (<1) == ((>10) | (<1)));
    assert_cue!((>10) | (<10) == (!=10)); // not true for floats
    assert_cue!((>10) | (<100) == (int));

    assert_cue!((>10) | (<=1) == ((>10) | (<=1)));
    assert_cue!((>10) | (<=10) == (int));
    assert_cue!((>10) | (<=100) == (int));

    assert_cue!((>=10) | (>=1) == (>=1));
    assert_cue!((>=10) | (>=10) == (>=10));
    assert_cue!((>=10) | (>=100) == (>=10));

    assert_cue!((>=10) | (>1) == (>1));
    assert_cue!((>=10) | (>10) == (>=10));
    assert_cue!((>=10) | (>100) == (>=10));

    assert_cue!((>=10) | (!=1) == (!=1));
    assert_cue!((>=10) | (!=10) == (int));
    assert_cue!((>=10) | (!=100) == (int));

    assert_cue!((>=10) | (<1) == ((>=10) | (<1)));
    assert_cue!((>=10) | (<10) == (int));
    assert_cue!((>=10) | (<100) == (int));

    assert_cue!((>=10) | (<=1) == ((>=10) | (<=1)));
    assert_cue!((>=10) | (<=10) == (int));
    assert_cue!((>=10) | (<=100) == (int));

    assert_cue!((<10) | (<1) == (<10));
    assert_cue!((<10) | (<10) == (<10));
    assert_cue!((<10) | (<100) == (<100));

    assert_cue!((<10) | (<=1) == (<10));
    assert_cue!((<10) | (<=10) == (<=10));
    assert_cue!((<10) | (<=100) == (<=100));

    assert_cue!((<10) | (!=1) == (int));
    assert_cue!((<10) | (!=10) == (!=10));
    assert_cue!((<10) | (!=100) == (!=100));

    assert_cue!((<10) | (>1) == (int));
    assert_cue!((<10) | (>10) == (!=10)); // not true for floats
    assert_cue!((<10) | (>100) == ((<10) | (>100)));

    assert_cue!((<10) | (>=1) == (int));
    assert_cue!((<10) | (>=10) == (int));
    assert_cue!((<10) | (>=100) == ((<10) | (>=100)));

    assert_cue!((<=10) | (<=1) == (<=10));
    assert_cue!((<=10) | (<=10) == (<=10));
    assert_cue!((<=10) | (<=100) == (<=100));

    assert_cue!((<=10) | (<1) == (<=10));
    assert_cue!((<=10) | (<10) == (<=10));
    assert_cue!((<=10) | (<100) == (<100));

    assert_cue!((<=10) | (!=1) == (int));
    assert_cue!((<=10) | (!=10) == (int));
    assert_cue!((<=10) | (!=100) == (!=100));

    assert_cue!((<=10) | (>1) == (int));
    assert_cue!((<=10) | (>10) == (int));
    assert_cue!((<=10) | (>100) == ((<=10) | (>100)));

    assert_cue!((<=10) | (>=1) == (int));
    assert_cue!((<=10) | (>=10) == (int));
    assert_cue!((<=10) | (>=100) == ((<=10) | (>=100)));

    assert_cue!((!=10) | (>1) == (int));
    assert_cue!((!=10) | (>10) == (!=10));
    assert_cue!((!=10) | (>100) == ((!=10) | (>100)));

    assert_cue!((!=10) | (>=1) == (int));
    assert_cue!((!=10) | (>=10) == (int));
    assert_cue!((!=10) | (>=100) == ((!=10) | (>=100)));

    assert_cue!((!=10) | (<1) == ((!=10) | (<1)));
    assert_cue!((!=10) | (<10) == (!=10));
    assert_cue!((!=10) | (<100) == (int));

    assert_cue!((!=10) | (<=1) == ((!=10) | (<=1)));
    assert_cue!((!=10) | (<=10) == (int));
    assert_cue!((!=10) | (<=100) == (int));
}

#[test]
fn test_struct_infimum() {
    assert_cue!(({}) & ({}) == ({}));
    assert_cue!(({}) & ({a: (1), b: (2)}) == ({a: (1), b: (2)}));
    assert_cue!(({a: (1)}) & ({b: (2)}) == ({a: (1), b: (2)}));
    assert_cue!(({a: (1), b: (2)}) & ({}) == ({a: (1), b: (2)}));
    assert_cue!(({a: (int)}) & ({a: (1)}) == ({a: (1)}));
    assert_cue!(({a: (float)}) & ({a: (1.0)}) == ({a: (1.0)}));
    assert_cue!(({a: (string)}) & ({a: ("hello")}) == ({a: ("hello")}));
    assert_cue!(({a: (int)}) & ({a: ("hello")}) == ({a: (_|_)}));
    assert_cue!(({a: (int)}) & ({b: ({c: ("d")})}) == ({a: (int), b: ({c: ("d")})}));
}

#[test]
fn test_list_infimum() {
    assert_cue!(([]) & ([]) == ([]));
    assert_cue!(([]) & ([(1), (2), (3)]) == (_|_));
    assert_cue!(([(int), (bool), (string)]) & ([(1), (bool), (=~"hello")]) == ([(1), (bool), (=~"hello")]));
}

#[test]
fn test_disjunct_infimum() {
    assert_cue!(((string) | (int) | (bool)) & (int) == (int));
    assert_cue!(((string) | (int) | (bool)) & (null) == (_|_));
    assert_cue!(((string) | (int) | (bool)) & (>2) == (>2));
    assert_cue!(((string) | (int) | (bool)) & (2) == (2));
    assert_cue!(((string) | (int) | (bool)) & ((2) | (true)) == ((2) | (true)));
    assert_cue!(((string) | (2) | (bool)) & ((int) | (true)) == ((2) | (true)));
}
