use std::fmt::Debug;

use regex::Regex;

use super::op::RelOp;

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Top,

    Disjunction(Vec<Value>),
    Conjunction(Vec<Value>),

    Struct(Vec<Field>),
    List(Vec<Value>),

    Bytes(BasicValue<String>),
    String(BasicValue<String>),
    Float(BasicValue<f64>),
    Int(BasicValue<i64>),

    Bool(Option<bool>), // (bool, true, false) == (None, Some(True), Some(False)

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

impl<T: PartialEq + PartialOrd + Debug> BasicValue<T> {
    pub fn join_with(
        self,
        other: Self,
        rel_op: impl Fn(&T, RelOp, &T) -> bool,
        getvalue: impl Fn(BasicValue<T>) -> Value,
    ) -> Value {
        match (self, other) {
            (Self::Type, _) => getvalue(Self::Type),
            (_, Self::Type) => getvalue(Self::Type),

            (Self::Concrete(a), Self::Concrete(b)) if a == b => getvalue(Self::Concrete(a)),

            (Self::Constraint(op, a), Self::Concrete(b)) if !rel_op(&b, op, &a) => {
                getvalue(Self::Constraint(op, a))
            }
            (Self::Concrete(a), Self::Constraint(op, b)) if !rel_op(&a, op, &b) => {
                getvalue(Self::Constraint(op, b))
            }

            (Self::Constraint(opa, a), Self::Constraint(opb, b)) => {
                match (opa, opb) {
                    (RelOp::GreaterThan, RelOp::GreaterThan) => todo!(),
                    (RelOp::GreaterEqual, RelOp::GreaterEqual) => todo!(),

                    (RelOp::LessThan, RelOp::LessThan) => todo!(),
                    (RelOp::LessEqual, RelOp::LessEqual) => todo!(),

                    _ => Value::Disjunction(vec![
                        getvalue(Self::Constraint(opa, a)),
                        getvalue(Self::Constraint(opb, b)),
                    ]),
                }
            }
            (a, b) => Value::Disjunction(vec![getvalue(a), getvalue(b)]),
        }
    }

    pub fn meet_with(
        self,
        other: Self,
        rel_op: impl Fn(&T, RelOp, &T) -> bool,
        getvalue: impl Fn(BasicValue<T>) -> Value,
    ) -> Value {
        match (self, other) {
            (Self::Type, b) => getvalue(b),
            (a, Self::Type) => getvalue(a),

            (Self::Concrete(a), Self::Concrete(b)) if a == b => getvalue(Self::Concrete(a)),

            (Self::Constraint(op, a), Self::Concrete(b)) if rel_op(&b, op, &a) => {
                getvalue(Self::Concrete(b))
            }
            (Self::Concrete(a), Self::Constraint(op, b)) if rel_op(&a, op, &b) => {
                getvalue(Self::Concrete(a))
            }

            (Self::Constraint(opa, a), Self::Constraint(opb, b)) => {
                #[rustfmt::skip]
                match (opa, opb) {
                    (RelOp::GreaterEqual, RelOp::LessEqual) if a == b => getvalue(Self::Concrete(a)),
                    (RelOp::LessEqual, RelOp::GreaterEqual) if a == b => getvalue(Self::Concrete(a)),


                    (RelOp::GreaterEqual, RelOp::GreaterEqual) if a >= b => getvalue(Self::Constraint(opa, a)),
                    (RelOp::GreaterEqual, RelOp::GreaterEqual) if a <  b => getvalue(Self::Constraint(opb, b)),
                    (RelOp::GreaterEqual, RelOp::GreaterThan)  if a >  b => getvalue(Self::Constraint(opa, a)),
                    (RelOp::GreaterEqual, RelOp::GreaterThan)  if a <= b => getvalue(Self::Constraint(opb, b)),
                    (RelOp::GreaterEqual, RelOp::NotEqual)     if a <  b => getvalue(Self::Constraint(opa, a)),

                    (RelOp::GreaterThan, RelOp::GreaterThan)  if a >= b => getvalue(Self::Constraint(opa, a)),
                    (RelOp::GreaterThan, RelOp::GreaterThan)  if a <  b => getvalue(Self::Constraint(opb, b)),
                    (RelOp::GreaterThan, RelOp::GreaterEqual) if a >= b => getvalue(Self::Constraint(opa, a)),
                    (RelOp::GreaterThan, RelOp::GreaterEqual) if a <  b => getvalue(Self::Constraint(opb, b)),
                    (RelOp::GreaterThan, RelOp::NotEqual)     if a <= b => getvalue(Self::Constraint(opa, a)),


                    (RelOp::LessEqual, RelOp::LessEqual) if a <= b => getvalue(Self::Constraint(opa, a)),
                    (RelOp::LessEqual, RelOp::LessEqual) if a >  b => getvalue(Self::Constraint(opb, b)),
                    (RelOp::LessEqual, RelOp::LessThan)  if a <  b => getvalue(Self::Constraint(opa, a)),
                    (RelOp::LessEqual, RelOp::LessThan)  if a >= b => getvalue(Self::Constraint(opb, b)),
                    (RelOp::LessEqual, RelOp::NotEqual)  if a >  b => getvalue(Self::Constraint(opa, a)),

                    (RelOp::LessThan, RelOp::LessThan)  if a <= b => getvalue(Self::Constraint(opa, a)),
                    (RelOp::LessThan, RelOp::LessThan)  if a >  b => getvalue(Self::Constraint(opb, b)),
                    (RelOp::LessThan, RelOp::LessEqual) if a <= b => getvalue(Self::Constraint(opa, a)),
                    (RelOp::LessThan, RelOp::LessEqual) if a >  b => getvalue(Self::Constraint(opb, b)),
                    (RelOp::LessThan, RelOp::NotEqual)  if a >= b => getvalue(Self::Constraint(opa, a)),


                    (RelOp::NotEqual, RelOp::NotEqual)     if a == b => getvalue(Self::Constraint(opb, b)),
                    (RelOp::NotEqual, RelOp::GreaterEqual) if a <  b => getvalue(Self::Constraint(opb, b)),
                    (RelOp::NotEqual, RelOp::GreaterThan)  if a <= b => getvalue(Self::Constraint(opb, b)),
                    (RelOp::NotEqual, RelOp::LessEqual)    if a >  b => getvalue(Self::Constraint(opb, b)),
                    (RelOp::NotEqual, RelOp::LessThan)     if a >= b => getvalue(Self::Constraint(opb, b)),


                    _ => Value::Conjunction(vec![getvalue(Self::Constraint(opa, a)), getvalue(Self::Constraint(opb, b))]),
                }
            }
            _ => Value::Bottom,
        }
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

fn rel_op_str(lhs: &String, op: RelOp, rhs: &String) -> bool {
    match op {
        RelOp::NotEqual => lhs != rhs,

        RelOp::GreaterEqual => lhs >= rhs,
        RelOp::GreaterThan => lhs > rhs,
        RelOp::LessEqual => lhs <= rhs,
        RelOp::LessThan => lhs < rhs,

        RelOp::Match => Regex::new(rhs.as_str()).unwrap().is_match(&lhs),
        RelOp::NotMatch => !Regex::new(rhs.as_str()).unwrap().is_match(&lhs),

        _ => false,
    }
}


impl Value {
    // supremum, least upper bound, anti-unification (|)
    pub fn join(self: Self, other: Self) -> Self {
        match (self, other) {
            (a, b) if a == b => a,

            (Self::Top, _) => Self::Top,
            (_, Self::Top) => Self::Top,

            (Self::Bottom, b) => b,
            (a, Self::Bottom) => a,

            (Self::Bool(_), Self::Bool(_)) => todo!("join_tcc(lhs, rhs)"),

            (Self::Float(_), Self::Float(_)) => todo!("join_tcc(lhs, rhs)"),
            (Self::Int(_), Self::Int(_)) => todo!("join_tcc(lhs, rhs)"),

            (Self::Bytes(_), Self::Bytes(_)) => todo!("join_tcc(lhs, rhs)"),
            (Self::String(_), Self::String(_)) => todo!("join_tcc(lhs, rhs)"),

            (Self::Disjunction(a), b) => Self::extend_disjunction(a, b),
            (a, Self::Disjunction(b)) => Self::extend_disjunction(b, a),

            (a, b) => Self::Disjunction(vec![a, b]),
        }
    }

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

            (Self::Bool(Some(a)), Self::Bool(Some(b))) => {
                if a == b {
                    Self::Bool(Some(a))
                } else {
                    Value::Bottom
                }
            }

            (Self::Float(lhs), Self::Float(rhs)) => lhs.meet_with(rhs, rel_op_ord, Self::Float),
            (Self::Int(lhs), Self::Int(rhs)) => lhs.meet_with(rhs, rel_op_ord, Self::Int),

            (Self::Bytes(lhs), Self::Bytes(rhs)) => lhs.meet_with(rhs, rel_op_str, Self::Bytes),
            (Self::String(lhs), Self::String(rhs)) => lhs.meet_with(rhs, rel_op_str, Self::String),

            (Self::Struct(lhs), Self::Struct(rhs)) => Self::meet_structs(lhs, rhs),
            (Self::List(lhs), Self::List(rhs)) => Self::meet_lists(lhs, rhs),

            (a, b) => Self::Conjunction(vec![a, b]),
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

    fn extend_disjunction(existing: Vec<Value>, extension: Value) -> Value {
        for val in existing.iter() {
            if val.clone().meet(extension.clone()) == *val {
                return Self::Disjunction(existing);
            }
        }
        return Self::Disjunction([existing, vec![extension]].concat());
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
        Value::Int(BasicValue::Type)
            .meet(Value::Int(BasicValue::Constraint(RelOp::GreaterThan, 3))),
        Value::Int(BasicValue::Constraint(RelOp::GreaterThan, 3)),
        "int & >3 == >3"
    );
}

#[test]
fn test_int() {
    macro_rules! int_val {
        (_) => { Value::Top };
        (_|_) => { Value::Bottom };
        (int) => { Value::Int(BasicValue::<i64>::Type) };
        (   $a:literal) => { Value::Int(BasicValue::Concrete($a)) };
        (>  $a:literal) => { Value::Int(BasicValue::Constraint(RelOp::GreaterThan, $a)) };
        (>= $a:literal) => { Value::Int(BasicValue::Constraint(RelOp::GreaterEqual, $a)) };
        (<  $a:literal) => { Value::Int(BasicValue::Constraint(RelOp::LessThan, $a)) };
        (<= $a:literal) => { Value::Int(BasicValue::Constraint(RelOp::LessEqual, $a)) };
        (!= $a:literal) => { Value::Int(BasicValue::Constraint(RelOp::NotEqual, $a)) };
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

    assert_int!((<=1) & (>=1) == (1));

    assert_int!((>10) & (>1) == (>10));
    assert_int!((>10) & (>10) == (>10));
    assert_int!((>10) & (>100) == (>100));

    assert_int!((>10) & (>=1) == (>10));
    assert_int!((>10) & (>=10) == (>10));
    assert_int!((>10) & (>=100) == (>=100));

    assert_int!((>10) & (!=1) == ((>10) & (!=1)));
    assert_int!((>10) & (!=10) == (>10));
    assert_int!((>10) & (!=100) == (>10));


    assert_int!((>=10) & (>=1) == (>=10));
    assert_int!((>=10) & (>=10) == (>=10));
    assert_int!((>=10) & (>=100) == (>=100));

    assert_int!((>=10) & (>1) == (>=10));
    assert_int!((>=10) & (>10) == (>10));
    assert_int!((>=10) & (>100) == (>100));

    assert_int!((>=10) & (!=1) == ((>=10) & (!=1)));
    assert_int!((>=10) & (!=10) == ((>=10) & (!=10)));
    assert_int!((>=10) & (!=100) == (>=10));


    assert_int!((<10) & (<1) == (<1));
    assert_int!((<10) & (<10) == (<10));
    assert_int!((<10) & (<100) == (<10));

    assert_int!((<10) & (<=1) == (<=1));
    assert_int!((<10) & (<=10) == (<10));
    assert_int!((<10) & (<=100) == (<10));

    assert_int!((<10) & (!=1) == (<10));
    assert_int!((<10) & (!=10) == (<10));
    assert_int!((<10) & (!=100) == ((<10) & (!=100)));


    assert_int!((<=10) & (<=1) == (<=1));
    assert_int!((<=10) & (<=10) == (<=10));
    assert_int!((<=10) & (<=100) == (<=10));

    assert_int!((<=10) & (<1) == (<1));
    assert_int!((<=10) & (<10) == (<10));
    assert_int!((<=10) & (<100) == (<=10));

    assert_int!((<=10) & (!=1) == (<=10));
    assert_int!((<=10) & (!=10) == ((<=10) & (!=10)));
    assert_int!((<=10) & (!=100) == ((<=10) & (!=100)));


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
