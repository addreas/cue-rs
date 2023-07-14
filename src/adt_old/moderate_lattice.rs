use regex::Regex;

use super::op::Op;

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Top,

    Disjunction(Vec<Value>),
    Conjunction(Vec<Value>),

    Struct(Vec<Field>),
    List(Vec<Value>),
    BasicValue(BasicValue),

    Bottom,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Field {
    label: String,
    optional: bool,
    value: Value,
}

#[derive(Debug, PartialEq, Clone)]
pub enum BasicValue {
    Bytes(TCC<String>),
    String(TCC<String>),
    Float(TCC<f64>),
    Int(TCC<i64>),
    Bool(Option<bool>), // represent the bool "type" as None
    Null,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TCC<T> {
    Type,
    Constraint(Op, T),
    Concrete(T),
}

impl<T: PartialEq> TCC<T> {
    pub fn meet_with(
        self,
        other: Self,
        meeter: impl Fn(Op, T, T) -> Option<T>,
        getvalue: impl Fn(TCC<T>) -> Value,
    ) -> Value {
        match (self, other) {
            (Self::Type, b) => getvalue(b),
            (a, Self::Type) => getvalue(a),

            (Self::Concrete(a), Self::Concrete(b)) => {
                if a == b {
                    getvalue(Self::Concrete(a))
                } else {
                    Value::Bottom
                }
            }

            (Self::Constraint(op, a), Self::Concrete(b)) => {
                meeter(op, b, a).map_or(Value::Bottom, |c| getvalue(Self::Concrete(c)))
            }
            (Self::Concrete(a), Self::Constraint(op, b)) => {
                meeter(op, a, b).map_or(Value::Bottom, |c| getvalue(Self::Concrete(c)))
            }

            (a, b) => Value::Conjunction(vec![getvalue(a), getvalue(b)]),
        }
    }

    fn meet_ord<TT: PartialOrd>(op: Op, concrete: TT, constraint: TT) -> Option<TT> {
        let success = match op {
            Op::Equal => concrete == constraint,
            Op::NotEqual => concrete != constraint,

            Op::GreaterEqual => concrete >= constraint,
            Op::GreaterThan => concrete > constraint,
            Op::LessEqual => concrete <= constraint,
            Op::LessThan => concrete < constraint,

            _ => false,
        };

        if success {
            Some(concrete)
        } else {
            None
        }
    }

    fn meet_str(op: Op, concrete: String, constraint: String) -> Option<String> {
        let success = match op {
            Op::Equal => concrete == constraint,
            Op::NotEqual => concrete != constraint,

            Op::GreaterEqual => concrete >= constraint,
            Op::GreaterThan => concrete > constraint,
            Op::LessEqual => concrete <= constraint,
            Op::LessThan => concrete < constraint,

            Op::Match => Regex::new(constraint.as_str()).unwrap().is_match(&concrete),
            Op::NotMatch => !Regex::new(constraint.as_str()).unwrap().is_match(&concrete),

            _ => false,
        };

        if success {
            Some(concrete)
        } else {
            None
        }
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

            (Self::BasicValue(lhs), Self::BasicValue(rhs)) => Self::join_basic(lhs, rhs),

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

            (Self::BasicValue(lhs), Self::BasicValue(rhs)) => Self::meet_basic(lhs, rhs),

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

    fn join_basic(lhs: BasicValue, rhs: BasicValue) -> Value {
        match (lhs, rhs) {
            (BasicValue::Null, BasicValue::Null) => Value::BasicValue(BasicValue::Null),
            (BasicValue::Bool(_), BasicValue::Bool(_)) => todo!("join_tcc(lhs, rhs)"),

            (BasicValue::Float(_), BasicValue::Float(_)) => todo!("join_tcc(lhs, rhs)"),
            (BasicValue::Int(_), BasicValue::Int(_)) => todo!("join_tcc(lhs, rhs)"),

            (BasicValue::Bytes(_), BasicValue::Bytes(_)) => todo!("join_tcc(lhs, rhs)"),
            (BasicValue::String(_), BasicValue::String(_)) => todo!("join_tcc(lhs, rhs)"),

            (lhs, rhs) => Value::Disjunction(vec![Value::BasicValue(lhs), Value::BasicValue(rhs)]),
        }
    }

    fn meet_basic(lhs: BasicValue, rhs: BasicValue) -> Value {
        match (lhs, rhs) {
            (BasicValue::Null, BasicValue::Null) => Value::BasicValue(BasicValue::Null),
            (BasicValue::Bool(None), BasicValue::Bool(b)) => Value::BasicValue(BasicValue::Bool(b)),
            (BasicValue::Bool(a), BasicValue::Bool(None)) => Value::BasicValue(BasicValue::Bool(a)),
            (BasicValue::Bool(Some(a)), BasicValue::Bool(Some(b))) => {
                if a == b {
                    Value::BasicValue(BasicValue::Bool(Some(a)))
                } else {
                    Value::Bottom
                }
            }

            (BasicValue::Float(lhs), BasicValue::Float(rhs)) => {
                lhs.meet_with(rhs, TCC::<f64>::meet_ord, |i| {
                    Value::BasicValue(BasicValue::Float(i))
                })
            }
            (BasicValue::Int(lhs), BasicValue::Int(rhs)) => {
                lhs.meet_with(rhs, TCC::<i64>::meet_ord, |i| {
                    Value::BasicValue(BasicValue::Int(i))
                })
            }

            (BasicValue::Bytes(lhs), BasicValue::Bytes(rhs)) => {
                lhs.meet_with(rhs, TCC::<String>::meet_str, |v| {
                    Value::BasicValue(BasicValue::Bytes(v))
                })
            }
            (BasicValue::String(lhs), BasicValue::String(rhs)) => {
                lhs.meet_with(rhs, TCC::<String>::meet_str, |v| {
                    Value::BasicValue(BasicValue::String(v))
                })
            }

            (lhs, rhs) => Value::Conjunction(vec![Value::BasicValue(lhs), Value::BasicValue(rhs)]),
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
