use std::cmp::Ordering;

use super::op::Op;

#[derive(Debug, PartialEq)]
pub enum Value<'a> {
    Top,

    Struct(Vec<Field<'a>>),
    List(Vec<Value<'a>>),
    // Func,
    Bytes(ValueType<&'a str>),
    String(ValueType<&'a str>),
    Float(ValueType<f64>),
    Int(ValueType<i64>),
    Bool(ValueType<bool>),
    Null,

    Bottom,
}

#[derive(Debug, PartialEq)]
pub struct Field<'a> {
    label: &'a str ,
    optional: bool,
    value: Value<'a>,
}

#[derive(Debug, PartialEq)]
pub enum ValueType<T: PartialEq> {
    Type,
    Constraint(Op, T),
    Concrete(T),
}

impl<T: PartialEq> ValueType<T> {
    pub const fn constraint(op: Op, inner: T) -> ValueType<T> {
        return ValueType::Constraint(op, inner);
    }
    pub const fn concrete(inner: T) -> ValueType<T> {
        return ValueType::Concrete(inner);
    }
}

impl<T: PartialEq> PartialOrd for ValueType<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (ValueType::Type, ValueType::Type) => Some(Ordering::Equal),
            (ValueType::Type, _) => Some(Ordering::Greater),
            (_, ValueType::Type) => Some(Ordering::Less),
            (ValueType::Constraint(_, _), _) => Some(Ordering::Greater),
            (_, ValueType::Constraint(_, _)) => Some(Ordering::Less),
            (ValueType::Concrete(_), ValueType::Concrete(_)) => Some(Ordering::Equal),
        }
    }
}

impl<'a> Field<'a> {
    pub const fn new(label: &'a str, optional: bool, value: Value<'a>) -> Field<'a> {
        Field {
            label,
            optional,
            value,
        }
    }
    pub const fn optional(label: &'a str, value: Value<'a>) -> Field<'a> {
        Self::new(label, true, value)
    }
    pub const fn required(label: &'a str, value: Value<'a>) -> Field<'a> {
        Self::new(label, false, value)
    }
}

impl<'a> PartialOrd for Field<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self.label != other.label {
            return None;
        }

        match (self.optional, other.optional) {
            (true, false) => Some(Ordering::Greater),
            (false, true) => Some(Ordering::Less),
            _ => self.value.partial_cmp(&other.value),
        }
    }
}

impl<'a> PartialOrd for Value<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Value::Top, Value::Top) => Some(Ordering::Equal),
            (Value::Top, _) => Some(Ordering::Greater),
            (_, Value::Top) => Some(Ordering::Less),

            (Value::Bottom, Value::Bottom) => Some(Ordering::Equal),
            (Value::Bottom, _) => Some(Ordering::Less),
            (_, Value::Bottom) => Some(Ordering::Greater),

            (Value::Null, Value::Null) => Some(Ordering::Equal),

            (Value::Struct(lhs), Value::Struct(rhs)) => compare_structs(lhs, rhs),
            (Value::List(lhs), Value::List(rhs)) => compare_lists(lhs, rhs),

            (Value::Bytes(lhs), Value::Bytes(rhs)) => lhs.partial_cmp(rhs),
            (Value::String(lhs), Value::String(rhs)) => lhs.partial_cmp(rhs),
            (Value::Float(lhs), Value::Float(rhs)) => lhs.partial_cmp(rhs),
            (Value::Int(lhs), Value::Int(rhs)) => lhs.partial_cmp(rhs),
            (Value::Bool(lhs), Value::Bool(rhs)) => lhs.partial_cmp(rhs),

            _ => None,
        }
    }
}

fn combine_orderings(acc: Option<Ordering>, item: Option<Ordering>) -> Option<Ordering> {
    match (acc, item) {
        (Some(a), Some(b)) if a == b => Some(a),
        (Some(Ordering::Equal), Some(a)) => Some(a),
        (Some(a), Some(Ordering::Equal)) => Some(a),
        _ => None,
    }
}

fn compare_structs(lhs: &Vec<Field>, rhs: &Vec<Field>) -> Option<Ordering> {
    let (long, short, reverse) = if lhs.len() > rhs.len() {
        (lhs, rhs, true)
    } else {
        (rhs, lhs, false)
    };

    let result = short
        .iter()
        .map(|f| long.iter().find_map(|ff| f.partial_cmp(ff)))
        .reduce(combine_orderings)
        .unwrap_or(None)
        .map(|ord| match ord {
            Ordering::Equal if lhs.len() != rhs.len() => Ordering::Greater,
            _ => ord,
        });

    if reverse {
        return result.map(|r| r.reverse());
    } else {
        return result;
    }
}

fn compare_lists(lhs: &Vec<Value>, rhs: &Vec<Value>) -> Option<Ordering> {
    lhs.iter()
        .zip(rhs.iter())
        .map(|(l, r)| l.partial_cmp(r))
        .reduce(combine_orderings)
        .unwrap_or(None)
}
