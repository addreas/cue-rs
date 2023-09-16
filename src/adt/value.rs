use std::{fmt::Debug, fmt::Display, rc::Rc};

use regex::Regex;

use super::op::RelOp;
use crate::{match_rel_ops, assert_cue};


#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Top,

    Disjunction(Vec<Rc<Value>>),
    Conjunction(Vec<Rc<Value>>),

    Struct(Vec<Field>),
    List(Vec<Rc<Value>>),

    String(Basic<Rc<str>>),
    Bytes(Basic<Rc<str>>),

    Float(Basic<f64>),
    Int(Basic<i64>),

    Bool(Option<bool>),

    Null,

    Bottom,
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        macro_rules! write_list {
            ($items:ident, $sep:literal) => { write_list!($items, "", $sep, "") };
            ($items:ident, $start:literal, $sep:literal, $end: literal) => {
                {
                    if let Some((last, items)) = $items.split_last() {
                        write!(f, "{}", $start)
                        .and_then(|_| {
                            items.iter()
                            .fold(Ok(()), |acc, item| {
                                acc
                                .and_then(|_| Display::fmt(item, f))
                                .and_then(|_| write!(f, "{}", $sep))
                            })
                        .and_then(|_| Display::fmt(last, f))
                        .and_then(|_| write!(f, "{}", $end))
                        })

                    } else {
                        Err(std::fmt::Error)
                    }
                }
            };
        }
        match self {
            Value::Top => write!(f, "_"),
            Value::Disjunction(items) => write_list!(items, " | "),
            Value::Conjunction(items) => write_list!(items, " & "),
            Value::Struct(items) => write_list!(items, "{", ",\n", "}"),
            Value::List(items) => write_list!(items, "[", ",\n", "]"),
            Value::String(val) => Display::fmt(val, f),
            Value::Bytes(val) => Display::fmt(val, f),
            Value::Float(val) => Display::fmt(val, f),
            Value::Int(val) => Display::fmt(val, f),
            Value::Bool(None) => write!(f, "bool"),
            Value::Bool(Some(true)) => write!(f, "true"),
            Value::Bool(Some(false)) => write!(f, "false"),
            Value::Null => write!(f, "null"),
            Value::Bottom => write!(f, "_|_"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Basic<T> {
    Type,
    Relation(RelOp, T),
    Value(T),
}

impl<T: Display> Display for Basic<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Type => write!(f, ""),
            Self::Relation(op, val) => write!(f, "{}{}", op, val),
            Self::Value(val) => write!(f, "{}", val),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Field {
    label: Rc<str>,
    optional: bool,
    definition: bool,
    hidden: bool,
    value: Rc<Value>,
}

impl Display for Field {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let definition = if self.definition { "#" } else { "" };
        let optional = if self.optional { "?" } else { "" };
        let hidden = if self.hidden { "_" } else { "" };
        write!(f, "{}{}{}{}: {}", hidden, definition, self.label, optional, self.value)
    }
}

impl Value {
    // infimum, greatest lower bound, unification (&)
    pub fn meet(self: Rc<Self>, other: Rc<Self>) -> Rc<Self> {
        #[rustfmt::skip]
        match (self.as_ref(), other.as_ref()) {
            (a, b) if a == b => self,

            (Self::Top, _) => other,
            (_, Self::Top) => self,

            (Self::Bottom, _) => self,
            (_, Self::Bottom) => other,

            (Self::Bool(None), Self::Bool(_)) => other,
            (Self::Bool(_), Self::Bool(None)) => self,

            (Self::Bool(Some(a)), Self::Bool(Some(b))) if a == b => self,
            (Self::Bool(Some(a)), Self::Bool(Some(b))) if a != b => Self::Bottom.into(),

            (Self::Int(lhs), Self::Int(rhs)) => Self::meet_basic(lhs.clone(), rhs.clone(), Self::Int, Self::rel_op_ord, Self::meet_rel_op_ord).into(),
            (Self::Float(lhs), Self::Float(rhs)) => Self::meet_basic(lhs.clone(), rhs.clone(), Self::Float, Self::rel_op_ord, Self::meet_rel_op_ord).into(),

            (Self::String(lhs), Self::String(rhs)) => Self::meet_basic(lhs.clone(), rhs.clone(), Self::String, |a, op, b| Self::rel_op_str(a, op, b), Self::meet_rel_op_str).into(),
            (Self::Bytes(lhs), Self::Bytes(rhs)) => Self::meet_basic(lhs.clone(), rhs.clone(), Self::Bytes, |a, op, b| Self::rel_op_str(a, op, b), Self::meet_rel_op_str).into(),

            (Self::Struct(lhs), Self::Struct(rhs)) => Self::meet_structs(lhs.clone(), rhs.clone()).into(),
            (Self::List(lhs), Self::List(rhs)) => Self::meet_lists(lhs.clone(), rhs.clone()).into(),

            (Self::Struct(fields), _) if fields.iter().all(|f| f.hidden | f.definition) => Self::Disjunction(vec![Self::Struct(fields.clone()).into(), other]).into(),
            (_, Self::Struct(fields)) if fields.iter().all(|f| f.hidden | f.definition) => Self::Disjunction(vec![Self::Struct(fields.clone()).into(), self]).into(),

            (Self::Disjunction(lhs), _) => Self::meet_disjunction(lhs.clone(), other),
            (_, Self::Disjunction(rhs)) => Self::meet_disjunction(rhs.clone(), self),

            (_, _) => Self::Bottom.into(), // probably some TODOs here
        }
    }

    // supremum, least upper bound, anti-unification (|)
    pub fn join(self: Rc<Self>, other: Rc<Self>) -> Rc<Self> {
        #[rustfmt::skip]
        match (self.as_ref(), other.as_ref()) {
            (a, b) if a == b => self,

            (Self::Top, _) => self,
            (_, Self::Top) => other,

            (Self::Bottom, _) => other,
            (_, Self::Bottom) => self,

            (Self::Bool(None), Self::Bool(_)) => self,
            (Self::Bool(_), Self::Bool(None)) => other,

            (Self::Bool(Some(a)), Self::Bool(Some(b))) if a == b => self,
            (Self::Bool(Some(a)), Self::Bool(Some(b))) if a != b => Self::Bool(None).into(),

            (Self::Int(lhs  ), Self::Int(rhs  )) => Self::join_basic(lhs.clone(), rhs.clone(), Self::Int, Self::rel_op_ord, Self::join_rel_op_ord).into(),
            (Self::Float(lhs), Self::Float(rhs)) => Self::join_basic(lhs.clone(), rhs.clone(), Self::Float, Self::rel_op_ord, Self::join_rel_op_ord).into(),

            (Self::Bytes(lhs ), Self::Bytes(rhs )) => Self::join_basic(lhs.clone(), rhs.clone(), Self::Bytes, |a, op, b| Self::rel_op_str(a, op, b), Self::join_rel_op_str).into(),
            (Self::String(lhs), Self::String(rhs)) => Self::join_basic(lhs.clone(), rhs.clone(), Self::String, |a, op, b| Self::rel_op_str(a, op, b), Self::join_rel_op_str).into(),

            (Self::Disjunction(lhs), _) => Self::join_disjunction(lhs.clone(), other).into(),
            (_, Self::Disjunction(rhs)) => Self::join_disjunction(rhs.clone(), self).into(),

            (_, _) => Self::Disjunction(vec![self, other]).into(),
        }
    }

    fn meet_basic<T: PartialEq + Display>(
        lhs: Basic<T>,
        rhs: Basic<T>,
        construct: fn(Basic<T>) -> Self,
        rel_op: fn(&T, RelOp, &T) -> bool,
        meet_rel_op: fn((RelOp, &T), (RelOp, &T), fn(Basic<T>) -> Self) -> Self,
    ) -> Self {
        match (&lhs, &rhs) {
            (Basic::Type, _) => construct(rhs),
            (_, Basic::Type) => construct(lhs),

            (Basic::Value(a), Basic::Value(b)) if a == b => construct(lhs),

            (Basic::Value(a), Basic::Relation(op, b)) if rel_op(a, *op, b) => construct(lhs),
            (Basic::Relation(op, a), Basic::Value(b)) if rel_op(a, *op, b) => construct(rhs),

            (Basic::Relation(opa, a), Basic::Relation(opb, b)) => {
                let res = meet_rel_op((*opa, a), (*opb, b), construct);

                let rel_op_bools = (rel_op(a, *opa, b), rel_op(a, *opb, b), rel_op(b, *opa, a), rel_op(b, *opb, a));
                println!("{rel_op_bools:?}: {opa}{a} & {opa}{a} == {res}");

                res
            }

            (_, _) => Self::Bottom,
        }
    }

    fn join_basic<T: PartialEq>(
        lhs: Basic<T>,
        rhs: Basic<T>,
        construct: fn(Basic<T>) -> Self,
        rel_op: fn(&T, RelOp, &T) -> bool,
        join_rel_op: fn((RelOp, &T), (RelOp, &T), fn(Basic<T>) -> Self) -> Self,
    ) -> Self {
        match (&lhs, &rhs) {
            (Basic::Type, _) => construct(lhs),
            (_, Basic::Type) => construct(rhs),

            (Basic::Value(a), Basic::Value(b)) if a == b => construct(lhs),

            (Basic::Value(a), Basic::Relation(op, b)) if rel_op(a, *op, b) => construct(rhs),
            (Basic::Value(a), Basic::Relation(op, b)) if *op == RelOp::NotEqual && a == b => construct(Basic::Type),
            (Basic::Relation(op, a), Basic::Value(b)) if rel_op(a, *op, b) => construct(lhs),
            (Basic::Relation(op, a), Basic::Value(b)) if *op == RelOp::NotEqual && a == b => construct(Basic::Type),

            (Basic::Relation(opa, a), Basic::Relation(opb, b)) => {
                join_rel_op((*opa, a), (*opb, b), construct)
            }

            (_, _) => Self::Disjunction(vec![construct(lhs).into(), construct(rhs).into()]),
        }
    }

    fn meet_rel_op_str<T: Eq + Clone>(
        a: (RelOp, &T),
        b: (RelOp, &T),
        construct: fn(Basic<T>) -> Self,
    ) -> Self {
        match_rel_ops!(a, b, construct, Value::Conjunction, {
            ((>=a) | (<=b)) if a == b => (a),
            ((match a) | (notmatch b)) if a == b => (bot),
        })
    }

    fn meet_rel_op_ord<T: PartialEq + PartialOrd + Copy>(
        a: (RelOp, &T),
        b: (RelOp, &T),
        construct: fn(Basic<T>) -> Self,
    ) -> Self {
        match_rel_ops!(a, b, construct, Value::Conjunction, {
            ((>=a) & (>=b)) if a <= b => (>=b) else => (>=a),
            ((>=a) & (<=b)) if a == b => (a) else if b < a => (bot),
            ((>=a) & (!=b)) if a >  b => (>=a) else if a == b => (>a),
            ((>=a) & (> b)) if a <= b => (> b) else => (>=a),
            ((>=a) & (< b)) if a >= b => (bot),
            ((> a) & (> b)) if a <  b => (> b),
            ((> a) & (< b)) if a >= b => (bot),
            ((> a) & (<=b)) if a >= b => (bot),
            ((> a) & (!=b)) if a >= b => (> a),
            ((< a) & (< b)) if a >  b => (< b) else => (< a),
            ((< a) & (<=b)) if a >  b => (<=b) else => (< a),
            ((< a) & (!=b)) if a <= b => (< a),
            ((<=a) & (<=b)) if a >  b => (<=b) else => (<=a),
            ((<=a) & (!=b)) if a <  b => (<=a) else if a == b => (<a),
        })
    }

    fn join_rel_op_str<T: Eq + Clone>(
        a: (RelOp, &T),
        b: (RelOp, &T),
        construct: fn(Basic<T>) -> Self,
    ) -> Self {
        match_rel_ops!(a, b, construct, Value::Disjunction, {
            ((>=a) | (<=b)) if a == b => (typ),
            ((match a) | (notmatch b)) if a == b => (typ),
        })
    }

    fn join_rel_op_ord<T: PartialEq + PartialOrd + Copy>(
        a: (RelOp, &T),
        b: (RelOp, &T),
        construct: fn(Basic<T>) -> Self,
    ) -> Self {
        match_rel_ops!(a, b, construct, Value::Disjunction, {
            ((>=a) | (>=b)) if a <= b => (>=a) else => (>=b),
            ((>=a) | (<=b)) if a <= b => (typ),
            ((>=a) | (!=b)) if a <= b => (typ) else => (!=b),
            ((>=a) | (> b)) if a <= b => (>=a) else => (> b),
            ((>=a) | (< b)) if a <= b => (typ),
            ((> a) | (> b)) if a <  b => (> a),
            ((> a) | (< b)) if a == b => (!=a) else if a < b => (typ),
            ((> a) | (<=b)) if a <= b => (typ),
            ((> a) | (!=b)) if a <  b => (typ) else => (!=b),
            ((< a) | (< b)) if a >  b => (< a) else => (< b),
            ((< a) | (<=b)) if a >  b => (< a) else => (<=b),
            ((< a) | (!=b)) if a <= b => (!=b) else => (typ),
            ((<=a) | (<=b)) if a >  b => (<=a) else => (<=b),
            ((<=a) | (!=b)) if a <  b => (!=b) else => (typ),
        })
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
                .map_or(Self::Top.into(), |ff| ff.value.clone());
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

    fn meet_lists(lhs: Vec<Rc<Value>>, rhs: Vec<Rc<Value>>) -> Value {
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

    fn meet_disjunction(items: Vec<Rc<Value>>, operand: Rc<Value>) -> Rc<Value> {
        let res: Vec<_> = items.into_iter().map(|i| i.meet(operand.clone())).collect();

        let non_bottoms: Vec<_> = res.into_iter().filter(|i| !i.is_bottom()).collect();

        match non_bottoms.as_slice() {
            [] => Self::Bottom.into(),
            [i] => i.clone(),
            _ => Self::Disjunction(non_bottoms).into(),
        }
    }

    fn join_disjunction(mut existing: Vec<Rc<Value>>, extension: Rc<Value>) -> Value {
        for val in existing.iter() {
            if extension.clone() == *val {
                return Self::Disjunction(existing);
            }
        }

        existing.push(extension);
        Self::Disjunction(existing)
    }

    pub fn is_bottom(self: &Self) -> bool {
        *self == Self::Bottom
    }

    pub fn to_option(self: Self) -> Option<Self> {
        match self {
            Self::Bottom => None,
            other => Some(other),
        }
    }
}

#[test]
fn test_basic_meets() {
    assert_cue!((_) & (_ | _) == (_ | _));
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

    assert_cue!((>10) & (!=1) == (>10));
    assert_cue!((>10) & (!=10) == (>10));
    assert_cue!((>10) & (!=100) == ((>10) & (!=100)));

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

    assert_cue!((>=10) & (!=1) == (>=10));
    assert_cue!((>=10) & (!=10) == (>10));
    assert_cue!((>=10) & (!=100) == ((>=10) & (!=100)));

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

    assert_cue!((<10) & (!=1) == ((<10) & (!=1)));
    assert_cue!((<10) & (!=10) == (<10));
    assert_cue!((<10) & (!=100) == (<10));

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

    assert_cue!((<=10) & (!=1) == ((<=10) & (!=1)));
    assert_cue!((<=10) & (!=10) == (<10));
    assert_cue!((<=10) & (!=100) == (<=10));

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
    assert_cue!((!=10) & (>=10) == (>10));
    assert_cue!((!=10) & (>=100) == (>=100));

    assert_cue!((!=10) & (<1) == (<1));
    assert_cue!((!=10) & (<10) == (<10));
    assert_cue!((!=10) & (<100) == ((!=10) & (<100)));

    assert_cue!((!=10) & (<=1) == (<=1));
    assert_cue!((!=10) & (<=10) == (<10));
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
    assert_cue!((1) | (<=1) == (<=1));
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
    assert_cue!((!=10) | (>100) == (!=10));

    assert_cue!((!=10) | (>=1) == (int));
    assert_cue!((!=10) | (>=10) == (int));
    assert_cue!((!=10) | (>=100) == (!=10));

    assert_cue!((!=10) | (<1) == (!=10));
    assert_cue!((!=10) | (<10) == (!=10));
    assert_cue!((!=10) | (<100) == (int));

    assert_cue!((!=10) | (<=1) == (!=10));
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
    assert_cue!(([]) & ([(1), (2), (3)]) == (_ | _));
    assert_cue!(([(int), (bool), (string)]) & ([(1), (bool), (=~"hello")]) == ([(1), (bool), (=~"hello")]));
}

#[test]
fn test_disjunct_infimum() {
    assert_cue!(((string) | (int) | (bool)) & (int) == (int));
    assert_cue!(((string) | (int) | (bool)) & (null) == (_ | _));
    assert_cue!(((string) | (int) | (bool)) & (>2) == (>2));
    assert_cue!(((string) | (int) | (bool)) & (2) == (2));
    assert_cue!(((string) | (int) | (bool)) & ((2) | (true)) == ((2) | (true)));
    assert_cue!(((string) | (2) | (bool)) & ((int) | (true)) == ((2) | (true)));
}

trait ToValue {
    fn to_value(self) -> Value;
    fn to_value_relation(self, op: RelOp) -> Value;
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

impl ToValue for &str {
    fn to_value(self) -> Value {
        Value::String(Basic::Value(Rc::from(self)))
    }
    fn to_value_relation(self, op: RelOp) -> Value {
        Value::String(Basic::Relation(op, Rc::from(self)))
    }
}
