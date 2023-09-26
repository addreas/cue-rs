use std::{fmt::Debug, fmt::Display, rc::Rc};

use regex::Regex;

use super::op::RelOp;
use crate::match_basic;


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

#[derive(Debug, PartialEq, Clone)]
pub enum Basic<T> {
    Type,
    Relation(RelOp, T),
    Value(T),
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Label {
    pub name: Rc<str>,
    pub optional: Option<bool>,
    pub definition: bool,
    pub hidden: bool,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Field {
    pub label: Label,
    pub value: Rc<Value>,
}

impl Display for Field {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let definition = if self.label.definition { "#" } else { "" };
        let optional = match self.label.optional {
            None => "",
            Some(true) => "?",
            Some(false) => "!",
        };
        let hidden = if self.label.hidden { "_" } else { "" };
        write!(
            f,
            "{hidden}{definition}{}{optional}: {}",
            self.label.name, self.value
        )
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

            (Self::Int(lhs), Self::Int(rhs)) => Self::meet_basic(lhs.clone(), rhs.clone(), Self::Int, Self::rel_op_ord).into(),
            (Self::Float(lhs), Self::Float(rhs)) => Self::meet_basic(lhs.clone(), rhs.clone(), Self::Float, Self::rel_op_ord).into(),

            (Self::String(lhs), Self::String(rhs)) => Self::meet_basic(lhs.clone(), rhs.clone(), Self::String, |a, op, b| Self::rel_op_str(a, op, b)).into(),
            (Self::Bytes(lhs), Self::Bytes(rhs)) => Self::meet_basic(lhs.clone(), rhs.clone(), Self::Bytes, |a, op, b| Self::rel_op_str(a, op, b)).into(),

            (Self::Struct(lhs), Self::Struct(rhs)) => Self::meet_structs(lhs.clone(), rhs.clone()).into(),
            (Self::List(lhs), Self::List(rhs)) => Self::meet_lists(lhs.clone(), rhs.clone()).into(),

            (Self::Struct(fields), _) if fields.iter().all(|f| f.label.hidden | f.label.definition) => Self::Disjunction(vec![Self::Struct(fields.clone()).into(), other]).into(),
            (_, Self::Struct(fields)) if fields.iter().all(|f| f.label.hidden | f.label.definition) => Self::Disjunction(vec![Self::Struct(fields.clone()).into(), self]).into(),

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

            (Self::Int(lhs  ), Self::Int(rhs  )) => Self::join_basic(lhs.clone(), rhs.clone(), Self::Int, Self::rel_op_ord).into(),
            (Self::Float(lhs), Self::Float(rhs)) => Self::join_basic(lhs.clone(), rhs.clone(), Self::Float, Self::rel_op_ord).into(),

            (Self::Bytes(lhs ), Self::Bytes(rhs )) => Self::join_basic(lhs.clone(), rhs.clone(), Self::Bytes, |a, op, b| Self::rel_op_str(a, op, b)).into(),
            (Self::String(lhs), Self::String(rhs)) => Self::join_basic(lhs.clone(), rhs.clone(), Self::String, |a, op, b| Self::rel_op_str(a, op, b)).into(),

            (Self::Disjunction(lhs), _) => Self::join_disjunction(lhs.clone(), other).into(),
            (_, Self::Disjunction(rhs)) => Self::join_disjunction(rhs.clone(), self).into(),

            (_, _) => Self::Disjunction(vec![self, other]).into(),
        }
    }

    fn meet_basic<T: PartialEq + Clone + Display>(
        lhs: Basic<T>,
        rhs: Basic<T>,
        construct: fn(Basic<T>) -> Self,
        rel_op: fn(&T, RelOp, &T) -> bool,
    ) -> Self {
        match (&lhs, &rhs) {
            (Basic::Type, _) => construct(rhs),
            (_, Basic::Type) => construct(lhs),

            (Basic::Value(a), Basic::Value(b)) if a == b => construct(lhs),

            (Basic::Value(a), Basic::Relation(op, b)) if rel_op(a, *op, b) => construct(lhs),
            (Basic::Relation(op, a), Basic::Value(b)) if rel_op(a, *op, b) => construct(rhs),

            (Basic::Relation(opa, a), Basic::Relation(opb, b)) => {
                match (
                    rel_op(a, *opb, b),
                    rel_op(b, *opa, a),
                    rel_op(a, *opa, b),
                    rel_op(b, *opb, a),
                ) {
                    (true, true, true, true) => construct(Basic::Value(a.clone())),
                    (true, true, _, _) => Value::Conjunction(vec![
                        construct(Basic::Relation(*opa, a.clone())).into(),
                        construct(Basic::Relation(*opb, b.clone())).into(),
                    ]),
                    (true, false, _, _) if a != b => construct(Basic::Relation(*opa, a.clone())),
                    (false, true, _, _) if a != b => construct(Basic::Relation(*opb, b.clone())),
                    (_, _, _, _) => {
                        match_basic!((*opa, a), (*opb, b), construct, Value::Conjunction, {
                            ((!=_) & (<=b)) => (<b),
                            ((!=_) & (>=b)) => (>b),
                            ((!=_) & (< b)) => (<b),
                            ((!=_) & (> b)) => (>b),
                            ((<a)  & (<=_)) => (<a),
                            ((>a)  & (>=_)) => (>a),
                            ((<_)  & (>=_)) => (_|_),
                            ((>_)  & (<=_)) => (_|_),
                            ((!~_) & (=~_)) => (_|_),
                        })
                    }
                }
            }

            (_, _) => Self::Bottom,
        }
    }

    fn join_basic<T: PartialEq + Clone + Display>(
        lhs: Basic<T>,
        rhs: Basic<T>,
        construct: fn(Basic<T>) -> Self,
        rel_op: fn(&T, RelOp, &T) -> bool,
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
                match (
                    rel_op(a, *opb, b),
                    rel_op(b, *opa, a),
                    rel_op(a, *opa, b),
                    rel_op(b, *opb, a),
                ) {
                    (true, true, _, _) => construct(Basic::Type),
                    (false, false, _, _) if a != b => Value::Disjunction(vec![
                        construct(Basic::Relation(*opa, a.clone())).into(),
                        construct(Basic::Relation(*opb, b.clone())).into(),
                    ]),
                    (false, true, _, _) if a != b => construct(Basic::Relation(*opa, a.clone())),
                    (true, false, _, _) if a != b => construct(Basic::Relation(*opb, b.clone())),
                    (_, _, _, _) => {
                        match_basic!((*opa, a), (*opb, b), construct, Value::Disjunction, {
                            ((!=_) | (<=_)) => (_),
                            ((!=_) | (>=_)) => (_),
                            ((!=a) | (< _)) => (!=a),
                            ((!=a) | (> _)) => (!=a),
                            ((<_)  | (> b)) => (!=b),
                            ((<_)  | (<=b)) => (<=b),
                            ((>_)  | (>=b)) => (>=b),
                            ((<_)  | (>=_)) => (_),
                            ((>_)  | (<=_)) => (_),
                            ((!~_) | (=~_)) => (_),
                        })
                    }
                }
            }

            (_, _) => Self::Disjunction(vec![construct(lhs).into(), construct(rhs).into()]),
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
                .map_or(Self::Top.into(), |ff| ff.value.clone());
            fields.push(Field {
                label: f.label.clone(),
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
}

#[test]
fn test_basic_meets() {
    use crate::assert_cue;

    assert_cue!((_) & (_|_) == (_|_));
    assert_cue!((_) & (null) == (null));
    assert_cue!((_) & (bool) == (bool));
    assert_cue!((_) & (true) == (true));
    assert_cue!((bool) & (true) == (true));
}

#[test]
fn test_int_infimum() {
    use crate::assert_cue;

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
    use crate::assert_cue;

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
fn test_str_infimum() {
    use crate::assert_cue;

    assert_cue!((string) & (string) == (string));
    assert_cue!((string) & ("hello") == ("hello"));
    assert_cue!((string) & (=~"hello") == (=~"hello"));

    assert_cue!((!~"hello") & (=~"hello") == (_|_));
}

#[test]
fn test_str_supremum() {
    use crate::assert_cue;

    assert_cue!((string) | (string) == (string));
    assert_cue!((string) | ("hello") == (string));
    assert_cue!((string) | (=~"hello") == (string));

    assert_cue!((!~"hello") | (=~"hello") == (string));
}

#[test]
fn test_struct_infimum() {
    use crate::assert_cue;

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
    use crate::assert_cue;

    assert_cue!(([]) & ([]) == ([]));
    assert_cue!(([]) & ([(1), (2), (3)]) == (_|_));
    assert_cue!(([(int), (bool), (string)]) & ([(1), (bool), (=~"hello")]) == ([(1), (bool), (=~"hello")]));
}

#[test]
fn test_disjunct_infimum() {
    use crate::assert_cue;

    assert_cue!(((string) | (int) | (bool)) & (int) == (int));
    assert_cue!(((string) | (int) | (bool)) & (null) == (_|_));
    assert_cue!(((string) | (int) | (bool)) & (>2) == (>2));
    assert_cue!(((string) | (int) | (bool)) & (2) == (2));
    assert_cue!(((string) | (int) | (bool)) & ((2) | (true)) == ((2) | (true)));
    assert_cue!(((string) | (2) | (bool)) & ((int) | (true)) == ((2) | (true)));
}

#[test]
fn test_format() {
    use crate::cue_val;

    assert_eq!(format!("{}", cue_val!((int))), "int");
    assert_eq!(format!("{}", cue_val!((1))), "1");
    assert_eq!(format!("{}", cue_val!((>1))), ">1");
    assert_eq!(format!("{}", cue_val!((<=1))), "<=1");
    assert_eq!(format!("{}", cue_val!((string))), "string");
    assert_eq!(format!("{}", cue_val!(("hello"))), "\"hello\"");
    assert_eq!(format!("{}", cue_val!((!="hello"))), "!=\"hello\"");
    assert_eq!(format!("{}", cue_val!(({}))), "{}");
    assert_eq!(format!("{}", cue_val!(({a: (1)}))), "{a: 1}");
    assert_eq!(
        format!("{}", cue_val!(({a: (1), b: ({c: ("d")})}))),
        "{a: 1,\nb: {c: \"d\"}}"
    );
    assert_eq!(format!("{}", cue_val!(([]))), "[]");
    assert_eq!(format!("{}", cue_val!(([(int)]))), "[int]");
    assert_eq!(
        format!("{}", cue_val!(([(int), (true), ("hello")]))),
        "[int,\ntrue,\n\"hello\"]"
    );
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Value::Bool(Some(value))
    }
}

impl From<i64> for Value {
    fn from(value: i64) -> Self {
        Value::Int(Basic::Value(value))
    }
}
impl From<(RelOp, i64)> for Value {
    fn from(value: (RelOp, i64)) -> Value {
        Value::Int(Basic::Relation(value.0, value.1))
    }
}

impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Value::Float(Basic::Value(value))
    }
}
impl From<(RelOp, f64)> for Value {
    fn from(value: (RelOp, f64)) -> Value {
        Value::Float(Basic::Relation(value.0, value.1))
    }
}

impl From<&str> for Value {
    fn from(value: &str) -> Self {
        Value::String(Basic::Value(value.into()))
    }
}
impl From<(RelOp, &str)> for Value {
    fn from(value: (RelOp, &str)) -> Value {
        Value::String(Basic::Relation(value.0, value.1.into()))
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        macro_rules! write_separated {
            ($items:ident, $sep:literal) => {
                write_separated!($items, "", $sep, "")
            };
            ($items:ident, $start:literal, $sep:literal, $end: literal) => {{
                Display::fmt($start, f)
                    .and_then(|_| {
                        $items.split_last().map_or(Ok(()), |(last, items)| {
                            items
                                .iter()
                                .fold(Ok(()), |acc, item| {
                                    acc.and_then(|_| Display::fmt(item, f))
                                        .and_then(|_| Display::fmt($sep, f))
                                })
                                .and_then(|_| Display::fmt(last, f))
                        })
                    })
                    .and_then(|_| Display::fmt($end, f))
            }};
        }

        match self {
            Value::Top => write!(f, "_"),

            Value::Disjunction(items) => write_separated!(items, " | "),
            Value::Conjunction(items) => write_separated!(items, " & "),

            Value::Struct(items) => write_separated!(items, "{", ",\n", "}"),

            Value::List(items) => write_separated!(items, "[", ",\n", "]"),

            Value::String(Basic::Type) => write!(f, "string"),
            Value::Bytes(Basic::Type) => write!(f, "bytes"),
            Value::Float(Basic::Type) => write!(f, "float"),
            Value::Int(Basic::Type) => write!(f, "int"),

            Value::String(Basic::Value(val)) => write!(f, "\"{}\"", val),
            Value::Bytes(Basic::Value(val)) => write!(f, "'{}'", val),
            Value::Float(Basic::Value(val)) => Display::fmt(val, f),
            Value::Int(Basic::Value(val)) => Display::fmt(val, f),

            Value::String(Basic::Relation(op, val)) => write!(f, "{}\"{}\"", op, val),
            Value::Bytes(Basic::Relation(op, val)) => write!(f, "{}'{}'", op, val),
            Value::Float(Basic::Relation(op, val)) => write!(f, "{}{}", op, val),
            Value::Int(Basic::Relation(op, val)) => write!(f, "{}{}", op, val),

            Value::Bool(None) => write!(f, "bool"),
            Value::Bool(Some(true)) => write!(f, "true"),
            Value::Bool(Some(false)) => write!(f, "false"),

            Value::Null => write!(f, "null"),

            Value::Bottom => write!(f, "_|_"),
        }
    }
}
