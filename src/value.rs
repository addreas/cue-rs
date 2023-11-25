use std::collections::HashMap;
use std::{fmt::Debug, fmt::Display, rc::Rc};

use regex::Regex;

use crate::ast;

use super::adt::op::RelOp;
use super::ast::{FieldConstraint, IdentKind};
use super::match_basic;

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Top,

    Disjunction(Rc<[Rc<Value>]>),
    Conjunction(Rc<[Rc<Value>]>),

    Struct(Rc<[Field]>, bool),
    List(Rc<[Rc<Value>]>),

    String(Option<Rc<str>>),
    Bytes(Option<Rc<str>>),
    // StringInterpolation(Rc<[Rc<str>]>, Rc<[Rc<Value>]>),
    // BytesInterpolation(Rc<[Rc<str>]>, Rc<[Rc<Value>]>),

    Bound(RelOp, Rc<Value>),

    Float(Option<f64>),
    Int(Option<i64>),

    Bool(Option<bool>),

    Null,

    Bottom,

    // Reference(??) ??
    // Unresolved(ast::Expr) ??
}

#[derive(Debug, PartialEq, Clone)]
pub enum Field {
    Defined(FieldName, Rc<Value>),
    Constraint(FieldName, FieldConstraint, Rc<Value>),
    Pattern(Rc<Value>, fn(Rc<str>) -> Rc<Value>),
    Embedding(Rc<Value>),
    // Default(Rc<Value>),
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum FieldName {
    Ident(ast::Ident),
    String(Rc<str>),
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
            (Self::Bool(Some(a)), Self::Bool(Some(b))) if a != b => Self::Bottom.into(),

            (Self::Int(None), Self::Int(_)) => other,
            (Self::Int(_), Self::Int(None)) => self,
            (Self::Int(Some(a)), Self::Int(Some(b))) if a != b => Self::Bottom.into(),

            (Self::Float(None), Self::Float(_)) => other,
            (Self::Float(_), Self::Float(None)) => self,
            (Self::Float(Some(a)), Self::Float(Some(b))) if a != b => Self::Bottom.into(),

            (Self::String(None), Self::String(_)) => other,
            (Self::String(_), Self::String(None)) => self,
            (Self::String(Some(a)), Self::String(Some(b))) if a != b => Self::Bottom.into(),

            (Self::Bytes(None), Self::Bytes(_)) => other,
            (Self::Bytes(_), Self::Bytes(None)) => self,
            (Self::Bytes(Some(a)), Self::Bytes(Some(b))) if a != b => Self::Bottom.into(),

            (Self::Disjunction(lhs), _) => Self::meet_disjunction(lhs.clone(), other),
            (_, Self::Disjunction(rhs)) => Self::meet_disjunction(rhs.clone(), self),

            (Self::Bound(op, a), _) => Self::meet_bound(op, a.clone(), other),
            (_, Self::Bound(op, b)) => Self::meet_bound(op, b.clone(), self),

            (Self::Struct(lhs, _), Self::Struct(rhs, _)) => Self::meet_structs(lhs.clone(), rhs.clone()).into(),
            (Self::List(lhs), Self::List(rhs)) => Self::meet_lists(lhs.clone(), rhs.clone()).into(),

            (Self::Struct(fields, _), _) if fields.iter().all(Field::visible) => Self::Disjunction([Self::Struct(fields.clone(), false).into(), other].into()).into(),
            (_, Self::Struct(fields, _)) if fields.iter().all(Field::visible) => Self::Disjunction([Self::Struct(fields.clone(), false).into(), self].into()).into(),

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

            (Self::Bool(Some(a)), Self::Bool(Some(b))) if a != b => Self::Bool(None).into(),

            (Self::Int(None), Self::Int(_)) => self,
            (Self::Int(_), Self::Int(None)) => other,

            (Self::Float(None), Self::Float(_)) => self,
            (Self::Float(_), Self::Float(None)) => other,

            (Self::String(None), Self::String(_)) => self,
            (Self::String(_), Self::String(None)) => other,

            (Self::Bytes(None), Self::Bytes(_)) => self,
            (Self::Bytes(_), Self::Bytes(None)) => other,

            (Self::Bound(op, a), _) => Self::join_bound(op, a.clone(), other),
            (_, Self::Bound(op, b)) => Self::join_bound(op, b.clone(), self),

            (Self::Disjunction(lhs), _) => Self::join_disjunction(lhs.clone(), other).into(),
            (_, Self::Disjunction(rhs)) => Self::join_disjunction(rhs.clone(), self).into(),

            (_, _) => Self::Disjunction([self, other].into()).into(),
        }
    }

    fn meet_bound(op: &RelOp, bounding: Rc<Self>, constrained: Rc<Self>) -> Rc<Self> {
        #[rustfmt::skip]
        match (bounding.as_ref(), constrained.as_ref()) {

            (Self::Int(_), Self::Int(None)) => Self::Bound(*op, bounding).into(),
            (Self::Float(_), Self::Float(None)) => Self::Bound(*op, bounding).into(),
            (Self::String(_), Self::String(None)) => Self::Bound(*op, bounding).into(),
            (Self::Bytes(_), Self::Bytes(None)) => Self::Bound(*op, bounding).into(),

            (Self::Int(Some(a)), Self::Int(Some(b))) if Self::rel_op_ord(b, op, a) => constrained,
            (Self::Float(Some(a)), Self::Float(Some(b))) if Self::rel_op_ord(b, op, a) => constrained,
            // (Self::Int(Some(a)), Self::Float(Some(b))) if Self::rel_op_ord(f64(a), op, b) => constrained,
            // (Self::Float(Some(a)), Self::Int(Some(b))) if Self::rel_op_ord(a, op, f64(b)) => constrained,
            (Self::String(Some(a)), Self::String(Some(b))) if Self::rel_op_str(b, op, a) => constrained,
            (Self::Bytes(Some(a)), Self::Bytes(Some(b))) if Self::rel_op_str(b, op, a) => constrained,

            (_, Self::Bound(opb, other)) => {
                match (bounding.as_ref(), other.as_ref()) {
                    (Value::Int(Some(a)), Value::Int(Some(b))) => Self::meet_bound_bound(*op, *a, *opb, *b, Self::Int, Self::rel_op_ord).into(),
                    (Value::Float(Some(a)), Value::Float(Some(b))) => Self::meet_bound_bound(*op, *a, *opb, *b, Self::Float, Self::rel_op_ord).into(),
                    (Value::String(Some(a)), Value::String(Some(b))) => Self::meet_bound_bound(*op, a.clone(), *opb, b.clone(), Self::String, Self::rel_op_str).into(),
                    (Value::Bytes(Some(a)), Value::Bytes(Some(b))) => Self::meet_bound_bound(*op, a.clone(), *opb, b.clone(), Self::Bytes, Self::rel_op_str).into(),
                    _ => Self::Bottom.into(),
                }
            }

            _ => Self::Bottom.into(),
        }
    }

    fn meet_bound_bound<T: PartialEq>(
        opa: RelOp,
        a: T,
        opb: RelOp,
        b: T,
        construct: fn(Option<T>) -> Self,
        rel_op: fn(&T, &RelOp, &T) -> bool,
    ) -> Self {
        match (
            rel_op(&a, &opb, &b),
            rel_op(&b, &opa, &a),
            rel_op(&a, &opa, &b),
            rel_op(&b, &opb, &a),
        ) {
            (true, true, true, true) => construct(Some(a)),
            (true, true, _, _) => Value::Conjunction([
                Self::Bound(opa, construct(Some(a)).into()).into(),
                Self::Bound(opb, construct(Some(b)).into()).into(),
            ].into()),
            (true, false, _, _) if a != b => Self::Bound(opa, construct(Some(a)).into()),
            (false, true, _, _) if a != b => Self::Bound(opb, construct(Some(b)).into()),
            (_, _, _, _) => {
                match_basic!((opa, a), (opb, b), construct, Value::Conjunction, {
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

    fn join_bound(op: &RelOp, bounding: Rc<Self>, constrained: Rc<Self>) -> Rc<Self> {
        #[rustfmt::skip]
        match (bounding.as_ref(), constrained.as_ref()) {
            (Self::Int(a), Self::Int(b)) if *op == RelOp::NotEqual && a == b => Self::Int(None).into(),
            (Self::Float(a), Self::Float(b)) if *op == RelOp::NotEqual && a == b => Self::Float(None).into(),
            (Self::String(a), Self::String(b)) if *op == RelOp::NotEqual && a == b => Self::String(None).into(),
            (Self::Bytes(a), Self::Bytes(b)) if *op == RelOp::NotEqual && a == b => Self::String(None).into(),

            (Self::Int(_), Self::Int(None)) => constrained,
            (Self::Float(_), Self::Float(None)) => constrained,
            (Self::String(_), Self::String(None)) => constrained,
            (Self::Bytes(_), Self::Bytes(None)) => constrained,


            (Self::Int(Some(a)), Self::Int(Some(b))) if Self::rel_op_ord(b, op, a) => Self::Bound(*op, bounding).into(),
            (Self::Float(Some(a)), Self::Float(Some(b))) if Self::rel_op_ord(b, op, a) => Self::Bound(*op, bounding).into(),
            // (Self::Int(Some(a)), Self::Float(Some(b))) if Self::rel_op_ord(f64(a), op, b) => Self::Bound(*op, bounding).into(),
            // (Self::Float(Some(a)), Self::Int(Some(b))) if Self::rel_op_ord(a, op, f64(b)) => Self::Bound(*op, bounding).into(),
            (Self::String(Some(a)), Self::String(Some(b))) if Self::rel_op_str(b, op, a) => Self::Bound(*op, bounding).into(),
            (Self::Bytes(Some(a)), Self::Bytes(Some(b))) if Self::rel_op_str(b, op, a) => Self::Bound(*op, bounding).into(),

            (_, Self::Bound(opb, other)) => {
                match (bounding.as_ref(), other.as_ref()) {
                    (Value::Int(Some(a)), Value::Int(Some(b))) => Self::join_bound_bound(*op, *a, *opb, *b, Self::Int, Self::rel_op_ord).into(),
                    (Value::Float(Some(a)), Value::Float(Some(b))) => Self::join_bound_bound(*op, *a, *opb, *b, Self::Float, Self::rel_op_ord).into(),
                    (Value::String(Some(a)), Value::String(Some(b))) => Self::join_bound_bound(*op, a.clone(), *opb, b.clone(), Self::String, Self::rel_op_str).into(),
                    (Value::Bytes(Some(a)), Value::Bytes(Some(b))) => Self::join_bound_bound(*op, a.clone(), *opb, b.clone(), Self::Bytes, Self::rel_op_str).into(),
                    _ => Self::Disjunction([Self::Bound(*op, bounding).into(), constrained].into()).into(),
                }
            }

            _ => Self::Disjunction([constrained, Self::Bound(*op, bounding).into()].into()).into(),
        }
    }

    fn join_bound_bound<T: PartialEq>(
        opa: RelOp,
        a: T,
        opb: RelOp,
        b: T,
        construct: fn(Option<T>) -> Self,
        rel_op: fn(&T, &RelOp, &T) -> bool,
    ) -> Self {
        match (
            rel_op(&a, &opb, &b),
            rel_op(&b, &opa, &a),
            rel_op(&a, &opa, &b),
            rel_op(&b, &opb, &a),
        ) {
            (true, true, _, _) => construct(None),
            (false, false, _, _) if a != b => Value::Disjunction([
                Self::Bound(opa, construct(Some(a)).into()).into(),
                Self::Bound(opb, construct(Some(b)).into()).into(),
            ].into()),
            (false, true, _, _) if a != b => Self::Bound(opa, construct(Some(a)).into()),
            (true, false, _, _) if a != b => Self::Bound(opb, construct(Some(b)).into()),
            (_, _, _, _) => {
                match_basic!((opa, a), (opb, b), construct, Value::Disjunction, {
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

    fn rel_op_str(lhs: &Rc<str>, op: &RelOp, rhs: &Rc<str>) -> bool {
        match op {
            RelOp::NotEqual => lhs != rhs,

            RelOp::GreaterEqual => lhs >= rhs,
            RelOp::GreaterThan => lhs > rhs,
            RelOp::LessEqual => lhs <= rhs,
            RelOp::LessThan => lhs < rhs,

            RelOp::Match => Regex::new(rhs.as_ref()).unwrap().is_match(lhs.as_ref()),
            RelOp::NotMatch => !Regex::new(rhs.as_ref()).unwrap().is_match(lhs.as_ref()),
        }
    }

    fn rel_op_ord<T: PartialEq + PartialOrd>(lhs: &T, op: &RelOp, rhs: &T) -> bool {
        match op {
            RelOp::NotEqual => lhs != rhs,

            RelOp::GreaterEqual => lhs >= rhs,
            RelOp::GreaterThan => lhs > rhs,
            RelOp::LessEqual => lhs <= rhs,
            RelOp::LessThan => lhs < rhs,

            _ => false,
        }
    }

    fn meet_structs(lhs: Rc<[Field]>, rhs: Rc<[Field]>) -> Value {
        let mut fields: Vec<Field> = vec![];

        for f in lhs.iter() {
            match f {
                Field::Defined(key, value) => {
                    let rhs_matches = rhs.iter().filter_map(|ff| match ff {
                        Field::Defined(ffkey, val) if ffkey == key => Some(val.clone()),
                        Field::Constraint(ffkey, _, val) if ffkey == key => Some(val.clone()),
                        Field::Pattern(_, _) => todo!(),
                        Field::Embedding(_) => todo!(),
                        _ => todo!(),
                    });
                    let res = rhs_matches.fold(value.clone(), |x, y| y.meet(x));
                    fields.push(Field::Defined(key.clone(), res))
                },
                Field::Constraint(_, _, _) => todo!(),

                Field::Pattern(_, _) => todo!(),
                Field::Embedding(_) => todo!(),
            }
        }


        // // let rhs_value = rhs
        // //     .iter()
        // //     .find(|ff| ff.label == f.label) // TODO: consider bulk fields
        // //     .map_or(Self::Top.into(), |ff| ff.value.clone());
        // // fields.push(Field {
        // //     label: f.label.clone(),
        // //     value: f.value.clone().meet(rhs_value),
        // // });
        // for f in rhs.iter() {
        //     if lhs.iter().any(|ff| ff.label == f.label) {
        //         // TODO: consider bulk fields
        //         continue;
        //     }
        //     fields.push(f.clone())
        // }

        Self::Struct(fields.into(), false)
    }

    fn meet_lists(lhs: Rc<[Rc<Value>]>, rhs: Rc<[Rc<Value>]>) -> Value {
        if lhs.len() != rhs.len() {
            Self::Bottom
        } else {
            Self::List(
                lhs.into_iter()
                    .zip(rhs.into_iter())
                    .map(|(l, r)| l.clone().meet(r.clone()))
                    .collect(),
            )
        }
    }

    fn meet_disjunction(items: Rc<[Rc<Value>]>, operand: Rc<Value>) -> Rc<Value> {
        let res: Rc<[_]> = items
            .into_iter()
            .map(|i| i.clone().meet(operand.clone()))
            .collect();

        let non_bottoms: Rc<[_]> = res
            .into_iter()
            .filter(|i| !i.is_bottom())
            .map(|i| i.clone())
            .collect();

        match &non_bottoms[..] {
            [] => Self::Bottom.into(),
            [i] => i.clone(),
            items => Self::Disjunction(items.into()).into(),
        }
    }

    fn join_disjunction(existing: Rc<[Rc<Value>]>, extension: Rc<Value>) -> Value {
        for val in existing.iter() {
            if extension.clone() == *val {
                return Self::Disjunction(existing);
            }
        }

        Self::Disjunction([existing, [extension].into()].concat().into())
    }

    pub fn is_bottom(self: &Self) -> bool {
        *self == Self::Bottom
    }
}

impl Field {
    fn visible(&self) -> bool {
        match self {
            Field::Defined(_, _) => true,
            _ => false,
        }
    }
}

#[test]
fn test_basic_meets() {
    use crate::assert_cue;

    assert_cue!((_) & (_ | _) == (_ | _));
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
    assert_cue!(({}) & ({(a): (1), (b): (2)}) == ({(a): (1), (b): (2)}));
    assert_cue!(({(a): (1)}) & ({(b): (2)}) == ({(a): (1), (b): (2)}));
    assert_cue!(({(a): (1), (b): (2)}) & ({}) == ({(a): (1), (b): (2)}));
    assert_cue!(({(a): (int)}) & ({(a): (1)}) == ({(a): (1)}));
    assert_cue!(({(a): (float)}) & ({(a): (1.0)}) == ({(a): (1.0)}));
    assert_cue!(({(a): (string)}) & ({(a): ("hello")}) == ({(a): ("hello")}));
    assert_cue!(({(a): (int)}) & ({(a): ("hello")}) == ({(a): (_|_)}));
    assert_cue!(({(a): (int)}) & ({(b): ({(c): ("d")})}) == ({(a): (int), (b): ({(c): ("d")})}));
}

#[test]
fn test_list_infimum() {
    use crate::assert_cue;

    assert_cue!(([]) & ([]) == ([]));
    assert_cue!(([]) & ([(1), (2), (3)]) == (_ | _));
    assert_cue!(([(int), (bool), (string)]) & ([(1), (bool), (=~"hello")]) == ([(1), (bool), (=~"hello")]));
}

#[test]
fn test_disjunct_infimum() {
    use crate::assert_cue;

    assert_cue!(((string) | (int) | (bool)) & (int) == (int));
    assert_cue!(((string) | (int) | (bool)) & (null) == (_ | _));
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
    assert_eq!(format!("{}", cue_val!(({(a): (1)}))), "{a: 1}");
    assert_eq!(
        format!("{}", cue_val!(({(a): (1), (b): ({(c): ("d")})}))),
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
        Value::Int(Some(value))
    }
}
impl From<(RelOp, i64)> for Value {
    fn from(value: (RelOp, i64)) -> Value {
        Value::Bound(value.0, Value::Int(Some(value.1)).into())
    }
}

impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Value::Float(Some(value))
    }
}
impl From<(RelOp, f64)> for Value {
    fn from(value: (RelOp, f64)) -> Value {
        Value::Bound(value.0, Value::Float(Some(value.1)).into())
    }
}

impl From<&str> for Value {
    fn from(value: &str) -> Self {
        Value::String(Some(value.into()))
    }
}
impl From<(RelOp, &str)> for Value {
    fn from(value: (RelOp, &str)) -> Value {
        Value::Bound(value.0, Value::String(Some(value.1.into())).into())
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

            Value::Struct(items, _) => write_separated!(items, "{", ",\n", "}"),

            Value::List(items) => write_separated!(items, "[", ",\n", "]"),

            Value::String(None) => write!(f, "string"),
            Value::Bytes(None) => write!(f, "bytes"),
            Value::Float(None) => write!(f, "float"),
            Value::Int(None) => write!(f, "int"),

            Value::String(Some(val)) => write!(f, "\"{}\"", val),
            Value::Bytes(Some(val)) => write!(f, "'{}'", val),
            Value::Float(Some(val)) => Display::fmt(val, f),
            Value::Int(Some(val)) => Display::fmt(val, f),

            Value::Bound(op, val) => write!(f, "{}{}", op, val),

            Value::Bool(None) => write!(f, "bool"),
            Value::Bool(Some(true)) => write!(f, "true"),
            Value::Bool(Some(false)) => write!(f, "false"),

            Value::Null => write!(f, "null"),

            Value::Bottom => write!(f, "_|_"),
        }
    }
}

impl Display for Field {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Field::Defined(key, val) => write!(f, "{key}: {val}"),
            Field::Constraint(key, constraint, val) => {
                let postfix = match constraint {
                    FieldConstraint::Optional => "?",
                    FieldConstraint::Required => "!",
                };
                write!(f, "{}{}: {}", key, postfix, val)
            }
            Field::Pattern(pat, val) => write!(f, "[{}]: {}", pat, val("".into())),
            Field::Embedding(v) => Display::fmt(v, f),
        }
    }
}

impl Display for FieldName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FieldName::Ident(ident) => {
                let kind = match ident.kind {
                    None => "",
                    Some(IdentKind::Hidden) => "_",
                    Some(IdentKind::Definition) => "#",
                    Some(IdentKind::HiddenDefinition) => "_#",
                };
                write!(f, "{}{}", kind, ident.name)
            }
            FieldName::String(s) => write!(f, r#""{}""#, s),
        }
    }
}
