use std::{
    cmp::Ordering,
    rc::{Rc, Weak},
};

use super::op::Op;

#[derive(Debug, Clone)]
pub enum Value {
    Top,

    Disjunct(Vec<Rc<Value>>),
    Conjunct(Vec<Rc<Value>>),

    Struct(Vec<Field>),
    List(Vec<Rc<Value>>),
    // Func,
    Reference(Weak<Value>, Vec<BasicValue>),

    BasicValue(BasicValue),

    Bottom,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BasicType {
    Bytes,
    String,
    Float,
    Int,
    Bool,
}

impl Value {
    pub fn disjunct_from_iter<I: IntoIterator<Item = Rc<Value>>>(iter: I) -> Self {
        Self::Disjunct(Vec::from_iter(iter))
    }
    pub fn conjunct_from_iter<I: IntoIterator<Item = Rc<Value>>>(iter: I) -> Self {
        Self::Conjunct(Vec::from_iter(iter))
    }
    pub fn struct_from_iter<I: IntoIterator<Item = Field>>(iter: I) -> Self {
        Self::Struct(Vec::from_iter(iter))
    }
    pub fn list_from_iter<I: IntoIterator<Item = Rc<Value>>>(iter: I) -> Self {
        Self::List(Vec::from_iter(iter))
    }
    pub fn reference(inner: Weak<Value>, path: Vec<BasicValue>) -> Self {
        Self::Reference(inner, path)
    }
    pub fn basic_value(inner: BasicValue) -> Self {
        Self::BasicValue(inner)
    }
    pub fn as_rc(self) -> Rc<Self> {
        Rc::new(self)
    }

    pub fn disjunct_with(self: Rc<Self>, other: Rc<Value>) -> Rc<Self> {
        match (&*self, &*other) {
            (Self::Disjunct(lhs), Self::Disjunct(rhs)) => {
                let parts = lhs.into_iter().chain(rhs.into_iter()).map(Rc::clone);
                Self::disjunct_from_iter(parts).as_rc()
            }
            (Self::Disjunct(lhs), _) => {
                let parts = lhs
                    .into_iter()
                    .chain(vec![&other].into_iter())
                    .map(Rc::clone);
                Self::disjunct_from_iter(parts).as_rc()
            }
            (_, Self::Disjunct(rhs)) => {
                let parts = rhs
                    .into_iter()
                    .chain(vec![&self].into_iter())
                    .map(Rc::clone);
                Self::disjunct_from_iter(parts).as_rc()
            }
            _ => Self::Disjunct(vec![self, other]).as_rc(),
        }
    }

    pub fn conjunct_with(self: Rc<Self>, other: Rc<Value>) -> Rc<Self> {
        match (&*self, &*other) {
            (Self::Conjunct(lhs), Self::Conjunct(rhs)) => {
                let parts = lhs.into_iter().chain(rhs.into_iter()).map(Rc::clone);
                Self::disjunct_from_iter(parts).as_rc()
            }
            (Self::Conjunct(lhs), _) => {
                let parts = lhs
                    .into_iter()
                    .chain(vec![&other].into_iter())
                    .map(Rc::clone);
                Self::disjunct_from_iter(parts).as_rc()
            }
            (_, Self::Conjunct(rhs)) => {
                let parts = rhs
                    .into_iter()
                    .chain(vec![&self].into_iter())
                    .map(Rc::clone);
                Self::disjunct_from_iter(parts).as_rc()
            }
            _ => Self::Conjunct(vec![self, other]).as_rc(),
        }
    }

    pub fn follow_reference(self: Rc<Self>, path: Vec<BasicValue>) -> Option<Rc<Value>> {
        let mut current_root = self;
        for segment in path.into_iter() {
            let next = match &*current_root {
                Self::Struct(flds) => flds
                    .into_iter()
                    .find(|f| segment == f.label)
                    .map(|f| Rc::clone(&f.value)),
                Self::List(items) => match segment {
                    BasicValue::Int(ValueType::Concrete(index)) => {
                        items.get(index as usize).map(Rc::clone)
                    }
                    _ => None,
                },
                _ => None,
            };

            match next {
                Some(x) => current_root = x,
                None => return None,
            };
        }

        Some(current_root)
    }

    pub fn is_bottom(&self) -> bool {
        matches!(&self, Self::Bottom)
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        todo!()
    }
}

#[derive(Debug, Clone)]
pub struct Field {
    label: BasicValue,
    optional: bool,
    value: Rc<Value>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BasicValue {
    Bytes(ValueType<usize>),
    String(ValueType<usize>),
    Float(ValueType<f64>),
    Int(ValueType<i64>),
    Bool(ValueType<bool>),
    Null,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ValueType<T: PartialEq + Copy> {
    Type,
    Constraint(Op, T),
    Concrete(T),
}

trait Lattice {
    // supremum, least upper bound, anti-unification
    fn join(self: Rc<Self>, other: Rc<Self>) -> Rc<Self>;

    // infimum, greatest lower bound, unification
    fn meet(self: Rc<Self>, other: Rc<Self>) -> Rc<Self>;
}

impl Lattice for Value {
    fn meet(self: Rc<Self>, other: Rc<Self>) -> Rc<Self> {
        match (&*self, &*other) {
            (Self::Top, _) => other,
            (_, Self::Top) => self,

            (Self::Bottom, _) => self,
            (_, Self::Bottom) => other,

            (Self::Reference(lhs_root, lhs_path), _) => lhs_root
                .upgrade()
                .and_then(|root| root.follow_reference(lhs_path.clone()))
                .map_or_else(|| Self::Bottom.as_rc(), |lhs| lhs.meet(other)),

            (Self::Disjunct(items), _) => Self::disjunct_from_iter(
                items
                    .into_iter()
                    .map(|i| Rc::clone(i).meet(Rc::clone(&other)))
                    .filter(|i| !i.is_bottom()),
            )
            .as_rc(),
            (Self::Conjunct(items), _) => items
                .into_iter()
                .fold(other, |acc, i| Rc::clone(i).meet(acc)),

            (Self::Struct(lhs), Self::Struct(rhs)) if lhs.len() <= rhs.len() => {
                let met = rhs
                    .into_iter()
                    .map(|i| {
                        let new_value = Rc::clone(&i.value).meet(
                            lhs.iter()
                                .find(|ii| i.label == ii.label)
                                .map_or_else(|| Self::Bottom.as_rc(), |ii| Rc::clone(&ii.value)),
                        );
                        i.with_value(new_value)
                    })
                    .collect::<Vec<_>>();

                if met.iter().any(|i| i.value.is_bottom()) {
                    Self::Bottom.as_rc()
                } else {
                    Self::Struct(met).as_rc()
                }
            }
            (Self::List(lhs), Self::List(rhs)) => Self::list_from_iter(
                lhs.into_iter()
                    .zip(rhs.into_iter())
                    .map(|(l, r)| Rc::clone(l).meet(Rc::clone(r))),
            )
            .as_rc(),

            (lhs, rhs @ Self::Struct(_)) => other.meet(self),
            (lhs, rhs @ Self::Reference(_, _)) => other.meet(self),
            (lhs, rhs @ Self::Disjunct(_)) => other.meet(self),
            (lhs, rhs @ Self::Conjunct(_)) => other.meet(self),

            // TODO: basic value
            _ => Self::Bottom.as_rc(),
        }
    }

    fn join(self: Rc<Self>, other: Rc<Self>) -> Rc<Self> {
        match (&*self, &*other) {
            (a, b) if a == b => self,

            (Self::Bottom, _) => other,
            (_, Self::Bottom) => self,

            (Self::Top, _) => self,
            (_, Self::Top) => other,

            (Self::Reference(root, path), _) => root
                .upgrade()
                .and_then(|r| r.follow_reference(path.clone()))
                .map_or_else(|| Self::Bottom.as_rc(), |v| v.join(other)),
            (_, _rhs @ Self::Reference(_, _)) => other.join(self),

            // TODO: should this resolve recursively?
            _ => self.disjunct_with(other),
        }
    }
}

trait Comparable {
    // either the constraint works and the result is Ordering::Greater
    // or the constraint is invalid and the items are not comparable
    fn constrains(lhs: &Self, op: &Op, rhs: &Self) -> Option<Ordering>;

    // lhs is Ordering::Greater if it's constraint is "weaker"
    fn compare_constraints(lhs: &Self, lop: &Op, rop: &Op, rhs: &Self) -> Option<Ordering>;
}

#[test]
fn test_compare_constraints_i64() {
    assert_eq!(
        i64::compare_constraints(&1, &Op::GreaterThan, &Op::GreaterThan, &2),
        Some(Ordering::Greater)
    );
    assert_eq!(
        i64::compare_constraints(&1, &Op::GreaterThan, &Op::GreaterThan, &1),
        Some(Ordering::Equal)
    );
    assert_eq!(
        i64::compare_constraints(&2, &Op::GreaterThan, &Op::GreaterThan, &1),
        Some(Ordering::Less)
    );

    assert_eq!(
        i64::compare_constraints(&1, &Op::GreaterThan, &Op::GreaterEqual, &2),
        Some(Ordering::Equal)
    );
    assert_eq!(
        i64::compare_constraints(&1, &Op::GreaterThan, &Op::GreaterEqual, &1),
        Some(Ordering::Less)
    );
    // >1 .compare_constraints >2 => Some(Ordering::Greater)
}

impl Comparable for i64 {
    fn constrains(lhs: &i64, op: &Op, rhs: &i64) -> Option<Ordering> {
        match op {
            Op::NotEqual => lhs != rhs,
            Op::LessThan => lhs < rhs,
            Op::LessEqual => lhs <= rhs,
            Op::GreaterThan => lhs >= rhs,
            Op::GreaterEqual => lhs >= rhs,
            _ => false,
        }
        .then_some(Ordering::Greater)
    }

    fn compare_constraints(lhs: &i64, lop: &Op, rop: &Op, rhs: &i64) -> Option<Ordering> {
        match (lop, rop) {
            (Op::GreaterEqual, Op::GreaterThan) => todo!(),
            (a, b) if a == b => lhs.partial_cmp(rhs),
            _ => None,
        }
    }
}

// TODO: interned strings
impl Comparable for usize {
    fn constrains(lhs: &usize, op: &Op, rhs: &usize) -> Option<Ordering> {
        match op {
            Op::NotEqual => lhs != rhs,
            Op::Match => lhs < rhs,
            Op::NotMatch => lhs <= rhs,
            Op::LessThan => lhs < rhs,
            Op::LessEqual => lhs <= rhs,
            Op::GreaterThan => lhs >= rhs,
            Op::GreaterEqual => lhs >= rhs,
            _ => false,
        }
        .then_some(Ordering::Greater)
    }
    fn compare_constraints(lhs: &usize, lop: &Op, rop: &Op, rhs: &usize) -> Option<Ordering> {
        todo!()
    }
}

impl Comparable for f64 {
    fn constrains(lhs: &f64, op: &Op, rhs: &f64) -> Option<Ordering> {
        match op {
            Op::NotEqual => lhs != rhs,
            Op::LessThan => lhs < rhs,
            Op::LessEqual => lhs <= rhs,
            Op::GreaterThan => lhs >= rhs,
            Op::GreaterEqual => lhs >= rhs,
            _ => false,
        }
        .then_some(Ordering::Greater)
    }
    fn compare_constraints(lhs: &f64, lop: &Op, rop: &Op, rhs: &f64) -> Option<Ordering> {
        todo!()
    }
}

impl Comparable for bool {
    fn constrains(lhs: &bool, op: &Op, rhs: &bool) -> Option<Ordering> {
        todo!()
    }
    fn compare_constraints(lhs: &bool, lop: &Op, rop: &Op, rhs: &bool) -> Option<Ordering> {
        todo!()
    }
}

impl<T: PartialEq + Copy> ValueType<T> {
    pub const fn constraint(op: Op, inner: T) -> Self {
        return Self::Constraint(op, inner);
    }
    pub const fn concrete(inner: T) -> Self {
        return Self::Concrete(inner);
    }
}

impl<T: PartialEq + Comparable + Copy> PartialOrd for ValueType<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Self::Type, Self::Type) => Some(Ordering::Equal),
            (Self::Type, _) => Some(Ordering::Greater),
            (_, Self::Type) => Some(Ordering::Less),

            (Self::Constraint(lop, lhs), Self::Constraint(rop, rhs)) => {
                T::compare_constraints(lhs, lop, rop, rhs)
            }

            (Self::Constraint(op, lhs), Self::Concrete(rhs)) => T::constrains(lhs, op, rhs),
            (Self::Concrete(lhs), Self::Constraint(op, rhs)) => {
                T::constrains(lhs, op, rhs).map(Ordering::reverse)
            }

            (Self::Concrete(lhs), Self::Concrete(rhs)) => {
                if lhs == rhs {
                    Some(Ordering::Equal)
                } else {
                    None
                }
            }
        }
    }
}

impl Field {
    pub const fn new(label: BasicValue, optional: bool, value: Rc<Value>) -> Field {
        Field {
            label,
            optional,
            value,
        }
    }
    pub const fn optional(label: BasicValue, value: Rc<Value>) -> Field {
        Self::new(label, true, value)
    }
    pub const fn required(label: BasicValue, value: Rc<Value>) -> Field {
        Self::new(label, false, value)
    }

    pub fn with_value(&self, value: Rc<Value>) -> Field {
        Self::new(self.label, self.optional, value)
    }
}
