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

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TCC<T> {
    Type,
    Constraint(Op, T),
    Concrete(T),
}

impl<T: PartialEq + PartialOrd> TCC<T> {
    pub fn meet_with(
        self,
        other: Self,
        bin_op: impl Fn(&T, Op, &T) -> bool,
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
                if bin_op(&b, op, &a) {
                    getvalue(Self::Concrete(b))
                } else {
                    Value::Bottom
                }
            }
            (Self::Concrete(a), Self::Constraint(op, b)) => {
                if bin_op(&a, op, &b) {
                    getvalue(Self::Concrete(a))
                } else {
                    Value::Bottom
                }
            }

            (Self::Constraint(opa, a), Self::Constraint(opb, b)) => {

                match (opa, opb) {
                    (Op::GreaterThan, Op::GreaterThan) => todo!(),
                    (Op::GreaterEqual, Op::GreaterEqual) => todo!(),

                    (Op::LessThan, Op::LessThan) => todo!(),
                    (Op::LessEqual, Op::LessEqual) => todo!(),

                    _ => Value::Conjunction(vec![getvalue(Self::Constraint(opa, a)), getvalue(Self::Constraint(opb, b))])
                }
                // if opa == opb {
                //     // ex. >10 && >2 => simplify
                //     // ex. <10 && <2 => simplify
                //     if bin_op(&a, opa, &b) {
                //         getvalue(Self::Constraint(opa, a))
                //     } else {
                //         getvalue(Self::Constraint(opb, b))
                //     }
                // } else {
                //     // ex. >10 && <20 => conjunction
                //     // ex. >=10 && <=10 => concrete
                //     // ex. >10 && <10 ==> bottom

                //     Value::Conjunction(vec![getvalue(Self::Constraint(opa, a)), getvalue(Self::Constraint(opb, b))])
                // }
            }
        }
    }

    fn bin_op_ord(lhs: &T, op: Op, rhs: &T)-> bool {
        match op {
            Op::NotEqual => lhs != rhs,

            Op::GreaterEqual => lhs >= rhs,
            Op::GreaterThan => lhs > rhs,
            Op::LessEqual => lhs <= rhs,
            Op::LessThan => lhs < rhs,

            _ => false,
        }
    }

    fn bin_op_str(lhs: &String, op: Op, rhs: &String) -> bool {
        match op {
            Op::NotEqual => lhs != rhs,

            Op::GreaterEqual => lhs >= rhs,
            Op::GreaterThan => lhs > rhs,
            Op::LessEqual => lhs <= rhs,
            Op::LessThan => lhs < rhs,

            Op::Match => Regex::new(rhs.as_str()).unwrap().is_match(&lhs),
            Op::NotMatch => !Regex::new(rhs.as_str()).unwrap().is_match(&lhs),

            _ => false,
        }
    }
}


#[test]
fn test_tcc() {
    let value = |a| Value::BasicValue(BasicValue::Int(a));
    let bin_op = TCC::<i64>::bin_op_ord;
    let conj = |a, b| Value::Conjunction(vec![value(a), value(b)]);

    macro_rules! assert_eq_tcc_meet {
        ($a:ident, $b:ident, $c:expr) => {
            assert_eq!($a.meet_with($b, bin_op, value), $c, "asserting {} & {}", stringify!($a), stringify!($b));
            assert_eq!($b.meet_with($a, bin_op, value), $c, "asserting {} & {}", stringify!($b), stringify!($a));
        };
    }


    let int = TCC::<i64>::Type;

    let zero = TCC::Concrete(0);
    let one = TCC::Concrete(1);
    let two = TCC::Concrete(2);

    let gt_1 = TCC::Constraint(Op::GreaterThan, 1);
    let gt_10 = TCC::Constraint(Op::GreaterThan, 10);
    let gt_100 = TCC::Constraint(Op::GreaterThan, 100);

    let ge_1 = TCC::Constraint(Op::GreaterEqual, 1);
    let ge_10 = TCC::Constraint(Op::GreaterEqual, 10);
    let ge_100 = TCC::Constraint(Op::GreaterEqual, 100);

    let lt_1 = TCC::Constraint(Op::LessThan, 1);
    let lt_10 = TCC::Constraint(Op::LessThan, 10);
    let lt_100 = TCC::Constraint(Op::LessThan, 100);

    let le_1 = TCC::Constraint(Op::LessEqual, 1);
    let le_10 = TCC::Constraint(Op::LessEqual, 10);
    let le_100 = TCC::Constraint(Op::LessEqual, 100);

    let ne_1 = TCC::Constraint(Op::NotEqual, 1);
    let ne_10 = TCC::Constraint(Op::NotEqual, 10);

    assert_eq_tcc_meet!(int, int, value(int));
    assert_eq_tcc_meet!(int, one, value(one));
    assert_eq_tcc_meet!(int, gt_1, value(gt_1));

    assert_eq_tcc_meet!(zero, gt_1, Value::Bottom);
    assert_eq_tcc_meet!(one, gt_1, Value::Bottom);
    assert_eq_tcc_meet!(two, gt_1, value(two));

    assert_eq_tcc_meet!(zero, lt_1, value(zero));
    assert_eq_tcc_meet!(one, lt_1, Value::Bottom);
    assert_eq_tcc_meet!(two, lt_1, Value::Bottom);

    assert_eq_tcc_meet!(zero, ge_1, Value::Bottom);
    assert_eq_tcc_meet!(one, ge_1, value(one));
    assert_eq_tcc_meet!(two, ge_1, value(two));

    assert_eq_tcc_meet!(zero, le_1, value(zero));
    assert_eq_tcc_meet!(one, le_1, value(one));
    assert_eq_tcc_meet!(two, le_1, Value::Bottom);

    assert_eq_tcc_meet!(zero, ne_1, value(zero));
    assert_eq_tcc_meet!(one, ne_1, Value::Bottom);
    assert_eq_tcc_meet!(two, ne_1, value(two));

    assert_eq_tcc_meet!(gt_10, gt_1, value(gt_10));
    assert_eq_tcc_meet!(gt_10, ge_1, value(gt_10));
    assert_eq_tcc_meet!(gt_10, ne_1, value(gt_10));

    assert_eq_tcc_meet!(ge_10, gt_1, value(ge_10));
    assert_eq_tcc_meet!(ge_10, ge_1, value(ge_10));
    assert_eq_tcc_meet!(ge_10, ne_1, value(ge_10));

    assert_eq_tcc_meet!(lt_10, lt_1, value(lt_10));
    assert_eq_tcc_meet!(lt_10, le_1, value(lt_10));
    assert_eq_tcc_meet!(lt_10, ne_1, value(lt_10));

    assert_eq_tcc_meet!(le_10, lt_1, value(le_10));
    assert_eq_tcc_meet!(le_10, le_1, value(le_10));
    assert_eq_tcc_meet!(le_10, ne_1, value(le_10));

    assert_eq_tcc_meet!(le_1, ge_1, value(one));

    assert_eq_tcc_meet!(ne_10, gt_1, conj(ne_10, gt_1));
    assert_eq_tcc_meet!(ne_10, ge_1, conj(ne_10, ge_1));
    assert_eq_tcc_meet!(ne_10, lt_100, conj(ne_10, lt_100));
    assert_eq_tcc_meet!(ne_10, le_100, conj(ne_10, le_100));
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
                lhs.meet_with(rhs, TCC::<f64>::bin_op_ord, |i| {
                    Value::BasicValue(BasicValue::Float(i))
                })
            }
            (BasicValue::Int(lhs), BasicValue::Int(rhs)) => {
                lhs.meet_with(rhs, TCC::<i64>::bin_op_ord, |i| {
                    Value::BasicValue(BasicValue::Int(i))
                })
            }

            (BasicValue::Bytes(lhs), BasicValue::Bytes(rhs)) => {
                lhs.meet_with(rhs, TCC::<String>::bin_op_str, |v| {
                    Value::BasicValue(BasicValue::Bytes(v))
                })
            }
            (BasicValue::String(lhs), BasicValue::String(rhs)) => {
                lhs.meet_with(rhs, TCC::<String>::bin_op_str, |v| {
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

#[test]
fn test_basic_meets() {
    assert_eq!(
        Value::Top.meet(Value::Bottom),
        Value::Bottom,
        "_ & _|_ == _|_"
    );

    assert_eq!(
        Value::Top.meet(Value::BasicValue(BasicValue::Null)),
        Value::BasicValue(BasicValue::Null),
        "_ & null == null"
    );

    assert_eq!(
        Value::Top.meet(Value::BasicValue(BasicValue::Bool(None))),
        Value::BasicValue(BasicValue::Bool(None)),
        "_ & bool == bool"
    );
    assert_eq!(
        Value::Top.meet(Value::BasicValue(BasicValue::Bool(Some(true)))),
        Value::BasicValue(BasicValue::Bool(Some(true))),
        "_ & true == true"
    );
    assert_eq!(
        Value::BasicValue(BasicValue::Bool(None))
            .meet(Value::BasicValue(BasicValue::Bool(Some(true)))),
        Value::BasicValue(BasicValue::Bool(Some(true))),
        "bool & true == true"
    );

    assert_eq!(
        Value::BasicValue(BasicValue::Int(TCC::Type))
            .meet(Value::BasicValue(BasicValue::Int(TCC::Constraint(Op::GreaterThan, 3)))),
        Value::BasicValue(BasicValue::Int(TCC::Constraint(Op::GreaterThan, 3))),
        "int & >3 == >3"
    );
}
