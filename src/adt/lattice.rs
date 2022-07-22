use std::cmp::Ordering;

use super::op::Op;

#[derive(Debug, PartialEq)]
pub enum ValueType<T: PartialEq> {
    Type,
    Constraint(Op, T),
    Concrete(T),
}

impl<T: PartialEq> ValueType<T> {
    pub fn t() -> ValueType<T> {
        return ValueType::Type;
    }
    pub fn constraint(op: Op, inner: T) -> ValueType<T> {
        return ValueType::Constraint(op, inner);
    }
    pub fn concrete(inner: T) -> ValueType<T> {
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

#[derive(Debug, PartialEq)]
pub struct Field<'a> {
    label: usize,
    optional: bool,
    value: &'a Value<'a>,
}

impl<'a> Field<'a> {
    pub fn new(label: usize, optional: bool, value: &'a Value<'a>) -> Field {
        Field {
            label,
            optional,
            value,
        }
    }
    pub fn optional(label: usize, value: &'a Value<'a>) -> Field {
        Self::new(label, true, value)
    }
    pub fn required(label: usize, value: &'a Value<'a>) -> Field {
        Self::new(label, false, &value)
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

#[derive(Debug, PartialEq)]
pub enum Value<'a> {
    Top,

    Struct(Vec<Field<'a>>),
    List(Vec<&'a Value<'a>>),
    // Func,
    Bytes(ValueType<usize>),
    String(ValueType<usize>),
    Float(ValueType<f64>),
    Int(ValueType<i64>),
    Bool(ValueType<bool>),
    Null,

    Bottom,
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

fn compare_lists(lhs: &Vec<&Value>, rhs: &Vec<&Value>) -> Option<Ordering> {
    lhs.iter()
        .zip(rhs.iter())
        .map(|(l, r)| l.partial_cmp(r))
        .reduce(combine_orderings)
        .unwrap_or(None)
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

#[test]
fn test_lattice() {
    let bool_t = &Value::Bool(ValueType::t());
    let bool_true = &Value::Bool(ValueType::concrete(true));

    let int_t = &Value::Int(ValueType::Type);
    let int_gt_5 = &Value::Int(ValueType::constraint(Op::GreaterThan, 5));
    let int_5 = &Value::Int(ValueType::concrete(5));

    let float_t = &Value::Float(ValueType::Type);
    let float_gt_5 = &Value::Float(ValueType::constraint(Op::GreaterThan, 5.0));
    let float_5 = &Value::Float(ValueType::concrete(5.0));

    let string_t = &Value::String(ValueType::Type);
    let string_match = &Value::String(ValueType::constraint(Op::Match, 0));
    let string_value = &Value::String(ValueType::concrete(1));

    let bytes_t = &Value::Bytes(ValueType::Type);
    let bytes_match = &Value::Bytes(ValueType::constraint(Op::Match, 0));
    let bytes_value = &Value::Bytes(ValueType::concrete(1));

    let list_types = &Value::List(vec![bool_t, int_t, float_t, string_t, bytes_t]);
    let list_constraints = &Value::List(vec![
        bool_t,
        int_gt_5,
        float_gt_5,
        string_match,
        bytes_match,
    ]);
    let list_values = &Value::List(vec![bool_true, int_5, float_5, string_value, bytes_value]);
    let list_mixed = &Value::List(vec![bool_t, int_5, float_gt_5, string_t, bytes_match]);

    let struct_a_int = &Value::Struct(vec![Field::required(0, int_t)]);
    let struct_b_int = &Value::Struct(vec![Field::required(1, int_t)]);
    let struct_ab_int = &Value::Struct(vec![Field::required(0, int_t), Field::required(1, int_t)]);
    let struct_a_5 = &Value::Struct(vec![Field::required(0, int_5)]);
    let struct_b_5 = &Value::Struct(vec![Field::required(1, int_5)]);
    let struct_ab_5 = &Value::Struct(vec![Field::required(0, int_5), Field::required(1, int_5)]);

    let struct_a_opt_int = &Value::Struct(vec![Field::optional(0, int_t)]);
    let struct_a_opt_int_gt5 = &Value::Struct(vec![Field::optional(0, int_gt_5)]);
    let struct_a_int_gt5 = &Value::Struct(vec![Field::required(0, int_gt_5)]);

    assert!(Value::Top > Value::Bottom);
    assert!(Value::Top > Value::Null);

    assert!(Value::Top > *bool_t);
    assert!(Value::Top > *bool_true);
    assert!(bool_t > bool_true);

    assert!(Value::Top > *int_t);
    assert!(Value::Top > *int_gt_5);
    assert!(Value::Top > *int_5);
    assert!(int_t > int_gt_5);
    assert!(int_t > int_5);
    assert!(int_gt_5 > int_5);

    assert!(Value::Top > *float_t);
    assert!(Value::Top > *float_gt_5);
    assert!(Value::Top > *float_5);
    assert!(float_t > float_gt_5);
    assert!(float_t > float_5);
    assert!(float_gt_5 > float_5);

    assert!(Value::Top > *string_t);
    assert!(Value::Top > *string_match);
    assert!(Value::Top > *string_value);
    assert!(string_t > string_match);
    assert!(string_t > string_value);
    assert!(string_match > string_value);

    assert!(Value::Top > *bytes_t);
    assert!(Value::Top > *bytes_match);
    assert!(Value::Top > *bytes_value);
    assert!(bytes_t > bytes_match);
    assert!(bytes_t > bytes_value);
    assert!(bytes_match > bytes_value);

    assert!(Value::Top > *list_types);
    assert!(Value::Top > *list_constraints);
    assert!(Value::Top > *list_values);
    assert!(Value::Top > *list_mixed);
    assert!(list_types > list_constraints);
    assert!(list_types > list_mixed);
    assert!(list_constraints > list_values);

    assert!(Value::Top > *struct_a_int);
    assert!(Value::Top > *struct_ab_int);
    assert!(Value::Top > *struct_b_int);
    assert!(Value::Top > *struct_a_5);
    assert!(Value::Top > *struct_ab_5);
    assert!(Value::Top > *struct_b_5);
    assert!(struct_a_int > struct_a_5);
    assert!(struct_a_int > struct_ab_int);
    assert!(struct_b_int > struct_ab_int);
    assert!(struct_b_int > struct_b_5);
    assert!(struct_a_5 > struct_ab_5);
    assert!(struct_ab_int > struct_ab_5);
    assert!(struct_b_5 > struct_ab_5);

    assert!(Value::Top > *struct_a_opt_int);
    assert!(Value::Top > *struct_a_opt_int_gt5);
    assert!(Value::Top > *struct_a_int_gt5);
    assert!(struct_a_opt_int > struct_a_opt_int_gt5);
    assert!(struct_a_opt_int > struct_a_int);
    assert!(struct_a_opt_int_gt5 > struct_a_int_gt5);
    assert!(struct_a_int > struct_a_int_gt5);

    assert!(Value::Bottom < Value::Top);
    assert!(Value::Bottom < Value::Null);

    assert!(Value::Bottom < *bool_t);
    assert!(Value::Bottom < *bool_true);
    assert!(bool_true < bool_t);

    assert!(Value::Bottom < *int_t);
    assert!(Value::Bottom < *int_gt_5);
    assert!(Value::Bottom < *int_5);
    assert!(int_5 < int_gt_5);
    assert!(int_5 < int_t);
    assert!(int_gt_5 < int_t);

    assert!(Value::Bottom < *float_t);
    assert!(Value::Bottom < *float_gt_5);
    assert!(Value::Bottom < *float_5);
    assert!(float_5 < float_gt_5);
    assert!(float_5 < float_t);
    assert!(float_gt_5 < float_t);

    assert!(Value::Bottom < *string_t);
    assert!(Value::Bottom < *string_match);
    assert!(Value::Bottom < *string_value);
    assert!(string_value < string_match);
    assert!(string_value < string_t);
    assert!(string_match < string_t);

    assert!(Value::Bottom < *bytes_t);
    assert!(Value::Bottom < *bytes_match);
    assert!(Value::Bottom < *bytes_value);
    assert!(bytes_value < bytes_match);
    assert!(bytes_value < bytes_t);
    assert!(bytes_match < bytes_t);

    assert!(Value::Bottom < *list_types);
    assert!(Value::Bottom < *list_constraints);
    assert!(Value::Bottom < *list_values);
    assert!(Value::Bottom < *list_mixed);
    assert!(list_values < list_constraints);
    assert!(list_values < list_types);
    assert!(list_values < list_mixed);
    assert!(list_constraints < list_types);

    assert!(Value::Bottom < *struct_a_int);
    assert!(Value::Bottom < *struct_ab_int);
    assert!(Value::Bottom < *struct_b_int);
    assert!(Value::Bottom < *struct_a_5);
    assert!(Value::Bottom < *struct_ab_5);
    assert!(Value::Bottom < *struct_b_5);
    assert!(struct_ab_5 < struct_a_5);
    assert!(struct_ab_5 < struct_ab_int);
    assert!(struct_ab_5 < struct_b_5);
    assert!(struct_a_5 < struct_a_int);
    assert!(struct_ab_int < struct_a_int);
    assert!(struct_ab_int < struct_b_int);
    assert!(struct_b_5 < struct_b_int);

    assert!(Value::Bottom < *struct_a_opt_int);
    assert!(Value::Bottom < *struct_a_opt_int_gt5);
    assert!(Value::Bottom < *struct_a_int_gt5);
    assert!(struct_a_opt_int_gt5 < struct_a_opt_int);
    assert!(struct_a_int < struct_a_opt_int);
    assert!(struct_a_int_gt5 < struct_a_opt_int_gt5);
    assert!(struct_a_int_gt5 < struct_a_int);
}
