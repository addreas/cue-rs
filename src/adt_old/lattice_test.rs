use super::lattice::*;
use super::op::Op;

const BOOL_T: Value = Value::Bool(ValueType::Type);
const BOOL_TRUE: Value = Value::Bool(ValueType::concrete(true));
#[test]
fn test_basic() {
    assert!(Value::Top > Value::Bottom);
    assert!(Value::Top > Value::Null);

    assert!(Value::Bottom < Value::Top);
    assert!(Value::Bottom < Value::Null);

    assert!(Value::Top > BOOL_T);
    assert!(Value::Top > BOOL_TRUE);
    assert!(BOOL_T > BOOL_TRUE);

    assert!(Value::Bottom < BOOL_T);
    assert!(Value::Bottom < BOOL_TRUE);
    assert!(BOOL_TRUE < BOOL_T);
}

const INT_T: Value = Value::Int(ValueType::Type);
const INT_GT5: Value = Value::Int(ValueType::constraint(Op::GreaterThan, 5));
const INT_5: Value = Value::Int(ValueType::concrete(5));
#[test]
fn test_int() {
    assert!(Value::Top > INT_T);
    assert!(Value::Top > INT_GT5);
    assert!(Value::Top > INT_5);
    assert!(INT_T > INT_GT5);
    assert!(INT_T > INT_5);
    assert!(INT_GT5 > INT_5);

    assert!(Value::Bottom < INT_T);
    assert!(Value::Bottom < INT_GT5);
    assert!(Value::Bottom < INT_5);
    assert!(INT_5 < INT_GT5);
    assert!(INT_5 < INT_T);
    assert!(INT_GT5 < INT_T);
}

const FLOAT_T: Value = Value::Float(ValueType::Type);
const FLOAT_GT5: Value = Value::Float(ValueType::constraint(Op::GreaterThan, 5.0));
const FLOAT_5: Value = Value::Float(ValueType::concrete(5.0));
#[test]
fn test_float() {
    assert!(Value::Top > FLOAT_T);
    assert!(Value::Top > FLOAT_GT5);
    assert!(Value::Top > FLOAT_5);
    assert!(FLOAT_T > FLOAT_GT5);
    assert!(FLOAT_T > FLOAT_5);
    assert!(FLOAT_GT5 > FLOAT_5);

    assert!(Value::Bottom < FLOAT_T);
    assert!(Value::Bottom < FLOAT_GT5);
    assert!(Value::Bottom < FLOAT_5);
    assert!(FLOAT_5 < FLOAT_GT5);
    assert!(FLOAT_5 < FLOAT_T);
    assert!(FLOAT_GT5 < FLOAT_T);
}

const STRING_T: Value = Value::String(ValueType::Type);
const STRING_MATCH: Value = Value::String(ValueType::constraint(Op::Match, "."));
const STRING_VALUE: Value = Value::String(ValueType::concrete("a"));
#[test]
fn test_string() {
    assert!(Value::Top > STRING_T);
    assert!(Value::Top > STRING_MATCH);
    assert!(Value::Top > STRING_VALUE);
    assert!(STRING_T > STRING_MATCH);
    assert!(STRING_T > STRING_VALUE);
    assert!(STRING_MATCH > STRING_VALUE);

    assert!(Value::Bottom < STRING_T);
    assert!(Value::Bottom < STRING_MATCH);
    assert!(Value::Bottom < STRING_VALUE);
    assert!(STRING_VALUE < STRING_MATCH);
    assert!(STRING_VALUE < STRING_T);
    assert!(STRING_MATCH < STRING_T);
}

const BYTES_T: Value = Value::Bytes(ValueType::Type);
const BYTES_MATCH: Value = Value::Bytes(ValueType::constraint(Op::Match, "."));
const BYTES_VALUE: Value = Value::Bytes(ValueType::concrete("a"));
#[test]
fn test_bytes() {
    assert!(Value::Top > BYTES_T);
    assert!(Value::Top > BYTES_MATCH);
    assert!(Value::Top > BYTES_VALUE);
    assert!(BYTES_T > BYTES_MATCH);
    assert!(BYTES_T > BYTES_VALUE);
    assert!(BYTES_MATCH > BYTES_VALUE);

    assert!(Value::Bottom < BYTES_T);
    assert!(Value::Bottom < BYTES_MATCH);
    assert!(Value::Bottom < BYTES_VALUE);
    assert!(BYTES_VALUE < BYTES_MATCH);
    assert!(BYTES_VALUE < BYTES_T);
    assert!(BYTES_MATCH < BYTES_T);
}

#[test]
fn test_list() {
    let list_types = Value::List(vec![BOOL_T, INT_T, FLOAT_T, STRING_T, BYTES_T]);
    let list_constraints = Value::List(vec![BOOL_T, INT_GT5, FLOAT_GT5, STRING_MATCH, BYTES_MATCH]);
    let list_values = Value::List(vec![BOOL_TRUE, INT_5, FLOAT_5, STRING_VALUE, BYTES_VALUE]);
    let list_mixed = Value::List(vec![BOOL_T, INT_5, FLOAT_GT5, STRING_T, BYTES_MATCH]);

    assert!(Value::Top > list_types);
    assert!(Value::Top > list_constraints);
    assert!(Value::Top > list_values);
    assert!(Value::Top > list_mixed);
    assert!(list_types > list_constraints);
    assert!(list_types > list_mixed);
    assert!(list_constraints > list_values);

    assert!(Value::Bottom < list_types);
    assert!(Value::Bottom < list_constraints);
    assert!(Value::Bottom < list_values);
    assert!(Value::Bottom < list_mixed);
    assert!(list_values < list_constraints);
    assert!(list_values < list_types);
    assert!(list_values < list_mixed);
    assert!(list_constraints < list_types);
}

#[test]
fn test_struct() {
    let struct_a_int = Value::Struct(vec![Field::required("a", INT_T)]);
    let struct_b_int = Value::Struct(vec![Field::required("b", INT_T)]);
    let struct_ab_int = Value::Struct(vec![Field::required("a", INT_T), Field::required("b", INT_T)]);
    let struct_a_5 = Value::Struct(vec![Field::required("a", INT_5)]);
    let struct_b_5 = Value::Struct(vec![Field::required("b", INT_5)]);
    let struct_ab_5 = Value::Struct(vec![Field::required("a", INT_5), Field::required("b", INT_5)]);

    assert!(Value::Top > struct_a_int);
    assert!(Value::Top > struct_ab_int);
    assert!(Value::Top > struct_b_int);
    assert!(Value::Top > struct_a_5);
    assert!(Value::Top > struct_ab_5);
    assert!(Value::Top > struct_b_5);
    assert!(struct_a_int > struct_a_5);
    assert!(struct_a_int > struct_ab_int);
    assert!(struct_b_int > struct_ab_int);
    assert!(struct_b_int > struct_b_5);
    assert!(struct_a_5 > struct_ab_5);
    assert!(struct_ab_int > struct_ab_5);
    assert!(struct_b_5 > struct_ab_5);

    assert!(Value::Bottom < struct_a_int);
    assert!(Value::Bottom < struct_ab_int);
    assert!(Value::Bottom < struct_b_int);
    assert!(Value::Bottom < struct_a_5);
    assert!(Value::Bottom < struct_ab_5);
    assert!(Value::Bottom < struct_b_5);
    assert!(struct_ab_5 < struct_a_5);
    assert!(struct_ab_5 < struct_ab_int);
    assert!(struct_ab_5 < struct_b_5);
    assert!(struct_a_5 < struct_a_int);
    assert!(struct_ab_int < struct_a_int);
    assert!(struct_ab_int < struct_b_int);
    assert!(struct_b_5 < struct_b_int);

    let struct_a_opt_int = Value::Struct(vec![Field::optional("a", INT_T)]);
    let struct_a_opt_int_gt5 = Value::Struct(vec![Field::optional("a", INT_GT5)]);
    let struct_a_int_gt5 = Value::Struct(vec![Field::required("a", INT_GT5)]);

    assert!(Value::Top > struct_a_opt_int);
    assert!(Value::Top > struct_a_opt_int_gt5);
    assert!(Value::Top > struct_a_int_gt5);
    assert!(struct_a_opt_int > struct_a_opt_int_gt5);
    assert!(struct_a_opt_int > struct_a_int);
    assert!(struct_a_opt_int_gt5 > struct_a_int_gt5);
    assert!(struct_a_int > struct_a_int_gt5);

    assert!(Value::Bottom < struct_a_opt_int);
    assert!(Value::Bottom < struct_a_opt_int_gt5);
    assert!(Value::Bottom < struct_a_int_gt5);
    assert!(struct_a_opt_int_gt5 < struct_a_opt_int);
    assert!(struct_a_int < struct_a_opt_int);
    assert!(struct_a_int_gt5 < struct_a_opt_int_gt5);
    assert!(struct_a_int_gt5 < struct_a_int);
}
