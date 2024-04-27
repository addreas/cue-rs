use std::collections::HashMap;
use json::JsonValue;

mod parser;
mod ebpf;
mod pratt;

#[derive(Debug)]
enum Op {
    Add,
}

#[derive(Debug)]
enum AST {
    Struct(HashMap<String, Box<AST>>),
    Number(i64), // floats can be NotNan, NaN -> _|_
    Identifier(String),
    BinaryExpr(Box<AST>, Op, Box<AST>),
    Selector(Box<AST>, String)
}

fn parse(input: &str) -> AST {
    AST::Struct(Default::default())
}

enum Value {
    Bottom,
    Reference,
    Expr,

    Concrete(JsonValue)
}

impl Into<JsonValue> for Value {
    fn into(self) -> JsonValue {
        match self {
            Self::Concrete(value) => value,
            _ => todo!(),
        }
    }
}


fn eval(ast: &AST) -> Value {
    Value::Bottom
}

#[test]
fn basic_txtar() {
    let txtar = crate::txtar::parse(include_str!("./basic.txtar").to_string());
    let input = txtar.sections().get("in.cue").unwrap();

    let ast = parse(input);
    assert_eq!(format!("{ast:?}"), txtar.sections().get("out/ast:?").unwrap().as_str());
    assert_eq!(json::from(eval(&ast)), json::parse(&txtar.sections().get("out/json").unwrap()).unwrap())
}
