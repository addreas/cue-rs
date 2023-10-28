use crate::{
    adt::scope::{MutableScope, Scope},
    ast, cue_val,
    value::{Basic, Field, Label, Value},
};
use std::{cell::RefCell, rc::Rc};

pub fn eval(source: ast::SourceFile) -> Rc<Value> {
    let scope = Rc::new(RefCell::new(Scope::default()));
    eval_decls(source.declarations, scope)
}

pub fn eval_decls(decls: Rc<[ast::Declaration]>, scope: MutableScope) -> Rc<Value> {
    let mut current_value = Rc::from(Value::Top);
    for decl in decls.into_iter() {
        match decl {
            ast::Declaration::Attribute(_) => todo!(),
            ast::Declaration::Field(f) => {
                // TODO: laziness?
                let field = eval_field(&f, scope.clone());

                current_value = current_value.meet(Value::Struct([field].into()).into());
            }
            ast::Declaration::Alias(_) => todo!(),
            ast::Declaration::Comprehension(_) => todo!(),
            ast::Declaration::Ellipsis(_) => todo!(),
            ast::Declaration::LetClause(_) => todo!(),
            ast::Declaration::Embedding(e) => {
                current_value = current_value.meet(eval_expr(e, Scope::as_parent(scope.clone())))
            }
        }
    }

    current_value
}

pub fn eval_field(field: &ast::Field, scope: MutableScope) -> Field {
    let label = match &field.label {
        ast::Label::Ident(n, lm) => {
            // scope.borrow_mut().set(n.clone(), RefCelPlaceholdervalue); // TODO
            Label::Single(n.name.clone(), n.kind, lm.clone())
        },
        ast::Label::Alias(_, _) => todo!(),
        ast::Label::String(str, lm) => Label::Single(
            eval_interpolation(str.clone(), Scope::as_parent(scope.clone())),
            None,
            lm.clone(),
        ),
        ast::Label::Paren(expr, lm) => {
            match eval_expr(&expr, Scope::as_parent(scope.clone())).as_ref() {
                Value::String(Basic::Value(v)) => Label::Single(v.clone(), None, lm.clone()),
                _ => todo!(),
            }
        }
        ast::Label::Bracket(expr) => {
            let label_expr = if let ast::Expr::Alias(alias) = expr {
                let inner = eval_expr(&alias.expr, Scope::as_parent(scope.clone()));
                scope.borrow_mut().set(alias.ident.clone(), inner.clone());
                inner
            } else {
                eval_expr(&expr, Scope::as_parent(scope.clone()))
            };
            Label::Bulk(label_expr).into()
        }
    };

    let value: Rc<Value> = Rc::from(eval_expr(&field.value, scope.clone()));

    Field { label, value }
}

pub fn eval_interpolation(_str: ast::Interpolation, _scope: MutableScope) -> Rc<str> {
    todo!()
}

fn eval_predeclared(name: &str) -> Option<Value> {
    // https://cuelang.org/docs/references/spec/#predeclared-identifiers
    match name {
        "bool" => Some(cue_val!((bool))),
        "int" => Some(cue_val!((int))),
        "float" => Some(cue_val!((float))),
        "string" => Some(cue_val!((string))),
        "bytes" => Some(cue_val!((bytes))),

        "number" => Some(cue_val!((int) | (float))),
        "uint" => Some(cue_val!((>=0))),
        "uint8" => Some(cue_val!((>=0) & (<=255))),
        "int8" => Some(cue_val!((>=-128) & (<=127))),
        "uint16" => Some(cue_val!((>=0) & (<=65535))),
        "int16" => Some(cue_val!((>=-32_768) & (<=32_767))),
        "rune" => Some(cue_val!((>=0) & (<=0x10FFFF))),
        "uint32" => Some(cue_val!((>=0) & (<=4_294_967_295))),
        "int32" => Some(cue_val!((>=-2_147_483_648) & (<=2_147_483_647))),
        // "uint64" => Some(cue_val!((>=0)
        //                    & (<=18_446_744_073_709_551_615))), // TODO
        "int64" => Some(cue_val!((>=-9_223_372_036_854_775_808)
                          & (<=9_223_372_036_854_775_807))),
        // "uint128" => Some(cue_val!((>=0)
        //                    & (<=340_282_366_920_938_463_463_374_607_431_768_211_455))), // TODO
        // "int128" => Some(cue_val!((>=-170_141_183_460_469_231_731_687_303_715_884_105_728)
        //                     & (<=170_141_183_460_469_231_731_687_303_715_884_105_727))), // TODO
        "float32" => Some(cue_val!((>=-3.40282346638528859811704183484516925440e+38)
                            & (<=3.40282346638528859811704183484516925440e+38))),
        "float64" => Some(cue_val!((>=-1.797693134862315708145274237317043567981e+308)
                            & (<=1.797693134862315708145274237317043567981e+308))),
        _ => None,
    }
}

pub fn eval_expr(expr: &ast::Expr, scope: MutableScope) -> Rc<Value> {
    match expr {
        ast::Expr::Alias(_) => todo!(),
        ast::Expr::Comprehension(_) => todo!(),
        ast::Expr::Ident(n) => {
            if let Some(predeclared) = eval_predeclared(n.name.as_ref()) {
                predeclared.into()
            } else if let Some(val) = scope.borrow().get(&n) {
                val.into()
            } else {
                Value::Bottom.into()
            }
        }
        ast::Expr::QualifiedIdent(_, _) => todo!(),
        ast::Expr::Null => Value::Null.into(),
        ast::Expr::Bottom => Value::Bottom.into(),
        ast::Expr::Bool(b) => Value::Bool(Some(*b)).into(),
        ast::Expr::Int(i) => Value::Int(Basic::Value(*i)).into(),
        ast::Expr::Float(f) => Value::Float(Basic::Value(*f)).into(),
        ast::Expr::String(s) => match s {
            ast::Interpolation::Simple(s) => Value::String(Basic::Value(s.clone())).into(),
            ast::Interpolation::Interpolated(_, _) => todo!(),
        },
        ast::Expr::Bytes(s) => match s {
            ast::Interpolation::Simple(s) => Value::Bytes(Basic::Value(s.clone())).into(),
            ast::Interpolation::Interpolated(_, _) => todo!(),
        },
        ast::Expr::Struct(s) => eval_decls(s.clone(), Scope::as_parent(scope.clone())),
        ast::Expr::List(l) => Value::List(
            l
                .into_iter()
                .map(|e| eval_expr(e, scope.clone()))
                .collect(),
        )
        .into(),
        ast::Expr::Ellipsis(_) => todo!(),
        ast::Expr::UnaryExpr(_, _) => todo!(),
        ast::Expr::BinaryExpr(lhs, op, rhs) => {
            let lval = eval_expr(lhs, scope.clone());
            let rval = eval_expr(rhs, scope.clone());

            match op {
                ast::Operator::Conjunct => lval.meet(rval),
                ast::Operator::Disjunct => lval.join(rval),

                _ => todo!(),
            }
        }
        ast::Expr::Parens(_) => todo!(),
        ast::Expr::Selector(_, _) => todo!(),
        ast::Expr::Index(_, _) => todo!(),
        ast::Expr::Slice(_, _, _) => todo!(),
        ast::Expr::Call(_, _) => todo!(),
    }
}

#[test]
fn test_source_file() {
    use crate::cue_val;
    use crate::parser::parse_file;
    let eval_str = |str| eval(parse_file(str).unwrap());

    assert_eq!(eval_str("a: 1"), cue_val!({(a): (1)}).into());
    assert_eq!(eval_str("{ a: 1 }"), cue_val!({(a): (1)}).into());
    assert_eq!(
        eval_str("a: 1, b: 2"),
        cue_val!({(a): (1), (b): (2)}).into()
    );
    assert_eq!(
        eval_str(
            "{
                a: 1
                b: 2
            }"
        ),
        cue_val!({(a): (1), (b): (2)}).into()
    );
    assert_eq!(
        eval_str(
            r#"{
                a: "a"
                b: "b"
                [x="c"]: x
            }"#
        ),
        cue_val!({
            (a): ("a"),
            (b): ("b"),
            (["c"]): ("c")
        })
        .into()
    );
    assert_eq!(
        eval_str(
            r#"{
                a: "a"
                b: "b"
                [x="c"|"d"]: x
                e: "e"
                e: "f"
            }"#
        ),
        cue_val!({
            (a): ("a"),
            (b): ("b"),
            ([("c") | ("d")]): (("c") | ("d")),
            ("e"): (_|_)
        })
        .into()
    );
}
