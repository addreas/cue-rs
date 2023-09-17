use std::{rc::Rc, collections::HashMap, cell::{RefCell, Ref}};

use crate::{ast, cue_val};
use super::value;
use super::value::ToValue;

pub struct Scope {
    parent: Option<Rc<Scope>>,
    items: HashMap<Rc<str>, RefCell<Rc<value::Value>>>,
}

pub fn eval(source: ast::SourceFile) -> value::Value {
    let scope = Rc::new(RefCell::new(Scope {
        parent: None,
        items: HashMap::default(),
    }));

    let mut fields: Vec<value::Field> = vec![];
    for decl in source.declarations {
        match decl {
            ast::Declaration::Bad => todo!(),
            ast::Declaration::CommentGroup(_) => todo!(),
            ast::Declaration::Attribute(_) => todo!(),
            ast::Declaration::Field(f) => {
                let val = Rc::from(eval_expr(f.value, scope.borrow()));
                let (label, modifier) = match f.label {
                    ast::Label::Ident(i, m) => (i.name, m),
                    ast::Label::String(ast::Interpolation::Simple(s,), m) => (s, m),
                    ast::Label::String(ast::Interpolation::Interpolated(_, _), m) => todo!(),
                    ast::Label::Alias(..) => todo!(),
                    ast::Label::Paren(e, m) => match eval_expr(e, scope.borrow()) {
                        value::Value::String(value::Basic::Value(s)) => (s, m),
                        _ => todo!(),
                    },
                    ast::Label::Bracket(_) => todo!(),
                };
                let definition = label.starts_with("#");
                let hidden = label.starts_with("_");
                fields.push(value::Field {
                    label: label.clone(),
                    optional: modifier.map(|m| m == ast::LabelModifier::Optional),
                    definition,
                    hidden,
                    value: val.clone(),
                });
                scope.borrow_mut().items.insert(label.clone(), val.into());
            },
            ast::Declaration::Alias(_) => todo!(),
            ast::Declaration::Comprehension(_) => todo!(),
            ast::Declaration::Ellipsis(_) => todo!(),
            ast::Declaration::LetClause(_) => todo!(),
            ast::Declaration::ImportDecl(_) => todo!(),
            ast::Declaration::Embedding(_) => todo!(),
        }
    }

    value::Value::Struct(fields)
}

#[test]
fn test_source_file() {
    let eval_str = |str| eval(ast::parser::parse_file(str).unwrap());

    assert_eq!(eval_str("a: 1"), cue_val!({a: (1_i64)}));
    assert_eq!(eval_str("a: 1, b: 2"), cue_val!({a: (1_i64), b: (2_i64)}));
}

pub fn eval_expr(expr: ast::Expr, scope: Ref<Scope>) -> value::Value {
    match expr {
        ast::Expr::Bad => todo!(),
        ast::Expr::Alias(_) => todo!(),
        ast::Expr::Comprehension(_) => todo!(),
        ast::Expr::Ident(_) => todo!(),
        ast::Expr::QualifiedIdent(_, _) => todo!(),
        ast::Expr::BasicLit(ast::BasicLit::Bool(b)) => value::Value::Bool(Some(b)),
        ast::Expr::BasicLit(ast::BasicLit::Int(i)) => value::Value::Int(value::Basic::Value(i)),
        ast::Expr::BasicLit(ast::BasicLit::Float(f)) => value::Value::Float(value::Basic::Value(f)),
        ast::Expr::BasicLit(_) => todo!(),
        ast::Expr::Struct(_) => todo!(),
        ast::Expr::List(_) => todo!(),
        ast::Expr::Ellipsis(_) => todo!(),
        ast::Expr::UnaryExpr(_) => todo!(),
        ast::Expr::BinaryExpr(_) => todo!(),
        ast::Expr::Parens(_) => todo!(),
        ast::Expr::Selector(_) => todo!(),
        ast::Expr::Index(_) => todo!(),
        ast::Expr::Slice(_) => todo!(),
        ast::Expr::Call(_) => todo!(),
    }
    // value::Value::Bottom
}
