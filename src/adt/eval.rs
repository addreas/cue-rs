use std::{rc::Rc, cell::RefCell};
use rpds::HashTrieMap;
use crate::ast;
use super::value;

#[derive(Clone)]
pub struct Scope {
    parent: Option<Rc<Scope>>,
    items: RefCell<HashTrieMap<value::Label, RefCell<Rc<value::Value>>>>
}

impl Scope {
    fn as_parent(self: Rc<Self>) -> Rc<Self> {
        Rc::new(Self {
            parent: Some(self),
            items: RefCell::default(),
        })
    }
}

impl Default for Scope {
    fn default() -> Self {
        Self { parent: Default::default(), items: Default::default() }
    }
}

pub fn eval(source: ast::SourceFile) -> value::Value {
    let scope = Rc::new(Scope::default());
    eval_decls(source.declarations, scope.as_parent())
}

#[test]
fn test_source_file() {
    use crate::cue_val;
    let eval_str = |str| eval(ast::parser::parse_file(str).unwrap());

    assert_eq!(eval_str("a: 1"), cue_val!({a: (1)}));
    assert_eq!(eval_str("a: 1, b: 2"), cue_val!({a: (1), b: (2)}));
}

pub fn eval_decls(decls: Vec<ast::Declaration>, scope: Rc<Scope>) -> value::Value {
    for decl in decls {
        match decl {
            ast::Declaration::Bad => todo!(),
            ast::Declaration::CommentGroup(_) => todo!(),
            ast::Declaration::Attribute(_) => todo!(),
            ast::Declaration::Field(f) => {
                let field = eval_field(f, scope.clone());
                if let Some(existing) = scope.items.borrow().get(&field.label) {
                    existing.replace_with(|e| e.clone().meet(field.value.clone()));
                } else {
                    scope.items.borrow_mut().insert_mut(field.label, RefCell::new(field.value));
                }
            },
            ast::Declaration::Alias(_) => todo!(),
            ast::Declaration::Comprehension(_) => todo!(),
            ast::Declaration::Ellipsis(_) => todo!(),
            ast::Declaration::LetClause(_) => todo!(),
            ast::Declaration::Embedding(_) => todo!(),
        }
    }

    value::Value::Struct(scope.items.borrow_mut().into_iter().map(|(k, v)| {
        value::Field {
            label: k.clone(),
            value: v.clone().into_inner(),
        }
    }).collect())
}

pub fn eval_field(field: ast::Field, scope: Rc<Scope>) -> value::Field {
    let (name, modifier) = match field.label {
        ast::Label::Ident(i, m) => (i.name, m),
        ast::Label::String(ast::Interpolation::Simple(s,), m) => (s, m),
        ast::Label::String(ast::Interpolation::Interpolated(_, _), m) => todo!(),
        ast::Label::Alias(..) => todo!(),
        ast::Label::Paren(e, m) => match eval_expr(e, scope.clone()) {
            value::Value::String(value::Basic::Value(s)) => (s, m),
            _ => todo!(),
        },
        ast::Label::Bracket(_) => todo!(),
    };
    let definition = name.starts_with("#");
    let hidden = name.starts_with("_");
    let value = Rc::from(eval_expr(field.value, scope.clone()));
    value::Field {
        label: value::Label {
            name,
            optional: modifier.map(|m| m == ast::LabelModifier::Optional),
            definition,
            hidden,
        },
        value,
    }
}

pub fn eval_expr(expr: ast::Expr, scope: Rc<Scope>) -> value::Value {
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
        ast::Expr::Struct(s) => eval_decls(s.elements, scope),
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
