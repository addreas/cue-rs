use std::{rc::Rc, cell::RefCell, collections::HashMap};
use crate::ast;
use super::value;

#[derive(Clone, Default)]
pub struct Scope {
    parent: Option<Rc<Scope>>, // TODO: can parent scope be made immutable despite refcells?
    items: RefCell<HashMap<Rc<str>, RefCell<Rc<value::Value>>>>

}

impl Scope {
    fn as_parent(self: Rc<Self>) -> Rc<Self> {
        Rc::new(Self {
            parent: Some(self),
            items: RefCell::default(),
        })
    }

    fn deref(self: Rc<Self>, key: Rc<str>) -> Option<Rc<value::Value>> {
        let items = self.items.borrow();

        if items.contains_key(&key) {
            return items.get(&key).unwrap().clone().into_inner().into()
        } else if let Some(p) = self.parent.clone() {
            return p.deref(key)
        } else {
            return None
        }
    }
}

pub fn eval(source: ast::SourceFile) -> Rc<value::Value> {
    let scope = Rc::new(Scope::default());
    eval_decls(source.declarations, scope)
}

#[test]
fn test_source_file() {
    use crate::cue_val;
    let eval_str = |str| eval(ast::parser::parse_file(str).unwrap());

    assert_eq!(eval_str("a: 1"), cue_val!({a: (1)}).into());
    assert_eq!(eval_str("a: 1, b: 2"), cue_val!({a: (1), b: (2)}).into());
}

pub fn eval_decls(decls: Rc<[ast::Declaration]>, scope: Rc<Scope>) -> Rc<value::Value> {
    let mut  current_value = Rc::from(value::Value::Top);
    for decl in decls.iter() {
        match decl {
            ast::Declaration::Bad => todo!(),
            ast::Declaration::Attribute(_) => todo!(),
            ast::Declaration::Field(f) => {
                let field = eval_field(f.clone(), scope.clone());

                current_value = current_value.meet(value::Value::Struct([field].into()).into()); // TODO?

                // TODO: expand scope
            },
            ast::Declaration::Alias(_) => todo!(),
            ast::Declaration::Comprehension(_) => todo!(),
            ast::Declaration::Ellipsis(_) => todo!(),
            ast::Declaration::LetClause(_) => todo!(),
            ast::Declaration::Embedding(_) => todo!(),
        }
    }

    // TODO: meet fields

    current_value
}

pub fn eval_field(field: ast::Field, scope: Rc<Scope>) -> value::Field {
    let label = match field.label {
        ast::Label::Ident(n, lm) => value::Label::Single(n.name, n.kind, lm),
        ast::Label::Alias(_, _) => todo!(),
        ast::Label::String(str, lm) => value::Label::Single(eval_interpolation(str, scope.clone()), None, lm),
        ast::Label::Paren(expr, lm) => value::Label::Single(eval_expr(expr, scope.clone()).to_string().into(), None, lm), // TODO?
        ast::Label::Bracket(expr) => {
            // TODO: add to scope if AliasExpr
            value::Label::Bulk(eval_expr(expr, scope.clone()).into())
        }
    };

    let value = Rc::from(eval_expr(field.value, scope));

    value::Field {
        label,
        value,
    }
}

pub fn eval_interpolation(str: ast::Interpolation, scope: Rc<Scope>) -> Rc<str> {
    todo!()
}

pub fn eval_expr(expr: ast::Expr, scope: Rc<Scope>) -> Rc<value::Value> {
    match expr {
        ast::Expr::Bad => todo!(),
        ast::Expr::Alias(_) => todo!(),
        ast::Expr::Comprehension(_) => todo!(),
        ast::Expr::Ident(_) => todo!(),
        ast::Expr::QualifiedIdent(_, _) => todo!(),
        ast::Expr::BasicLit(ast::BasicLit::Bool(b)) => value::Value::Bool(Some(b)).into(),
        ast::Expr::BasicLit(ast::BasicLit::Int(i)) => value::Value::Int(value::Basic::Value(i)).into(),
        ast::Expr::BasicLit(ast::BasicLit::Float(f)) => value::Value::Float(value::Basic::Value(f)).into(),
        ast::Expr::BasicLit(_) => todo!(),
        ast::Expr::Struct(s) => eval_decls(s.elements, scope.as_parent()),
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
