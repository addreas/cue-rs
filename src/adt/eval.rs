use super::value;
use crate::{ast, cue_val};
use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[derive(Clone, Default)]
pub struct Scope {
    parent: Option<Rc<Scope>>, // TODO: can parent scope be made immutable despite refcells? can refcells be avoided?
    items: RefCell<HashMap<Rc<str>, RefCell<Rc<value::Value>>>>,
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
            items.get(&key).unwrap().clone().into_inner().into()
        } else if let Some(p) = self.parent.clone() {
            p.deref(key)
        } else {
            None
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
    let mut current_value = Rc::from(value::Value::Top);
    for decl in decls.iter() {
        match decl {
            ast::Declaration::Bad => todo!(),
            ast::Declaration::Attribute(_) => todo!(),
            ast::Declaration::Field(f) => {
                let field = eval_field(f.clone(), scope.clone());

                current_value = current_value.meet(value::Value::Struct([field].into()).into());
                // TODO?

                // TODO: expand scope
            }
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
        ast::Label::String(str, lm) => {
            value::Label::Single(eval_interpolation(str, scope.clone()), None, lm)
        }
        ast::Label::Paren(expr, lm) => {
            value::Label::Single(eval_expr(expr, scope.clone()).to_string().into(), None, lm)
            // TODO?
        }
        ast::Label::Bracket(expr) => {
            // TODO: add to scope if AliasExpr
            value::Label::Bulk(eval_expr(expr, scope.clone()).into())
        }
    };

    let value = Rc::from(eval_expr(field.value, scope));

    value::Field { label, value }
}

pub fn eval_interpolation(str: ast::Interpolation, scope: Rc<Scope>) -> Rc<str> {
    todo!()
}

pub fn eval_expr(expr: ast::Expr, scope: Rc<Scope>) -> Rc<value::Value> {
    match expr {
        ast::Expr::Bad => value::Value::Bottom.into(),
        ast::Expr::Alias(_) => todo!(),
        ast::Expr::Comprehension(_) => todo!(),
        ast::Expr::Ident(n) => {
            // https://cuelang.org/docs/references/spec/#predeclared-identifiers
            let predeclared = match n.name.as_ref() {
                "bool" => cue_val!((bool)),
                "int" => cue_val!((int)),
                "float" => cue_val!((float)),
                "string" => cue_val!((string)),
                "bytes" => cue_val!((bytes)),

                "number" => cue_val!((int) | (float)),
                "uint" => cue_val!((>=0)),
                "uint8" => cue_val!((>=0) & (<=255)),
                "int8" => cue_val!((>=-128) & (<=127)),
                "uint16" => cue_val!((>=0) & (<=65535)),
                "int16" => cue_val!((>=-32_768) & (<=32_767)),
                "rune" => cue_val!((>=0) & (<=0x10FFFF)),
                "uint32" => cue_val!((>=0) & (<=4_294_967_295)),
                "int32" => cue_val!((>=-2_147_483_648) & (<=2_147_483_647)),
                // "uint64" => cue_val!((>=0)
                //                    & (<=18_446_744_073_709_551_615)), // TODO
                "int64" => cue_val!((>=-9_223_372_036_854_775_808)
                                  & (<=9_223_372_036_854_775_807)),
                // "uint128" => cue_val!((>=0)
                //                    & (<=340_282_366_920_938_463_463_374_607_431_768_211_455)), // TODO
                // "int128" => cue_val!((>=-170_141_183_460_469_231_731_687_303_715_884_105_728)
                //                     & (<=170_141_183_460_469_231_731_687_303_715_884_105_727)), // TODO
                "float32" => cue_val!((>=-3.40282346638528859811704183484516925440e+38)
                                    & (<=3.40282346638528859811704183484516925440e+38)),
                "float64" => cue_val!((>=-1.797693134862315708145274237317043567981e+308)
                                    & (<=1.797693134862315708145274237317043567981e+308)),

                _ => value::Value::Bottom,
            };
            if predeclared != value::Value::Bottom {
                predeclared.into()
            } else if let Some(val) = scope.deref(n.name) {
                // TODO: include n.kind, should scope be keyed with an Ident instead?
                val.into()
            } else {
                value::Value::Bottom.into()
            }
        }
        ast::Expr::QualifiedIdent(_, _) => todo!(),
        ast::Expr::BasicLit(ast::BasicLit::Null) => value::Value::Null.into(),
        ast::Expr::BasicLit(ast::BasicLit::Bottom) => value::Value::Bottom.into(),
        ast::Expr::BasicLit(ast::BasicLit::Bool(b)) => value::Value::Bool(Some(b)).into(),
        ast::Expr::BasicLit(ast::BasicLit::Int(i)) => {
            value::Value::Int(value::Basic::Value(i)).into()
        }
        ast::Expr::BasicLit(ast::BasicLit::Float(f)) => {
            value::Value::Float(value::Basic::Value(f)).into()
        }
        ast::Expr::BasicLit(ast::BasicLit::String(s)) => match s {
            ast::Interpolation::Simple(s) => value::Value::String(value::Basic::Value(s)).into(),
            ast::Interpolation::Interpolated(_, _) => todo!(),
        },
        ast::Expr::BasicLit(ast::BasicLit::Bytes(s)) => match s {
            ast::Interpolation::Simple(s) => value::Value::Bytes(value::Basic::Value(s)).into(),
            ast::Interpolation::Interpolated(_, _) => todo!(),
        },
        ast::Expr::Struct(s) => eval_decls(s.elements, scope.as_parent()),
        ast::Expr::List(l) => value::Value::List(
            l.elements
                .into_iter()
                .map(|e| eval_expr(e.clone(), scope.clone())) // TODO: e.clone() is not an rc clone
                .collect(),
        )
        .into(),
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
