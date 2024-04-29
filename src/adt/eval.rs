use crate::{
    adt::environment::{MutableEnvironment, Environment},
    ast::{self, Interpolation}, cue_val,
    value::{Field, Value, FieldName},
};
use std::{cell::RefCell, fmt::Debug, rc::{Rc, Weak}, vec};

pub type NodeRef = Rc<RefCell<Node>>;
pub type WeakNodeRef = Weak<RefCell<Node>>;
#[derive(Debug, Clone)]
pub enum Node {
    Bottom,
    Value(Rc<Value>),

    Interpolation(Rc<[NodeRef]>),
    List(Rc<[NodeRef]>),
    Struct(Rc<[Edge]>, Rc<[NodeRef]>),
    Selector(NodeRef, Selector),
    Reference(WeakNodeRef),
    Index(NodeRef, NodeRef),
    Slice(NodeRef, Option<NodeRef>, Option<NodeRef>),
    Guard(NodeRef, NodeRef),
    For(NodeRef, ast::Ident, Option<ast::Ident>, NodeRef),

    Expr(ast::Expr, MutableEnvironment<NodeRef>),
    Declarations(Rc<[ast::Declaration]>, MutableEnvironment<NodeRef>),
}

impl Node {
    pub fn into_ref(self) -> NodeRef {
        Rc::new(RefCell::new(self))
    }
}

#[derive(Debug, Clone)]
pub enum Edge {
    Defined(Selector, NodeRef),
    Constraint(Selector, ast::FieldConstraint, NodeRef),
    Pattern(NodeRef, NodeRef),
}

#[derive(Debug, Clone)]
pub enum Selector {
    Ident(ast::Ident),
    Interpolation(NodeRef)
}

pub struct Interpreter {}

impl Interpreter {
    pub fn eval(source: ast::SourceFile) -> Rc<Value> {
        println!("eval");
        let interpreter = Interpreter {};
        let env = Environment::new();
        Self::init_predeclared(env.clone());

        let result = Rc::new(RefCell::new(Node::Declarations(source.declarations, env)));
        let mut unfinished = vec![result.clone()];
        while let Some(next) = unfinished.pop() {
            println!("unfinished len: {}", unfinished.len());
            // println!("next: {:?}", next.borrow());
            let (result, more) = interpreter.eval_node(next.clone(), &next.borrow());

            match result {
                Node::Value(_) => {},
                _ => {
                    println!("unfinished push result: {:#?}", result);
                    println!("unfinished push next: {:#?}", next);
                    if unfinished.len() == 13 {
                        break
                    }
                    unfinished.push(next.clone())
                },
            };
            if more.len() > 0 {
                println!("unfinished extend: {:#?}", more);
                unfinished.extend(more);
            }
            next.replace(result);
        }

        match &*result.clone().borrow() {
            Node::Value(res) => res.clone(),
            e => {
                println!("e: {e:#?}");
                println!("ee: {unfinished:#?}");
                todo!()
            },
        }
    }

    pub fn eval_node(&self, outer: NodeRef, node: &Node) -> (Node, Vec<NodeRef>) {
        println!("eval_node");
        match node {
            Node::Value(_) => unreachable!(),

            Node::Bottom => todo!(),

            Node::Declarations(decls, env) => self.eval_ast_decls(decls.clone(), env.clone()),
            Node::Expr(expr, env) => self.eval_ast_expr(outer, expr, env.clone()),

            Node::Struct(edges, embeddings) => self.eval_struct(edges.clone(), embeddings.clone()),
            Node::List(elems) => self.eval_list(elems.clone()),

            Node::Interpolation(parts) => self.eval_interpolation(parts.clone()),
            Node::Reference(r) => self.eval_reference(r.clone()),
            Node::Selector(source, selector) => self.eval_selector(source.clone(), selector),
            Node::Index(source, index) => self.eval_index(source.clone(), index.clone()),
            Node::Slice(source, low, high) => self.eval_slice(source.clone(), low.clone(), high.clone()),

            Node::Guard(_, _) => todo!(),
            Node::For(_, _, _, _) => todo!(),
        }
    }
    pub fn eval_struct(&self, edges: Rc<[Edge]>, embeddings: Rc<[NodeRef]>) -> (Node, Vec<NodeRef>) {
        println!("eval_struct");
        let fields: Vec<_> = edges.iter().map(|e| match e {
            Edge::Defined(Selector::Ident(name), value) => match &*value.borrow() {
                Node::Value(v) => Some(Field::Defined(FieldName::Ident(name.clone()), v.clone())),
                _ => None
            }
            Edge::Constraint(Selector::Ident(name), constraint, value) => match &*value.borrow() {
                Node::Value(v) => Some(Field::Constraint(FieldName::Ident(name.clone()), constraint.clone(), v.clone())),
                _ => None
            }
            Edge::Pattern(pat, val) => match &*pat.borrow() {
                Node::Value(v) => Some(Field::Pattern(v.clone(), |_| Value::Bottom.into())),
                _ => None
            }
            _ => None
        }).collect();
        let embeds: Vec<_> = embeddings.iter().map(|e| match &*e.borrow() {
            Node::Value(v) => Some(v.clone()),
            _ => None
        }).collect();

        println!("fields: {fields:?}");
        println!("embeds: {embeds:?}");
        if fields.iter().all(Option::is_some) && embeds.iter().all(Option::is_some) {
            let actual_fields = fields.into_iter().map(Option::unwrap).collect();
            let mut val = Rc::from(Value::Struct(actual_fields, false));

            if let Some(v) = embeds.into_iter().map(Option::unwrap).reduce(|e, v| e.meet(v)) {
                val = val.meet(v);
            };

            return (Node::Value(val), vec![])
        }
        (Node::Struct(edges, embeddings), vec![])
    }

    pub fn eval_list(&self, thing: Rc<[NodeRef]>) -> (Node, Vec<NodeRef>) {
        println!("eval_list");
        (todo!(), vec![])
    }

    pub fn eval_interpolation(&self, parts: Rc<[NodeRef]>) -> (Node, Vec<NodeRef>) {
        println!("eval_interpolation");
        (todo!(), vec![])
    }

    pub fn eval_reference(&self, reference: WeakNodeRef) -> (Node, Vec<NodeRef>) {
        println!("eval_reference");
        let reff = reference.upgrade().expect("reference should exist in env anyway");
        let beep_boop = reff.borrow();
        match &*beep_boop {
            Node::Bottom => (Node::Bottom, vec![]),
            Node::Value(v) => (Node::Value(v.clone()), vec![]),
            Node::Reference(r) if !self.is_cycle(reference.clone(), r.clone()) => {
                let reff = r.upgrade().expect("shoudl exist in env");
                let b = reff.borrow();
                self.eval_node(reff.clone(), &b)
            },
            Node::Reference(r) => todo!("reference cycle"),
            Node::Expr(expr, env) => self.eval_ast_expr(reff.clone(), expr, env.clone()),
            Node::Selector(source, selector) => self.eval_selector(source.clone(), selector),
            Node::Struct(edges, embeddings) => self.eval_struct(edges.clone(), embeddings.clone()),
            v => {
                print!("todo: {:?}", v);
                (todo!(), todo!())
            }
            _ => (todo!(), todo!()),
        }
    }


    fn is_cycle(&self, root: WeakNodeRef, reference: WeakNodeRef) -> bool {
        match &*(reference.upgrade().unwrap().borrow()) {
            Node::Reference(r) if r.clone().into_raw() == root.clone().into_raw() => true,
            Node::Reference(r) => self.is_cycle(root, r.clone()),
            _ => false,
        }
    }

    pub fn eval_selector(&self, source: NodeRef, selector: &Selector) -> (Node, Vec<NodeRef>) {
        println!("eval_selector");
        match selector {
            Selector::Ident(ident) => match &*source.borrow() {
                Node::Value(val) => match &*val.clone() {
                    Value::Struct(fields, _) => {
                        let selected = fields.iter().find_map(|f| {
                            match f {
                                Field::Defined(name, value) => {
                                match name {
                                    FieldName::Ident(m) if m == ident => {
                                        Some(value.clone())
                                    }
                                    FieldName::String(str) if str.clone() == ident.as_str() => {
                                        Some(value.clone())
                                    }
                                    _ => None
                                }
                                }
                                _ => None
                            }
                        });

                        if let Some(v) = selected {
                            (Node::Value(v), vec![])
                        } else {
                            (Node::Bottom, vec![])
                        }
                    },
                    _ => todo!(),
                },
                Node::Struct(fields, _) => todo!(),
                _ => todo!(),
            },
            Selector::Interpolation(_) => todo!(),
        }
        // (todo!(), vec![])
    }

    pub fn eval_index(&self, source: NodeRef, index: NodeRef) -> (Node, Vec<NodeRef>) {
        println!("eval_index");
        (todo!(), vec![])
    }

    pub fn eval_slice(&self, source: NodeRef, low: Option<NodeRef>, high: Option<NodeRef>) -> (Node, Vec<NodeRef>) {
        println!("eval_slice");
        (todo!(), vec![])
    }

    pub fn eval_ast_decls(
        &self,
        decls: Rc<[ast::Declaration]>,
        env: MutableEnvironment<NodeRef>,
    ) -> (Node, Vec<NodeRef>) {
        println!("eval_ast_decls");
        let inner_env = Environment::as_parent(env);
        let mut edges = vec![];
        let mut embeddings = vec![];
        let mut unfinished = vec![];
        for decl in decls.into_iter() {
            match decl {
                ast::Declaration::Attribute(_) => todo!(),
                ast::Declaration::Field(f) => {
                    let value = Node::Expr(f.value.clone(), inner_env.clone()).into_ref();
                    unfinished.push(value.clone());

                    let (edge, todo) = self.eval_ast_label(&f.label, value.clone(), inner_env.clone());
                    if let Some(item) = todo {
                        unfinished.push(item);
                    }
                    edges.push(edge);
                }
                ast::Declaration::Alias(_) => todo!(),
                ast::Declaration::Comprehension(_) => todo!(),
                ast::Declaration::Ellipsis(_) => todo!(),
                ast::Declaration::LetClause(_) => todo!(),
                ast::Declaration::Embedding(expr) => {
                    let expr_node = Node::Expr(expr.clone(), inner_env.clone()).into_ref();
                    embeddings.push(expr_node.clone());
                    unfinished.push(expr_node.clone());
                }
            }
        }
        return (Node::Struct(edges.into(), embeddings.into()), unfinished)
    }

    pub fn eval_ast_label(&self, label: &ast::Label, value: NodeRef, env: MutableEnvironment<NodeRef>) -> (Edge, Option<NodeRef>) {
        println!("eval_ast_edge");

        match label {
            ast::Label::Single(name, lm) => {
                let mut todo = None;

                let key = match name {
                    ast::LabelName::Ident(ident) => {
                        env.borrow_mut().set(ident.clone(), value.clone());
                        Selector::Ident(ident.clone())
                    },
                    ast::LabelName::String(interpolation) => {
                        let val = match interpolation {
                            Interpolation::Simple(str) => Node::Value(Value::String(Some(str.clone())).into()),
                            Interpolation::Interpolated(_, _) => todo!(),
                        }.into_ref();
                        todo = Some(val.clone());
                        Selector::Interpolation(val.clone())
                    },
                    ast::LabelName::Dynamic(expr) => {
                        let val = Node::Expr(expr.clone(), env).into_ref();
                        todo = Some(val.clone());
                        Selector::Interpolation(val.clone())
                    },
                };

                if let Some(constraint) = lm {
                    (Edge::Constraint(key, constraint.clone(), value.clone()), todo)
                } else {
                    (Edge::Defined(key, value), todo)
                }
            }
            ast::Label::Alias(alias, label) => {
                env.borrow_mut().set(alias.clone(), value.clone());
                self.eval_ast_label(label, value.clone(), env)
            }
            ast::Label::Pattern(pat) => {
                let key = Node::Expr(pat.clone(), env.clone()).into_ref();
                (Edge::Pattern(key.clone(), value.clone()), Some(key.clone()))
            },
        }
    }

    pub fn eval_ast_expr(&self, outer: NodeRef, expr: &ast::Expr, env: MutableEnvironment<NodeRef>) -> (Node, Vec<NodeRef>) {
        println!("eval_ast_expr");
        match expr {
            ast::Expr::Alias(alis) => {
                env.borrow_mut().set(alis.ident.clone(), outer);
                (Node::Expr(alis.expr.clone(), env.clone()), vec![])
            },
            ast::Expr::Comprehension(_) => todo!(),
            ast::Expr::Ident(n) => {
                if let Some(val) = env.borrow().get(&n) {
                    (Node::Reference(Rc::downgrade(&val)), vec![])
                } else {
                    (Node::Bottom, vec![])
                }
            }
            ast::Expr::QualifiedIdent(_, _) => todo!(),
            ast::Expr::Null => (Node::Value(Value::Null.into()), vec![]),
            ast::Expr::Bottom => (Node::Value(Value::Bottom.into()), vec![]),
            ast::Expr::Bool(b) => (Node::Value(Value::Bool(Some(*b)).into()), vec![]),
            ast::Expr::Int(i) => (Node::Value(Value::Int(Some(*i)).into()), vec![]),
            ast::Expr::Float(f) => (Node::Value(Value::Float(Some(*f)).into()), vec![]),
            ast::Expr::String(s) => match s {
                ast::Interpolation::Simple(s) => (Node::Value(Value::String(Some(s.clone())).into()), vec![]),
                ast::Interpolation::Interpolated(_, _) => todo!(),
            },
            ast::Expr::Bytes(s) => match s {
                ast::Interpolation::Simple(s) => (Node::Value(Value::Bytes(Some(s.clone())).into()), vec![]),
                ast::Interpolation::Interpolated(_, _) => todo!(),
            },
            ast::Expr::Struct(s) => self.eval_ast_decls(s.clone(), Environment::as_parent(env)),
            ast::Expr::List(l) => {
                let nodes: Vec<_> = l.into_iter()
                    .map(|e| Node::Expr(e.clone(), env.clone()).into_ref())
                    .collect();
                (Node::List(nodes.clone().into()), nodes)
            },
            ast::Expr::Ellipsis(_) => todo!(),
            ast::Expr::UnaryExpr(_, _) => todo!(),
            ast::Expr::BinaryExpr(lhs, op, rhs) => {
                let lval = self.eval_ast_expr(outer.clone(), lhs, env.clone());
                let rval = self.eval_ast_expr(outer.clone(), rhs, env.clone());

                match op {
                    // ast::Operator::Conjunct => lval.meet(rval),
                    // ast::Operator::Disjunct => lval.join(rval),

                    _ => todo!(),
                }
            }
            ast::Expr::Parens(e) => self.eval_ast_expr(outer, e, env),
            ast::Expr::Selector(e, selector) => {
                let source = Node::Expr(*e.clone(), env.clone()).into_ref();
                match *selector.clone() {
                    ast::Selector::Ident(i) => {
                        (Node::Selector(source.clone(), Selector::Ident(i)), vec![source.clone()])
                    },
                    ast::Selector::String(s) => {
                        // :(
                        let (interpolation, more) = self.eval_ast_interpolation(s, env.clone());
                        let interpolation_ref = interpolation.into_ref();
                        let mut even_more = vec![source.clone(), interpolation_ref.clone()];
                        even_more.extend(more);
                        (Node::Selector(source.clone(), Selector::Interpolation(interpolation_ref.clone())), even_more)
                    },
                }
            }
            ast::Expr::Index(_, _) => todo!(),
            ast::Expr::Slice(_, _, _) => todo!(),
            ast::Expr::Call(_, _) => todo!(),
        }
    }

    pub fn eval_ast_interpolation(
        &self,
        _str: ast::Interpolation,
        _env: MutableEnvironment<NodeRef>,
    ) -> (Node, Vec<NodeRef>) {
        println!("eval_ast_interpolation");
        todo!()
    }

    fn init_predeclared(env: MutableEnvironment<NodeRef>) {
        // https://cuelang.org/docs/references/spec/#predeclared-identifiers
        let predeclared = vec![
            ("bool", cue_val!((bool))),
            ("int", cue_val!((int))),
            ("float", cue_val!((float))),
            ("string", cue_val!((string))),
            ("bytes", cue_val!((bytes))),

            ("number", cue_val!((int) | (float))),
            ("uint", cue_val!((>=0))),
            ("uint8", cue_val!((>=0) & (<=255))),
            ("int8", cue_val!((>=-128) & (<=127))),
            ("uint16", cue_val!((>=0) & (<=65535))),
            ("int16", cue_val!((>=-32_768) & (<=32_767))),
            ("rune", cue_val!((>=0) & (<=0x10FFFF))),
            ("uint32", cue_val!((>=0) & (<=4_294_967_295))),
            ("int32", cue_val!((>=-2_147_483_648) & (<=2_147_483_647))),
            // ("uint64", cue_val!((>=0) & (<=18_446_744_073_709_551_615))), // TODO
            ("int64", cue_val!((>=-9_223_372_036_854_775_808) & (<=9_223_372_036_854_775_807))),
            // ("uint128", cue_val!((>=0) & (<=340_282_366_920_938_463_463_374_607_431_768_211_455))), // TODO
            // ("int128", cue_val!((>=-170_141_183_460_469_231_731_687_303_715_884_105_728) & (<=170_141_183_460_469_231_731_687_303_715_884_105_727))), // TODO
            ("float32", cue_val!((>=-3.40282346638528859811704183484516925440e+38) & (<=3.40282346638528859811704183484516925440e+38))),
            ("float64", cue_val!((>=-1.797693134862315708145274237317043567981e+308) & (<=1.797693134862315708145274237317043567981e+308))),
        ];

        let mut s = env.borrow_mut();
        for (name, val) in predeclared.into_iter() {
            s.set(ast::Ident {name: name.into(), kind: None}, Node::Value(val.into()).into_ref())
        }
    }
}

#[test]
fn test_source_file1() {
    use crate::cue_val;
    use crate::parser::parse_file;
    let eval_str = |str| Interpreter::eval(parse_file(str).unwrap());

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
}

#[test]
fn test_source_file2() {
    use crate::cue_val;
    use crate::parser::parse_file;
    let eval_str = |str| Interpreter::eval(parse_file(str).unwrap());
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

#[test]
fn test_source_file3() {
    use crate::cue_val;
    use crate::parser::parse_file;
    let eval_str = |str| Interpreter::eval(parse_file(str).unwrap());

    assert_eq!(
        eval_str(
            r#"{
                a: {
                    b: {
                        c: {
                            i: x.x
                            j: x.y
                            d: {
                                e: 3
                            }
                        }
                    }
                }

                x: {
                    x: a.b.c.d.e
                    y: 2
                }
            }"#
        ),
        cue_val!({
            (a): ({
                (b): ({
                    (c): ({
                        (i): (3),
                        (j): (2),
                        (d): ({
                            (e): (3)
                        })
                    })
                })
            }),
            (x): ({
                (x): (3),
                (y): (2)
            })
        })
        .into()
    );
}

#[test]
fn test_source_file4() {
    use crate::cue_val;
    use crate::parser::parse_file;
    let eval_str = |str| Interpreter::eval(parse_file(str).unwrap());

    assert_eq!(
        eval_str(
            r#"{
                a: b: c: 1

                x: a.b.c
            }"#
        ),
        cue_val!({
            (a): ({
                (b): ({
                    (c): (1)
                })
            }),
            (x): (1)
        })
        .into()
    );
    assert_eq!(
        eval_str(
            r#"{
                x: a.b.c

                a: b: c: 1
            }"#
        ),
        cue_val!({
            (x): (1),
            (a): ({
                (b): ({
                    (c): (1)
                })
            })
        })
        .into()
    );
}

#[test]
fn test_source_file5() {
    use crate::cue_val;
    use crate::parser::parse_file;
    let eval_str = |str| Interpreter::eval(parse_file(str).unwrap());

    assert_eq!(
        eval_str(
            r#"{
                a: b: 1
                x: y: 2

                a: x
                x: a
            }"#
        ),
        cue_val!({
            (a): ({
                (b): (1),
                (y): (2)
            }),
            (x): ({
                (b): (1),
                (y): (2)
            })
        })
        .into()
    );
}
