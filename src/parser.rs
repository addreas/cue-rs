use std::vec;

use pest::{prec_climber::{PrecClimber,Operator,Assoc}};

use crate::ast;
use pest_consume::{match_nodes, Error, Parser};

#[derive(Parser)]
#[grammar = "cue.pest"]
pub struct CUEParser;

lazy_static::lazy_static! {
    static ref PRECCLIMBER: PrecClimber<Rule> = PrecClimber::new(
        vec![
            Operator::new(Rule::disjunct_op, Assoc::Left),
            Operator::new(Rule::conjuct_op, Assoc::Left),
            Operator::new(Rule::or_op, Assoc::Left),
            Operator::new(Rule::and_op, Assoc::Left),
            Operator::new(Rule::rel_op, Assoc::Left) | Operator::new(Rule::equal_op, Assoc::Left),
            Operator::new(Rule::add_op, Assoc::Left),
            Operator::new(Rule::mul_op, Assoc::Left),
        ]
    );
}

type Result<T> = std::result::Result<T, Error<Rule>>;
type Node<'i> = pest_consume::Node<'i, Rule, ()>;

#[allow(unused_macros)]
macro_rules! parse_single {
    ($rule:ident, $input:expr) =>  {
        CUEParser::parse(Rule::$rule, $input)
            .and_then(|r| r.single())
            .and_then(|r| CUEParser::$rule(r))
    };
}

fn interpolation_elements(input: Node) -> Result<Vec<ast::Expr>> {
    let mut current_str = String::new();
    let mut string_parts = vec![];
    let mut expr_parts = vec![];

    for n in input.clone().into_children() {
        // println!("interpolation_elements: {:?}: {}", n.as_rule(), n.as_str());
        match n.as_rule() {
            Rule::escaped_char => current_str.push(CUEParser::escaped_char(n)?),
            Rule::octal_byte_value => current_str.push(CUEParser::octal_byte_value(n)?),
            Rule::hex_byte_value => current_str.push(CUEParser::hex_byte_value(n)?),
            Rule::little_u_value => current_str.push(CUEParser::little_u_value(n)?),
            Rule::big_u_value => current_str.push(CUEParser::big_u_value(n)?),
            Rule::unicode_value => current_str.push(CUEParser::unicode_value(n)?),
            Rule::byte_value => current_str.push(CUEParser::byte_value(n)?),
            Rule::newline => current_str.push('\n'),
            Rule::Interpolation => {
                string_parts.push(current_str.clone());
                current_str.clear();

                expr_parts.push(CUEParser::Interpolation(n)?);
            }
            Rule::ending_indent => {
                let pattern = format!("\n{}", n.as_str());
                current_str = current_str
                    .trim_start_matches(n.as_str())
                    .to_string()
                    .replace(pattern.as_str(), "\n");

                string_parts = string_parts
                    .iter()
                    .inspect(|s| println!("replacing '{}' in '{}'", pattern, s))
                    .map(|s| s.replace(pattern.as_str(), "\n"))
                    .collect();
            }
            _ => unreachable!("unreachable interpolation_element: {:#?}", n.as_rule()),
        }
    }
    string_parts.push(current_str);

    let mut expr_iterator = expr_parts.iter();

    return Ok(string_parts
        .iter()
        .map(|s| ast::new_str(s.clone()))
        .intersperse_with(|| {
            expr_iterator
                .next()
                .cloned()
                .expect("grammar should ensure len(expr_parts) > len(string_parts)")
        })
        .collect());
}

fn parse_digits_radix(input: Node, radix: u32) -> Result<char> {
    let escape = input.clone().into_pair().into_inner().as_str();
    let digits = input.as_str().trim_start_matches(escape);
    match u32::from_str_radix(digits, radix) {
        Ok(char_value) => match char::from_u32(char_value) {
            Some(c) => Ok(c),
            None => Err(input.error("parse_digits_radix: invalid char")),
        },
        _ => Err(input.error("parse_digits_radix: invalid digits")),
    }
}

fn binary_op(input: Node) -> Result<ast::Operator> {
    match input.as_rule() {
        Rule::rel_op => CUEParser::rel_op(input),
        Rule::add_op => CUEParser::add_op(input),
        Rule::mul_op => CUEParser::mul_op(input),
        Rule::binary_op => match input.into_children().single()?.as_rule() {
            Rule::disjunct_op => Ok(ast::Operator::Disjunct),
            Rule::conjuct_op => Ok(ast::Operator::Conjunct),
            Rule::or_op => Ok(ast::Operator::Or),
            Rule::and_op => Ok(ast::Operator::And),
            Rule::equal_op => Ok(ast::Operator::Equal),
            x => panic!("binary_op {:?}", x),
        },
        x => panic!("binary_op {:?}", x),
    }
}

// This is the other half of the parser, using pest_consume.
#[pest_consume::parser]
#[allow(dead_code, non_snake_case)]
impl CUEParser {
    fn identifier(input: Node) -> Result<ast::Ident> {
        Ok(ast::Ident {
            name: input.as_str(),
        })
    }
    fn unicode_char(input: Node) -> Result<char> {
        input
            .as_str()
            .chars()
            .next()
            .map_or(Err(input.error("no first char in str")), |c| Ok(c))
    }
    fn decimal_lit(input: Node) -> Result<i64> {
        input
            .as_str()
            .replace("_", "")
            .as_str()
            .parse()
            .map_err(|e| input.error(format!("decimal_lit: {}: {}", input.as_str(), e)))
    }
    fn fraction(input: Node) -> Result<f64> {
        ("0.".to_owned() + input.as_str().replace("_", "").as_str())
            .parse()
            .map_err(|e| input.error(format!("fraction: {}: {}", input.as_str(), e)))
    }
    fn si_lit(input: Node) -> Result<i64> {
        Ok(match_nodes!(input.into_children();
            [decimal_lit(a), fraction(b), multiplier(m)] => ((a as f64 + b) * m  as f64) as i64,
            [decimal_lit(a),              multiplier(m)] => ((a             * m) as f64) as i64,
            [                fraction(b), multiplier(m)] => (            b  * m  as f64) as i64))
    }
    fn binary_lit(input: Node) -> Result<i64> {
        i64::from_str_radix(input.as_str(), 2)
            .map_err(|e| input.error(format!("binary_lit: {}: {}", input.as_str(), e)))
    }
    fn octal_lit(input: Node) -> Result<i64> {
        i64::from_str_radix(input.as_str(), 8)
            .map_err(|e| input.error(format!("octal_lit: {}: {}", input.as_str(), e)))
    }
    fn hex_lit(input: Node) -> Result<i64> {
        i64::from_str_radix(input.as_str(), 16)
            .map_err(|e| input.error(format!("hex_lit: {}: {}", input.as_str(), e)))
    }
    fn multiplier(input: Node) -> Result<i64> {
        match input.as_str() {
            "K" => Ok(i64::pow(10, 3)),
            "M" => Ok(i64::pow(10, 6)),
            "G" => Ok(i64::pow(10, 9)),
            "T" => Ok(i64::pow(10, 12)),
            "P" => Ok(i64::pow(10, 15)),
            "Ki" => Ok(i64::pow(2, 10)),
            "Mi" => Ok(i64::pow(2, 20)),
            "Gi" => Ok(i64::pow(2, 30)),
            "Ti" => Ok(i64::pow(2, 40)),
            "Pi" => Ok(i64::pow(2, 50)),
            _ => Err(input.error(format!("unknown multiplier: {}", input.as_str()))),
        }
    }
    fn exponent(input: Node) -> Result<i32> {
        Ok(match_nodes!(input.into_children(); [decimal_lit(n)] => n as i32))
    }
    fn int_lit(input: Node) -> Result<i64> {
        Ok(match_nodes!(input.into_children();
            [si_lit(n)] => n,
            [decimal_lit(n)] => n,
            [binary_lit(n)] => n,
            [octal_lit(n)] => n,
            [hex_lit(n)] => n,
        ))
    }
    fn float_lit(input: Node) -> Result<f64> {
        Ok(match_nodes!(input.into_children();
            [decimal_lit(a), fraction(b), exponent(e)] => (a as f64 + b) * f64::powi(10.0, e),
            [decimal_lit(a), fraction(b)             ] =>  a as f64 + b,
            [decimal_lit(a),              exponent(e)] =>  a as f64      * f64::powi(10.0, e),
            [                fraction(b), exponent(e)] =>             b  * f64::powi(10.0, e),
            [                fraction(b)             ] =>             b,
        ))
    }
    fn escaped_char(input: Node) -> Result<char> {
        // println!("escaped_char: {:?}", input.as_str());
        match input.as_str().chars().last() {
            Some('a') => Ok('\u{0007}'),
            Some('b') => Ok('\u{0008}'),
            Some('f') => Ok('\u{000C}'),
            Some('n') => Ok('\n'),
            Some('r') => Ok('\r'),
            Some('t') => Ok('\t'),
            Some('v') => Ok('\u{000b}'),
            Some(x) => Ok(x),
            x => Err(input.error(format!("escaped_char: unknown escape char {:?}", x))),
        }
    }
    fn octal_byte_value(input: Node) -> Result<char> {
        // println!("octal_byte_value: {:?}", input.as_str());
        parse_digits_radix(input, 8)
    }
    fn hex_byte_value(input: Node) -> Result<char> {
        // println!("hex_byte_value: {:?}", input.as_str());
        parse_digits_radix(input, 16)
    }
    fn little_u_value(input: Node) -> Result<char> {
        // println!("littel_u_value: {:?}", input.as_str());
        parse_digits_radix(input, 16)
    }
    fn big_u_value(input: Node) -> Result<char> {
        // println!("big_u_value: {:?}", input.as_str());
        parse_digits_radix(input, 16)
    }
    fn unicode_value(input: Node) -> Result<char> {
        // println!("unicode_value: {:?}", input.as_str());
        Ok(match_nodes!(input.clone().into_children();
            [little_u_value(c)] => c,
            [big_u_value(c)] => c,
            [escaped_char(c)] => c,
            [unicode_char(c)] => c,
        ))
    }
    fn byte_value(input: Node) -> Result<char> {
        // println!("byte_value: {:?}", input.as_str());
        Ok(match_nodes!(input.into_children();
            [octal_byte_value(c)] => c,
            [hex_byte_value(c)] => c,
        ))
    }
    fn Interpolation(input: Node) -> Result<ast::Expr> {
        // println!("Iterpolation: {:?}", input.as_str());
        CUEParser::Expression(input.into_children().single()?)
    }
    fn String(input: Node) -> Result<ast::Interpolation> {
        // println!("String: {:?}", input.as_str());
        Ok(match_nodes!(input.into_children();
            [SimpleString(s)] => s,
            [MultilineString(s)] => s,
            [SimpleBytes(s)] => s,
            [MultilineBytes(s)] => s))
    }
    fn SimpleString(input: Node) -> Result<ast::Interpolation> {
        // println!("SimpleString: {:?}", input);
        Ok(ast::Interpolation {
            is_bytes: false,
            elements: interpolation_elements(input)?,
        })
    }
    fn SimpleBytes(input: Node) -> Result<ast::Interpolation> {
        // println!("SimpleBytes: {:?}", input);
        Ok(ast::Interpolation {
            is_bytes: true,
            elements: interpolation_elements(input)?,
        })
    }
    fn MultilineString(input: Node) -> Result<ast::Interpolation> {
        // println!("MultilineString: {:?}", input);
        Ok(ast::Interpolation {
            is_bytes: false,
            elements: interpolation_elements(input)?,
        })
    }
    fn MultilineBytes(input: Node) -> Result<ast::Interpolation> {
        // println!("MultilineBytes: {:?}", input);
        Ok(ast::Interpolation {
            is_bytes: true,
            elements: interpolation_elements(input)?,
        })
    }
    fn bottom_lit(input: Node) -> Result<()> {
        Ok(())
    }
    fn null_lit(input: Node) -> Result<()> {
        Ok(())
    }
    fn bool_lit(input: Node) -> Result<bool> {
        match input.as_str() {
            "true" => Ok(true),
            "false" => Ok(false),
            _ => unreachable!(),
        }
    }
    fn StructLit(input: Node) -> Result<ast::StructLit> {
        Ok(ast::StructLit {
            elements: input
                .into_children()
                .map(|c| CUEParser::Declaration(c))
                .try_collect()?,
        })
    }
    fn Declaration(input: Node) -> Result<ast::Declaration> {
        Ok(match_nodes!(input.into_children();
            [Field(f)] => ast::Declaration::Field(f),
            [Ellipsis(e)] => ast::Declaration::Ellipsis(e),
            [Embedding(e)] => ast::Declaration::Embedding(e),
            [attribute(e)] => ast::Declaration::Attribute(e),
        ))
    }
    fn Ellipsis(input: Node) -> Result<ast::Ellipsis> {
        Ok(ast::Ellipsis {
            inner: CUEParser::Expression(input.into_children().single()?)?,
        })
    }
    fn Embedding(input: Node) -> Result<ast::Expr> {
        Ok(match_nodes!(input.into_children();
            [Comprehension(c)] => ast::Expr::Comprehension(Box::new(c)),
            [AliasExpr(a)] => a,
        ))
    }
    fn Field(input: Node) -> Result<ast::Field> {
        match_nodes!(input.into_children();
        [Labels(ls), AliasExpr(value), attribute(attributes)..] => {
            let mut lss = ls.clone();
            let mut lsi = lss.iter_mut();
            let init = ast::Field {
                label: lsi.next().expect("nonempty list").clone(),
                value: value,
                attributes: attributes.collect(),
            };
            return Ok(lsi.rfold(init, |acc, label| ast::Field {
                label: label.clone(),
                value: ast::Expr::Struct(ast::StructLit {
                    elements: vec![ast::Declaration::Field(acc)],
                }),
                attributes: vec![],
            }));
        })
    }
    fn Labels(input: Node) -> Result<Vec<ast::Label>> {
        Ok(match_nodes!(input.into_children();
            [Label(labels)..] => labels.collect()))
    }
    fn Label(input: Node) -> Result<ast::Label> {
        Ok(match_nodes!(input.into_children();
            [LabelName(n)] => n,
            [Expression(e)] => ast::Label::Paren(e),
            [AliasExpr(a)] => ast::Label::Bracket(a),
        ))
    }
    fn LabelName(input: Node) -> Result<ast::Label> {
        Ok(match_nodes!(input.into_children();
            [identifier(i)] => ast::Label::Ident(i),
            [SimpleString(s)] => ast::Label::Basic(s)
        ))
    }
    fn attribute(input: Node) -> Result<ast::Attribute> {
        Ok(ast::Attribute {
            text: input.as_str(),
        })
    }
    fn attr_tokens(input: Node) -> Result<()> {
        Err(input.error("attr_tokens should be unreachable"))
    }
    fn attr_token(input: Node) -> Result<()> {
        Err(input.error("attr_token should be unreachable"))
    }
    fn AliasExpr(input: Node) -> Result<ast::Expr> {
        Ok(match_nodes!(input.into_children();
            [identifier(ident), Expression(expr)] => ast::Expr::Alias(Box::new(ast::Alias {
                ident,
                expr,
            })),
            [Expression(expr)] => expr
        ))
    }
    fn ListLit(input: Node) -> Result<ast::ListLit> {
        Ok(ast::ListLit {
            elements: CUEParser::ElementList(input.into_children().single()?)?
        })
    }
    fn ElementList(input: Node) -> Result<Vec<ast::Expr>> {
        Ok(match_nodes!(input.into_children();
            [Ellipsis(e)] => vec![ast::Expr::Ellipsis(Box::new(e))],
            [Embedding(e)..] => e.collect(),
            [Embedding(em).., Ellipsis(el)] => {
                let mut res: Vec<_> = em.collect();
                res.push(ast::Expr::Ellipsis(Box::new(el)));
                res
            }))
    }
    fn Literal(input: Node) -> Result<ast::Expr> {
        Ok(match_nodes!(input.into_children();
            [BasicLit(b)] => ast::Expr::BasicLit(b),
            [ListLit(l)] => ast::Expr::List(l),
            [StructLit(s)] => ast::Expr::Struct(s)))
    }
    fn Operand(input: Node) -> Result<ast::Expr> {
        Ok(match_nodes!(input.into_children(); 
            [Literal(l)] => l,
            [OperandName(o)] => o,
            [Expression(e)] => ast::Expr::Parens { inner: Box::new(e)}))
    }
    fn BasicLit(input: Node) -> Result<ast::BasicLit> {
        match_nodes!(input.into_children();
            [int_lit(l)] => Ok(ast::BasicLit::Int(l)),
            [float_lit(l)] => Ok(ast::BasicLit::Float(l)),
            [bool_lit(l)] => Ok(ast::BasicLit::Bool(l)),
            [null_lit(_)] => Ok(ast::BasicLit::Null),
            [bottom_lit(_)] => Ok(ast::BasicLit::Bottom),
        )
    }
    fn OperandName(input: Node) -> Result<ast::Expr> {
        Ok(match_nodes!(input.into_children();
            [identifier(i)] => ast::Expr::Ident(i),
            [QualifiedIdent(qi)] => qi))
    }
    fn QualifiedIdent(input: Node) -> Result<ast::Expr> {
        Ok(match_nodes!(input.into_children();
            [PackageName(p), identifier(i)] => ast::Expr::QualifiedIdent(p, i)))
    }
    fn Selector(input: Node) -> Result<ast::Label> {
        Ok(match_nodes!(input.into_children();
            [identifier(i)] => ast::Label::Ident(i),
            [SimpleString(s)] => ast::Label::Basic(s)))
    }
    fn Index(input: Node) -> Result<ast::Expr> {
        CUEParser::Expression(input.into_children().single()?)
    }
    fn Slice(input: Node) -> Result<(ast::Expr, ast::Expr)> {
        Ok(match_nodes!(input.into_children();
            [Expression(low), Expression(high)] => (low, high)))
    }
    fn Argument(input: Node) -> Result<ast::Expr> {
        CUEParser::Expression(input.into_children().single()?)
    }
    fn Arguments(input: Node) -> Result<Vec<ast::Expr>> {
        Ok(match_nodes!(input.into_children(); 
            [Argument(a)..] => a.collect()))
    }
    fn PrimaryExpr(input: Node) -> Result<ast::Expr> {
        match_nodes!(input.into_children(); 
            [Operand(o)] => Ok(o),
            [Operand(o), items] => {
                let mut current_expr = o;
                for item in items.into_children() {
                    let source = Box::new(current_expr);
                    current_expr = match item.as_rule() {
                        Rule::Selector => ast::Expr::Selector {
                            source,
                            field: Box::new(CUEParser::Selector(item)?)
                        },
                        Rule::Index => ast::Expr::Index {
                            source,
                            index: Box::new(CUEParser::Index(item)?),
                        },
                        Rule::Slice => {
                            let (low, high) = CUEParser::Slice(item)?;
                            ast::Expr::Slice {
                                source,
                                low: Box::new(low),
                                high: Box::new(high),
                            }
                        },
                        Rule::Arguments => ast::Expr::Call {
                            source,
                            args: CUEParser::Arguments(item)?
                        },
                        x => unreachable!("unexpected primary expr rule {:?}", x)
                    }
                }
                
                Ok(current_expr)
            })
    }
    fn UnaryExpr(input: Node) -> Result<ast::Expr> {
        Ok(match_nodes!(input.into_children();
        [PrimaryExpr(p)] => p,
        [unary_op(op), Expression(child)] => ast::Expr::UnaryExpr {
            op,
            child: Box::new(child)
        }))
    }
    fn Expression(input: Node) -> Result<ast::Expr> {
        let infix = |lhs, op, rhs| {
            Ok(ast::Expr::BinaryExpr {
                lhs: Box::new(lhs?),
                op: binary_op(Node::new(op))?,
                rhs: Box::new(rhs?)
            })
        };
        
        PRECCLIMBER.climb(
            input.into_children().into_pairs(),
            |p| CUEParser::UnaryExpr(Node::new(p)),
            infix
        )
    }
    fn rel_op(input: Node) -> Result<ast::Operator> {
        match input.as_str() {
            "!=" => Ok(ast::Operator::NotEqual),
            "<" => Ok(ast::Operator::Less),
            "<=" => Ok(ast::Operator::LessEqual),
            ">" => Ok(ast::Operator::Greater),
            ">=" => Ok(ast::Operator::GreaterEqual),
            "=~" => Ok(ast::Operator::Match),
            "!~" => Ok(ast::Operator::NotMatch),
            x => unreachable!("rel_op {}", x),
        }
    }
    fn add_op(input: Node) -> Result<ast::Operator> {
        match input.as_str() {
            "+" => Ok(ast::Operator::Add),
            "-" => Ok(ast::Operator::Subtract),
            x => unreachable!("add_op {}", x),
        }
    }
    fn mul_op(input: Node) -> Result<ast::Operator> {
        match input.as_str() {
            "*" => Ok(ast::Operator::Multiply),
            "-" => Ok(ast::Operator::Divide),
            x => unreachable!("mul_op {}", x),
        }
    }
    fn unary_op(input: Node) -> Result<ast::Operator> {
        match input.as_rule() {
            Rule::rel_op => CUEParser::rel_op(input),
            Rule::add_op => CUEParser::add_op(input),
            Rule::unary_op => match input.as_str() {
                "!" => Ok(ast::Operator::Not),
                "*" => Ok(ast::Operator::Multiply),
                x => unreachable!("unary_op {}", x),
            },
            x => unreachable!("unary_op {:?}", x),
        }
    }
    fn Comprehension(input: Node) -> Result<ast::Comprehension> {
        Ok(match_nodes!(input.into_children();
            [Clauses(clauses), StructLit(s)] => ast::Comprehension {
                clauses, expr: ast::Expr::Struct(s)
            }))
    }
    fn Clauses(input: Node) -> Result<Vec<ast::Clause>> {
        Ok(match_nodes!(input.into_children();
            [StartClause(sc)] => vec![sc],
            [StartClause(sc), Clause(cs)..] => {
                let mut clauses = vec![sc];
                clauses.extend(cs);
                clauses
            }))
    }
    fn StartClause(input: Node) -> Result<ast::Clause> {
        Ok(match_nodes!(input.into_children();
            [ForClause(fc)] => fc,
            [GuardClause(gc)] => gc))
    }
    fn Clause(input: Node) -> Result<ast::Clause> {
        Ok(match_nodes!(input.into_children();
            [StartClause(sc)] => sc,
            [LetClause(lc)] => lc))
    }
    fn ForClause(input: Node) -> Result<ast::Clause> {
        Ok(match_nodes!(input.into_children();
            [identifier(key), identifier(value), Expression(source)] => ast::Clause::For {
                key: Some(key),
                value,
                source
            },
            [identifier(value), Expression(source)] => ast::Clause::For {
                key:None,
                value,
                source
            }))
    }
    fn GuardClause(input: Node) -> Result<ast::Clause> {
        Ok(ast::Clause::If(CUEParser::Expression(input.into_children().single()?)?))
    }
    fn LetClause(input: Node) -> Result<ast::Clause> {
        Ok(match_nodes!(input.into_children();
            [identifier(i), Expression(e)] => ast::Clause::Let(ast::LetClause {
                alias: i,
                value: e
            })))
    }
    fn PackageClause(input: Node) -> Result<ast::Ident> {
        CUEParser::PackageName(input.into_children().single()?)
    }
    fn PackageName(input: Node) -> Result<ast::Ident> {
        CUEParser::identifier(input.into_children().single()?)
    }
    fn ImportDecl(input: Node) -> Result<ast::Declaration> {
        input
        .into_children()
        .map(|i| CUEParser::ImportSpec(i))
        .try_collect()
        .map(|specs| ast::Declaration::ImportDecl(specs))
    }
    fn ImportSpec(input: Node) -> Result<ast::ImportSpec> {
        Ok(match_nodes!(input.into_children();
            [PackageName(alias), ImportPath((path, package))] => ast::ImportSpec {
                alias,
                path,
                package,
            }))
    }
    fn ImportLocation(input: Node) -> Result<ast::BasicLit> {
        Ok(ast::BasicLit::Str(input.as_str()))
    }
    fn ImportPath(input: Node) -> Result<(ast::BasicLit, Option<ast::Ident>)> {
        Ok(match_nodes!(input.into_children();
            [ImportLocation(path)] => (path, None),
            [ImportLocation(path), identifier(package)] => (path, Some(package))))
    }
    fn SourceFile(input: Node) -> Result<ast::File> {
        let children = input.into_children();
        
        Ok(ast::File {
            name: "",
            attributes:  children.clone().filter_map(|n| {
                match n.as_rule() {
                    Rule::attribute => CUEParser::attribute(n).ok(),
                    _ => None
                }
            }).collect(),
            package: children
                    .clone()
                    .find(|n| n.as_rule() == Rule::PackageClause)
                    .map(|n| CUEParser::PackageClause(n))
                    .transpose()?,
            imports: children.clone().filter_map(|n| {
                match n.as_rule() {
                    Rule::ImportDecl => CUEParser::ImportDecl(n).ok(),
                    _ => None
                }
            }).collect(),
            declarations: children.clone().filter_map(|n| {
                match n.as_rule() {
                    Rule::Declaration => CUEParser::Declaration(n).ok(),
                    _ => None
                }
            }).collect(),
        })
    }
    fn WHITESPACE(input: Node) -> Result<()> {
        Ok(())
    }
    fn COMMENT(input: Node) -> Result<()> {
        Ok(())
    }
    fn EOI(_input: Node) -> Result<()> {
        Ok(())
    }
}

#[test]
fn test_float() {
    let parse_float = |i| parse_single!(float_lit, i);
    assert_eq!(parse_float("0.0"), Ok(0.0));
    assert_eq!(parse_float("1.0"), Ok(1.0));
    assert_eq!(parse_float("-1.0"), Ok(-1.0));
    assert_eq!(parse_float("0.1"), Ok(0.1));
    assert_eq!(parse_float(".1"), Ok(0.1));
    assert_eq!(parse_float(".1e1"), Ok(0.1e1));
    assert_eq!(parse_float("1.1"), Ok(1.1));
    assert_eq!(parse_float("1.01"), Ok(1.01));
    assert_eq!(parse_float("0.01"), Ok(0.01));
    assert_eq!(parse_float("1.10"), Ok(1.10));
    assert_eq!(parse_float("3.14"), Ok(3.14));
    assert_eq!(parse_float("13e2"), Ok(13e2));
    assert_eq!(parse_float("1.3e2"), Ok(1.3e2));
    assert_eq!(parse_float("3.14e4"), Ok(3.14e4));
    assert_eq!(parse_float("0.1e-3"), Ok(0.1e-3));
    assert_eq!(parse_float(".1e-3"), Ok(0.1e-3));
    // TODO:
    // assert_eq!(parse_float("3.14e-4"), Ok(3.14e-4));
}

#[test]
fn test_int() {
    let parse_int = |i| parse_single!(int_lit, i);
    assert_eq!(parse_int("1"), Ok(1));
    assert_eq!(parse_int("0"), Ok(0));
    assert_eq!(parse_int("100_000"), Ok(100_000));
    assert_eq!(parse_int("99"), Ok(99));
    assert_eq!(parse_int("1K"), Ok(1000));
    assert_eq!(parse_int("1.2K"), Ok(1200));
    assert_eq!(parse_int("3Ki"), Ok(3 * 1024));
}

#[test]
fn test_strings() {
    let parse_str = |i| parse_single!(String, i);
    // println!(
    //     "{:#?}",
    //     CUEParser::String(
    //         CUEParser::parse(Rule::String, "\'\\100\'")
    //             .unwrap()
    //             .single()
    //             .unwrap()
    //     )
    // );
    // println!(
    //     "{:#?}",
    //     CUEParser::parse(Rule::String, "\"hello \\(1+1) world\"")
    // );
    let str = |s: &str| ast::Interpolation {
        is_bytes: false,
        elements: vec![ast::new_str(s.to_string())],
    };
    let bytes = |s: &str| ast::Interpolation {
        is_bytes: true,
        elements: vec![ast::new_str(s.to_string())],
    };

    // assert_eq!(
    //     parse_str(r###"#'\#test'#"###),
    //     Ok(ast::Interpolation {
    //         is_bytes: true,
    //         elements: vec![new_str("\test".to_string())]
    //     })
    // );
    assert_eq!(parse_str(r#""test""#), Ok(str("test")));
    assert_eq!(parse_str(r##"#""test""#"##), Ok(str(r#""test""#)));
    assert_eq!(parse_str(r"'test'"), Ok(bytes("test")));
    assert_eq!(parse_str(r##"#"test"#"##), Ok(str("test")));
    assert_eq!(parse_str(r"#''test''#"), Ok(bytes("'test'")));
    assert_eq!(
        parse_str(
            r#""""
            test
            test

            """"#
        ),
        Ok(str("test\ntest\n"))
    );
    assert_eq!(
        parse_str(
            r#"'''
            test
            test

            '''"#
        ),
        Ok(bytes("test\ntest\n"))
    );
}

#[test]
fn test_label() {
    assert_eq!(parse_single!(Label, "identifier"), Ok(ast::Label::Ident(ast::Ident { name: "identifier" })));
    assert_eq!(parse_single!(Label, "\"quoted\""), Ok(ast::Label::Basic(parse_single!(String, "\"quoted\"").unwrap())));
    assert_eq!(
        parse_single!(Label, "(parenthesis)"),
        Ok(ast::Label::Paren(ast::Expr::Ident(ast::Ident {
            name: "parenthesis"
        })))
    );
    assert_eq!(
        parse_single!(Label, "[brackets=string]"), 
        Ok(ast::Label::Bracket(ast::Expr::Alias(Box::new(ast::Alias {
            ident: ast::Ident { name: "brackets" },
            expr: ast::Expr::Ident(ast::Ident { name: "string" })
        })))));
}
#[test]
fn test_struct() {
    let field = |label, value| ast::Declaration::Field(ast::Field {
        attributes: vec![],
        label,
        value,
    });

    assert_eq!(
        parse_single!(
            StructLit,
            r#"{
    identifier: 0,
    "quoted": 1,
    (parenthesis): 2,
    [brackets=string]: 3,
}"#
        ),
        Ok(ast::StructLit {
            elements: vec![
                field(
                    parse_single!(Label, "identifier").unwrap(),
                    parse_single!(Expression, "0").unwrap()
                ),
                field(
                    parse_single!(Label, "\"quoted\"").unwrap(),
                    parse_single!(Expression, "1").unwrap(),
                ),
                field(
                    parse_single!(Label, "(parenthesis)").unwrap(),
                    parse_single!(Expression, "2").unwrap(),
                ),
                field(
                    parse_single!(Label, "[brackets=string]").unwrap(),
                    parse_single!(Expression, "3").unwrap(),
                ),
            ]
        })
    );
}

#[test]
fn test_expr() {
    assert_eq!(parse_single!(Expression, "1"), Ok(ast::Expr::BasicLit(parse_single!(BasicLit, "1").unwrap())));
    assert_eq!(parse_single!(Expression, "1 + 1"), Ok(ast::Expr::BinaryExpr {
        lhs: Box::new(parse_single!(Expression ,"1").unwrap()),
        op: ast::Operator::Add,
        rhs: Box::new(parse_single!(Expression ,"1").unwrap()),
    }));
    assert_eq!(parse_single!(Expression, "1 + 2 * 3"), Ok(ast::Expr::BinaryExpr {
        lhs: Box::new(parse_single!(Expression ,"1").unwrap()),
        op: ast::Operator::Add,
        rhs: Box::new(parse_single!(Expression ,"2 * 3").unwrap()),
    }));
    assert_eq!(parse_single!(Expression, "1 * 2 + 3"), Ok(ast::Expr::BinaryExpr {
        lhs: Box::new(parse_single!(Expression ,"1 * 2").unwrap()),
        op: ast::Operator::Add,
        rhs: Box::new(parse_single!(Expression ,"3").unwrap()),
    }));
}
