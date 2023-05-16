use pest::{
    Parser,
    iterators::Pair,
    pratt_parser::{Assoc, Op, PrattParser},
};
use std::result::Result;
use std::vec;

use crate::ast;

#[derive(Parser)]
#[grammar = "./ast/cue.pest"]
pub struct CUEParser;

lazy_static::lazy_static! {
    static ref  PRATT: PrattParser<Rule> =
    PrattParser::new()
        .op(Op::infix(Rule::disjunct_op, Assoc::Left))
        .op(Op::infix(Rule::conjuct_op, Assoc::Left))
        .op(Op::infix(Rule::or_op, Assoc::Left))
        .op(Op::infix(Rule::and_op, Assoc::Left))
        .op(Op::infix(Rule::rel_op, Assoc::Left) | Op::infix(Rule::equal_op, Assoc::Left))
        .op(Op::infix(Rule::add_op, Assoc::Left))
        .op(Op::infix(Rule::mul_op, Assoc::Left))
        .op(Op::postfix(Rule::Selector) | Op::postfix(Rule::Index) | Op::postfix(Rule::Slice) | Op::postfix(Rule::Arguments))
        .op(Op::prefix(Rule::not_op) | Op::prefix(Rule::default_op) | Op::prefix(Rule::rel_op) | Op::prefix(Rule::add_op));
}

type Error = String;


macro_rules! match_pair {
    (
        $expression:expr, {
            $( $rule:ident($binding:pat) => $body:expr ),*
            $(,)?
        }
    )
    =>
    {
        {
            match $expression.as_rule() {
                $(
                    Rule::$rule  => {
                        let $binding = CUEParser::$rule($expression);

                        $body
                    }
                ),*
                _ => unreachable!("rule unreachable: {:?}", $expression),
            }
        }
    }
}

macro_rules! match_pairs {
    (
        $expression:expr, {
            $( [ $($rule:ident($binding:pat)),* ] => $body:expr ),*
            $(,)?
        }
    )
    =>
    {
        {
            let val = $expression;
            let mut mutval = val.clone();
            match val.map(|p| p.as_rule()).collect::<Vec<_>>()[..] {
                $(
                    [ $( Rule::$rule ),* ] => {
                        $( let $binding = CUEParser::$rule(mutval.next().unwrap()); )*

                        $body
                    }
                ),*
                _ => unreachable!("rule unreachable: {:?}", mutval),
            }
        }
    };
}

#[test]
fn test_pairs() {
    let pairs = CUEParser::parse(Rule::si_lit, "0.5Ti")
        .unwrap()
        .next()
        .unwrap()
        .into_inner();
    let res = match_pairs!(pairs, {
        [decimal_lit(a), fraction(b), multiplier(m)] => (a, b, m),
        [decimal_lit(a),              multiplier(m)] => (a, Ok(1.0), m),
        [                fraction(b), multiplier(m)] => (Ok(0), b, m),
    });
    assert_eq!(res, (Ok(0), Ok(0.5), 1099511627776))
}

impl CUEParser {
    fn identifier(input: Pair<Rule>) -> ast::Ident {
        ast::Ident {
            name: input.as_str().to_string(),
        }
    }
    fn unicode_char(input: Pair<Rule>) -> Result<char, Error> {
        input
            .as_str()
            .chars()
            .next()
            .map_or(Err("no first char in str".to_string()), |c| Ok(c))
    }
    fn decimal_lit(input: Pair<Rule>) -> Result<i64, Error> {
        input
            .as_str()
            .replace("_", "")
            .as_str()
            .parse()
            .map_err(|e| format!("decimal_lit: {:?}: {}", input, e))
    }
    fn fraction(input: Pair<Rule>) -> Result<f64, Error> {
        ("0.".to_owned() + input.as_str().replace("_", "").as_str())
            .parse()
            .map_err(|e| format!("fraction: {:?}: {}", input, e))
    }
    fn si_lit(input: Pair<Rule>) -> Result<i64, Error> {
        Ok(match_pairs!(input.into_inner(), {
            [decimal_lit(a), fraction(b), multiplier(m)] => ((a? as f64 + b?) * m as f64) as i64,
            [decimal_lit(a),              multiplier(m)] => a? * m,
            [                fraction(b), multiplier(m)] => (b? * m as f64) as i64,
        }))
    }
    fn binary_lit(input: Pair<Rule>) -> Result<i64, Error> {
        i64::from_str_radix(input.as_str(), 2)
            .map_err(|e| format!("binary_lit: {:?}: {}", input, e))
    }
    fn octal_lit(input: Pair<Rule>) -> Result<i64, Error> {
        i64::from_str_radix(input.as_str(), 8).map_err(|e| format!("octal_lit: {:?}: {}", input, e))
    }
    fn hex_lit(input: Pair<Rule>) -> Result<i64, Error> {
        i64::from_str_radix(input.as_str(), 16).map_err(|e| format!("hex_lit: {:?}: {}", input, e))
    }
    fn multiplier(input: Pair<Rule>) -> i64 {
        match input.as_str() {
            "K" => i64::pow(10, 3),
            "M" => i64::pow(10, 6),
            "G" => i64::pow(10, 9),
            "T" => i64::pow(10, 12),
            "P" => i64::pow(10, 15),
            "Ki" => i64::pow(2, 10),
            "Mi" => i64::pow(2, 20),
            "Gi" => i64::pow(2, 30),
            "Ti" => i64::pow(2, 40),
            "Pi" => i64::pow(2, 50),
            _ => unreachable!("unknown multiplier: {}", input.as_str()),
        }
    }
    fn exponent(input: Pair<Rule>) -> Result<i32, Error> {
        match_pairs!(input.into_inner(), {
            [decimal_lit(n)] => n.map(|n| n as i32)
        })
    }
    fn int_lit(input: Pair<Rule>) -> Result<i64, Error> {
        match_pairs!(input.into_inner(), {
            [si_lit(n)] => n,
            [decimal_lit(n)] => n,
            [binary_lit(n)] => n,
            [octal_lit(n)] => n,
            [hex_lit(n)] => n,
        })
    }
    fn float_lit(input: Pair<Rule>) -> Result<f64, Error> {
        Ok(match_pairs!(input.into_inner(), {
            [decimal_lit(a), fraction(b), exponent(e)] => (a? as f64 + b?) * f64::powi(10.0, e?),
            [decimal_lit(a), fraction(b)             ] =>  a? as f64 + b?,
            [decimal_lit(a),              exponent(e)] =>  a? as f64      * f64::powi(10.0, e?),
            [                fraction(b), exponent(e)] =>             b?  * f64::powi(10.0, e?),
            [                fraction(b)             ] =>             b?,
        }))
    }
    fn escaped_char(input: Pair<Rule>) -> Result<char, Error> {
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
            x => Err(format!(
                "escaped_char: unknown escape char {:?}: {:?}",
                x, input
            )),
        }
    }
    fn octal_byte_value(input: Pair<Rule>) -> Result<char, Error> {
        // println!("octal_byte_value: {:?}", input.as_str());
        parse_digits_radix(input, 8)
    }
    fn hex_byte_value(input: Pair<Rule>) -> Result<char, Error> {
        // println!("hex_byte_value: {:?}", input.as_str());
        parse_digits_radix(input, 16)
    }
    fn little_u_value(input: Pair<Rule>) -> Result<char, Error> {
        // println!("littel_u_value: {:?}", input.as_str());
        parse_digits_radix(input, 16)
    }
    fn big_u_value(input: Pair<Rule>) -> Result<char, Error> {
        // println!("big_u_value: {:?}", input.as_str());
        parse_digits_radix(input, 16)
    }
    fn unicode_value(input: Pair<Rule>) -> Result<char, Error> {
        // println!("unicode_value: {:?}", input.as_str());
        match_pairs!(input.into_inner(), {
            [little_u_value(c)] => c,
            [big_u_value(c)] => c,
            [escaped_char(c)] => c,
            [unicode_char(c)] => c,
        })
    }
    fn byte_value(input: Pair<Rule>) -> Result<char, Error> {
        // println!("byte_value: {:?}", input.as_str());
        match_pairs!(input.into_inner(), {
            [octal_byte_value(c)] => c,
            [hex_byte_value(c)] => c,
        })
    }
    fn Interpolation(input: Pair<Rule>) -> Result<ast::Expr, Error> {
        // println!("Iterpolation: {:?}", input.as_str());
        // TODO?
        CUEParser::Expression(input) // todo: into inner?
    }
    fn String(input: Pair<Rule>) -> Result<ast::Interpolation, Error> {
        // println!("String: {:?}", input.as_str());
        match_pairs!(input.into_inner(), {
            [SimpleString(s)] => s,
            [MultilineString(s)] => s,
            [SimpleBytes(s)] => s,
            [MultilineBytes(s)] => s
        })
    }
    fn SimpleString(input: Pair<Rule>) -> Result<ast::Interpolation, Error> {
        // println!("SimpleString: {:?}", input);
        Ok(ast::Interpolation {
            is_bytes: false,
            elements: interpolation_elements(input)?,
        })
    }
    fn SimpleBytes(input: Pair<Rule>) -> Result<ast::Interpolation, Error> {
        // println!("SimpleBytes: {:?}", input);
        Ok(ast::Interpolation {
            is_bytes: true,
            elements: interpolation_elements(input)?,
        })
    }
    fn MultilineString(input: Pair<Rule>) -> Result<ast::Interpolation, Error> {
        // println!("MultilineString: {:?}", input);
        Ok(ast::Interpolation {
            is_bytes: false,
            elements: interpolation_elements(input)?,
        })
    }
    fn MultilineBytes(input: Pair<Rule>) -> Result<ast::Interpolation, Error> {
        // println!("MultilineBytes: {:?}", input);
        Ok(ast::Interpolation {
            is_bytes: true,
            elements: interpolation_elements(input)?,
        })
    }
    fn bottom_lit(_: Pair<Rule>) -> ast::BasicLit {
        ast::BasicLit::Bottom
    }
    fn null_lit(_: Pair<Rule>) -> ast::BasicLit {
        ast::BasicLit::Null
    }
    fn bool_lit(input: Pair<Rule>) -> Result<bool, Error> {
        match input.as_str() {
            "true" => Ok(true),
            "false" => Ok(false),
            _ => unreachable!(),
        }
    }
    fn StructLit(input: Pair<Rule>) -> Result<ast::StructLit, Error> {
        Ok(ast::StructLit {
            elements: input
                .into_inner()
                .map(|c| CUEParser::Declaration(c))
                .try_collect()?,
        })
    }
    fn Declaration(input: Pair<Rule>) -> Result<ast::Declaration, Error> {
        Ok(match_pairs!(input.into_inner(), {
            [Field(f)] => ast::Declaration::Field(f?),
            [Ellipsis(e)] => ast::Declaration::Ellipsis(e?),
            [Embedding(e)] => ast::Declaration::Embedding(e?),
            [attribute(e)] => ast::Declaration::Attribute(e?),
        }))
    }
    fn Ellipsis(input: Pair<Rule>) -> Result<ast::Ellipsis, Error> {
        Ok(ast::Ellipsis {
            inner: CUEParser::Expression(input)?, // todo: into inner?
        })
    }
    fn Embedding(input: Pair<Rule>) -> Result<ast::Expr, Error> {
        Ok(match_pairs!(input.into_inner(), {
            [Comprehension(c)] => ast::Expr::Comprehension(Box::new(c?)),
            [AliasExpr(a)] => a?,
        }))
    }
    fn Field(input: Pair<Rule>) -> Result<ast::Field, Error> {
        // todo: rewrite?
        let mut inner = input.into_inner();
        let mut ls = CUEParser::Labels(inner.next().unwrap())?;
        let value = CUEParser::AliasExpr(inner.next().unwrap())?;

        let attributes: Vec<_> = inner.map(|p| CUEParser::attribute(p).unwrap()).collect();

        let mut lsi = ls.iter_mut();
        let init = ast::Field {
            label: lsi.next().expect("nonempty list").clone(),
            value: value,
            attributes: {
                match attributes.len() {
                    0 => None,
                    _ => Some(attributes),
                }
            },
        };

        Ok(lsi.rfold(init, |acc, label| ast::Field {
            label: label.clone(),
            value: ast::Expr::Struct(ast::StructLit {
                elements: vec![ast::Declaration::Field(acc)],
            }),
            attributes: None,
        }))
    }
    fn Labels(input: Pair<Rule>) -> Result<Vec<ast::Label>, Error> {
        input.into_inner().map(|p| CUEParser::Label(p)).collect()
    }
    fn Label(input: Pair<Rule>) -> Result<ast::Label, Error> {
        Ok(match_pairs!(input.into_inner(), {
            [LabelName(n)] => n?,
            [Expression(e)] => ast::Label::Paren(e?),
            [AliasExpr(a)] => ast::Label::Bracket(a?),
        }))
    }
    fn LabelName(input: Pair<Rule>) -> Result<ast::Label, Error> {
        Ok(match_pairs!(input.into_inner(), {
            [identifier(i)] => ast::Label::Ident(i),
            [SimpleString(s)] => ast::Label::Basic(s?)
        }))
    }
    fn attribute(input: Pair<Rule>) -> Result<ast::Attribute, Error> {
        Ok(ast::Attribute {
            text: input.as_str().to_string(),
        })
    }
    fn AliasExpr(input: Pair<Rule>) -> Result<ast::Expr, Error> {
        Ok(match_pairs!(input.into_inner(), {
                [identifier(ident), Expression(expr)] => ast::Expr::alias(ident, expr?),
                [Expression(expr)] => expr?
        }))
    }
    fn ListLit(input: Pair<Rule>) -> Result<ast::ListLit, Error> {
        Ok(ast::ListLit {
            elements: CUEParser::ElementList(input)?, // todo: into inner
        })
    }
    fn ElementList(input: Pair<Rule>) -> Result<Vec<ast::Expr>, Error> {
        input.into_inner().map(|p| {
            Ok(match_pair!(p, {
                Ellipsis(e) => ast::Expr::Ellipsis(Box::new(e?)),
                Embedding(e) => e?,
            }))
        }).collect()
    }
    fn Literal(input: Pair<Rule>) -> Result<ast::Expr, Error> {
        Ok(match_pairs!(input.into_inner(), {
            [BasicLit(b)] => ast::Expr::BasicLit(b?),
            [ListLit(l)] => ast::Expr::List(l?),
            [StructLit(s)] => ast::Expr::Struct(s?)
        }))
    }
    fn Operand(input: Pair<Rule>) -> Result<ast::Expr, Error> {
        Ok(match_pairs!(input.into_inner(), {
            [Literal(l)] => l?,
            [OperandName(o)] => o?,
            [Expression(e)] => ast::Expr::parens(e?)
        }))
    }
    fn BasicLit(input: Pair<Rule>) -> Result<ast::BasicLit, Error> {
        Ok(match_pairs!(input.into_inner(), {
            [int_lit(l)] => ast::BasicLit::Int(l?),
            [float_lit(l)] => ast::BasicLit::Float(l?),
            [bool_lit(l)] => ast::BasicLit::Bool(l?),
            [null_lit(l)] => l,
            [bottom_lit(l)] => l,
        }))
    }
    fn OperandName(input: Pair<Rule>) -> Result<ast::Expr, Error> {
        Ok(match_pairs!(input.into_inner(), {
            [identifier(i)] => ast::Expr::Ident(i),
            [QualifiedIdent(qi)] => qi?
        }))
    }
    fn QualifiedIdent(input: Pair<Rule>) -> Result<ast::Expr, Error> {
        Ok(match_pairs!(input.into_inner(), {
            [PackageName(p), identifier(i)] => ast::Expr::QualifiedIdent(p?, i)
        }))
    }
    fn Selector(input: Pair<Rule>) -> Result<ast::Label, Error> {
        Ok(match_pairs!(input.into_inner(), {
            [identifier(i)] => ast::Label::Ident(i),
            [SimpleString(s)] => ast::Label::Basic(s?)
        }))
    }
    fn Index(input: Pair<Rule>) -> Result<ast::Expr, Error> {
        CUEParser::Expression(input) // todo: into_inner?
    }
    fn Slice(input: Pair<Rule>) -> Result<(ast::Expr, ast::Expr), Error> {
        Ok(match_pairs!(input.into_inner(), {
            [Expression(low), Expression(high)] => (low?, high?)
        }))
    }
    fn Argument(input: Pair<Rule>) -> Result<ast::Expr, Error> {
        CUEParser::Expression(input) // todo: into inner?
    }
    fn Arguments(input: Pair<Rule>) -> Result<Vec<ast::Expr>, Error> {
        input.into_inner().map(|p| CUEParser::Argument(p)).collect()
    }
    fn Expression(input: Pair<Rule>) -> Result<ast::Expr, Error> {
        PRATT
            .map_primary(|primary| match primary.as_rule() {
                Rule::Literal => CUEParser::Literal(primary),
                Rule::OperandName => CUEParser::OperandName(primary),
                Rule::Expression => CUEParser::Expression(primary),
                _ => unreachable!("unknown primary rule"),
            })
            .map_prefix(|prefix, rhs| {
                let op = match prefix.as_rule() {
                    Rule::rel_op => rel_op(prefix)?,
                    Rule::add_op => add_op(prefix)?,
                    Rule::not_op => ast::Operator::Not,
                    Rule::default_op => ast::Operator::Default,
                    _ => unreachable!("unknown prefix rule"),
                };
                return Ok(ast::Expr::UnaryExpr(ast::UnaryExpr {
                    op: op,
                    child: Box::new(rhs?),
                }));
            })
            .map_infix(|lhs, op, rhs| Ok(ast::Expr::binary_expr(rhs?, binary_op(op)?, lhs?)))
            .map_postfix(|lhs, op| {
                Ok(match_pairs!(op.into_inner(), {
                    [Selector(sel)] => ast::Expr::selector(lhs?, sel?),
                    [Index(i)] => ast::Expr::index(lhs?, i?),
                    [Slice(s)] => {
                        let (lo, hi) = s?;
                        ast::Expr::slice(lhs?, lo, hi)
                    },
                    [Arguments(args)] => ast::Expr::call(lhs?, args?),
                }))
            })
            .parse(input.into_inner())
    }

    fn Comprehension(input: Pair<Rule>) -> Result<ast::Comprehension, Error> {
        Ok(match_pairs!(input.into_inner(), {
            [Clauses(clauses), StructLit(s)] => ast::Comprehension {
                clauses: clauses?,
                expr: ast::Expr::Struct(s?),
            }
        }))
    }
    fn Clauses(input: Pair<Rule>) -> Result<Vec<ast::Clause>, Error> {
        input.into_inner().map(|p| match_pair!(p, {
            StartClause(c) => c,
            Clause(c) => c,
        })).collect()
    }
    fn StartClause(input: Pair<Rule>) -> Result<ast::Clause, Error> {
        Ok(match_pairs!(input.into_inner(), {
            [ForClause(fc)] => fc?,
            [GuardClause(gc)] => gc?
        }))
    }
    fn Clause(input: Pair<Rule>) -> Result<ast::Clause, Error> {
        Ok(match_pairs!(input.into_inner(), {
            [StartClause(sc)] => sc?,
            [LetClause(lc)] => lc?
        }))
    }
    fn ForClause(input: Pair<Rule>) -> Result<ast::Clause, Error> {
        Ok(match_pairs!(input.into_inner(), {
            [identifier(key), identifier(value), Expression(source)] => ast::Clause::For {
                key: Some(key),
                value,
                source: source?
            },
            [identifier(value), Expression(source)] => ast::Clause::For {
                key:None,
                value,
                source: source?
            }
        }))
    }
    fn GuardClause(input: Pair<Rule>) -> Result<ast::Clause, Error> {
        Ok(ast::Clause::If(CUEParser::Expression(
            input, // todo: into inner
        )?))
    }
    fn LetClause(input: Pair<Rule>) -> Result<ast::Clause, Error> {
        Ok(match_pairs!(input.into_inner(), {
            [identifier(i), Expression(e)] => ast::Clause::Let(ast::LetClause { alias: i, value: e? })
        }))
    }
    fn PackageClause(input: Pair<Rule>) -> Result<ast::Ident, Error> {
        CUEParser::PackageName(input) // todo: into inner
    }
    fn PackageName(input: Pair<Rule>) -> Result<ast::Ident, Error> {
        Ok(CUEParser::identifier(input)) // todo: into inner
    }
    fn ImportDecl(input: Pair<Rule>) -> Result<ast::Declaration, Error> {
        input
            .into_inner()
            .map(|i| CUEParser::ImportSpec(i))
            .try_collect()
            .map(|specs| ast::Declaration::ImportDecl(specs))
    }
    fn ImportSpec(input: Pair<Rule>) -> Result<ast::ImportSpec, Error> {
        Ok(match_pairs!(input.into_inner(), {
            [PackageName(alias), ImportPath(path)] => ast::ImportSpec {
                alias: alias?,
                path: path.0,
                package: path.1,
            }
        }))
    }
    fn ImportLocation(input: Pair<Rule>) -> ast::BasicLit {
        ast::BasicLit::String(input.as_str().to_string())
    }
    fn ImportPath(input: Pair<Rule>) -> (ast::BasicLit, Option<ast::Ident>) {
        match_pairs!(input.into_inner(), {
            [ImportLocation(path)] => (path, None),
            [ImportLocation(path), identifier(package)] => (path, Some(package))
        })
    }
    fn SourceFile(input: Pair<Rule>) -> Result<ast::SourceFile, Error> {
        let children = input.into_inner();

        Ok(ast::SourceFile {
            attributes: children
                .clone()
                .filter_map(|n| match n.as_rule() {
                    Rule::attribute => CUEParser::attribute(n).ok(),
                    _ => None,
                })
                .collect(),
            package: children
                .clone()
                .find(|n| n.as_rule() == Rule::PackageClause)
                .map(|n| CUEParser::PackageClause(n))
                .transpose()?,
            imports: children
                .clone()
                .filter_map(|n| match n.as_rule() {
                    Rule::ImportDecl => CUEParser::ImportDecl(n).ok(),
                    _ => None,
                })
                .collect(),
            declarations: children
                .clone()
                .filter_map(|n| match n.as_rule() {
                    Rule::Declaration => CUEParser::Declaration(n).ok(),
                    _ => None,
                })
                .collect(),
        })
    }
}

fn interpolation_elements(input: Pair<Rule>) -> Result<Vec<ast::Expr>, Error> {
    let mut current_str = String::new();
    let mut string_parts = vec![];
    let mut expr_parts = vec![];

    for n in input.clone().into_inner() {
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

fn parse_digits_radix(input: Pair<Rule>, radix: u32) -> Result<char, String> {
    let escape = input.clone().into_inner().as_str();
    let digits = input.as_str().trim_start_matches(escape);
    match u32::from_str_radix(digits, radix) {
        Ok(char_value) => match char::from_u32(char_value) {
            Some(c) => Ok(c),
            None => Err("parse_digits_radix: invalid char".to_string()),
        },
        _ => Err("parse_digits_radix: invalid digits".to_string()),
    }
}

fn rel_op(input: Pair<Rule>) -> Result<ast::Operator, Error> {
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

fn add_op(input: Pair<Rule>) -> Result<ast::Operator, Error> {
    match input.as_str() {
        "+" => Ok(ast::Operator::Add),
        "-" => Ok(ast::Operator::Subtract),
        x => unreachable!("add_op {}", x),
    }
}

fn mul_op(input: Pair<Rule>) -> Result<ast::Operator, Error> {
    match input.as_str() {
        "*" => Ok(ast::Operator::Multiply),
        "/" => Ok(ast::Operator::Divide),
        x => unreachable!("mul_op {}", x),
    }
}

fn binary_op(input: Pair<Rule>) -> Result<ast::Operator, Error> {
    match input.as_rule() {
        Rule::rel_op => rel_op(input),
        Rule::add_op => add_op(input),
        Rule::mul_op => mul_op(input),
        Rule::disjunct_op => Ok(ast::Operator::Disjunct),
        Rule::conjuct_op => Ok(ast::Operator::Conjunct),
        Rule::or_op => Ok(ast::Operator::Or),
        Rule::and_op => Ok(ast::Operator::And),
        Rule::equal_op => Ok(ast::Operator::Equal),
        x => unreachable!("binary_op {:?}", x),
    }
}

#[allow(unused_macros)]
macro_rules! parse_single {
    ($rule:ident, $input:expr) => {
        CUEParser::$rule(
            CUEParser::parse(Rule::$rule, $input)
                .unwrap()
                .next()
                .unwrap(),
        )
    };
}

#[test]
fn test_float() {
    let parse_float = |i: &str| parse_single!(float_lit, i);
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
    assert_eq!(
        parse_single!(Label, "identifier"),
        Ok(ast::Label::Ident(ast::Ident {
            name: "identifier".to_string()
        }))
    );
    assert_eq!(
        parse_single!(Label, "\"quoted\""),
        Ok(ast::Label::Basic(
            parse_single!(String, "\"quoted\"").unwrap()
        ))
    );
    assert_eq!(
        parse_single!(Label, "(parenthesis)"),
        Ok(ast::Label::Paren(ast::Expr::Ident(ast::Ident {
            name: "parenthesis".to_string()
        })))
    );
    assert_eq!(
        parse_single!(Label, "[brackets=string]"),
        Ok(ast::Label::bracket(ast::Expr::alias(
            ast::Ident::from("brackets"),
            ast::Expr::ident("string".to_string())
        ))),
    );
}
#[test]
fn test_struct() {
    let field = |label, value| {
        ast::Declaration::Field(ast::Field {
            attributes: None,
            label,
            value,
        })
    };

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
    assert_eq!(
        parse_single!(Expression, "1"),
        Ok(ast::Expr::BasicLit(parse_single!(BasicLit, "1").unwrap()))
    );
    assert_eq!(
        parse_single!(Expression, "1 + 1"),
        Ok(ast::Expr::binary_expr(
            parse_single!(Expression, "1").unwrap(),
            ast::Operator::Add,
            parse_single!(Expression, "1").unwrap()
        ))
    );
    assert_eq!(
        parse_single!(Expression, "1 + 2 * 3"),
        Ok(ast::Expr::binary_expr(
            parse_single!(Expression, "1").unwrap(),
            ast::Operator::Add,
            parse_single!(Expression, "2 * 3").unwrap(),
        ))
    );
    assert_eq!(
        parse_single!(Expression, "1 * 2 + 3"),
        Ok(ast::Expr::binary_expr(
            parse_single!(Expression, "1 * 2").unwrap(),
            ast::Operator::Add,
            parse_single!(Expression, "3").unwrap(),
        ))
    );
}
