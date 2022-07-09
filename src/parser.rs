use std::vec;

use crate::ast::{self, new_str};
use pest_consume::{match_nodes, Error, Parser};

#[derive(Parser)]
#[grammar = "cue.pest"]
pub struct CUEParser;

type Result<T> = std::result::Result<T, Error<Rule>>;
type Node<'i> = pest_consume::Node<'i, Rule, ()>;

fn interpolation_elements(input: Node) -> Result<Vec<ast::Expr>> {
    let mut current_str = String::new();
    let mut iter = input.clone().into_children().peekable();
    let mut res = vec![];
    loop {
        // println!(
        //     "interpolation_elements: {:?}",
        //     input
        //         .clone()
        //         .into_children()
        //         .peekable()
        //         .next_if(|n| n.as_rule() != Rule::Interpolation)
        // );
        if let Some(n) = iter.next_if(|n| n.as_rule() != Rule::Interpolation) {
            let ch = match n.as_rule() {
                Rule::escaped_char => CUEParser::escaped_char(n),
                Rule::octal_byte_value => CUEParser::octal_byte_value(n),
                Rule::hex_byte_value => CUEParser::hex_byte_value(n),
                Rule::little_u_value => CUEParser::little_u_value(n),
                Rule::big_u_value => CUEParser::big_u_value(n),
                Rule::unicode_value => CUEParser::unicode_value(n),
                Rule::byte_value => CUEParser::byte_value(n),
                x => Err(n.error(format!("interpolation_elements: unknown node type {:?}", x))),
            };
            // println!("im here with {:?}!", ch);

            current_str.push(ch?)
        } else if let Some(n) = iter.next() {
            res.push(ast::new_str(current_str.clone()));
            res.push(CUEParser::Interpolation(n)?);
            current_str.clear();
        } else {
            res.push(ast::new_str(current_str));
            break;
        }
    }
    return Ok(res);
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
// This is the other half of the parser, using pest_consume.
#[pest_consume::parser]
#[allow(dead_code)]
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
        println!("escaped_char: {:?}", input.as_str());
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
        println!("octal_byte_value: {:?}", input.as_str());
        parse_digits_radix(input, 8)
    }
    fn hex_byte_value(input: Node) -> Result<char> {
        println!("hex_byte_value: {:?}", input.as_str());
        parse_digits_radix(input, 16)
    }
    fn little_u_value(input: Node) -> Result<char> {
        println!("littel_u_value: {:?}", input.as_str());
        parse_digits_radix(input, 16)
    }
    fn big_u_value(input: Node) -> Result<char> {
        println!("big_u_value: {:?}", input.as_str());
        parse_digits_radix(input, 16)
    }
    fn unicode_value(input: Node) -> Result<char> {
        println!("unicode_value: {:?}", input.as_str());
        Ok(match_nodes!(input.clone().into_children();
            [little_u_value(c)] => c,
            [big_u_value(c)] => c,
            [escaped_char(c)] => c,
            [unicode_char(c)] => c,
        ))
    }
    fn byte_value(input: Node) -> Result<char> {
        println!("byte_value: {:?}", input.as_str());
        Ok(match_nodes!(input.into_children();
            [octal_byte_value(c)] => c,
            [hex_byte_value(c)] => c,
        ))
    }
    fn Interpolation(input: Node) -> Result<ast::Expr> {
        println!("Iterpolation: {:?}", input.as_str());
        CUEParser::Expression(input.into_children().single()?)
    }
    fn String(input: Node) -> Result<ast::Interpolation> {
        println!("String: {:?}", input.as_str());
        Ok(match_nodes!(input.into_children();
            [SimpleString(s)] => s,
            [MultilineString(s)] => s,
            [SimpleBytes(s)] => s,
            [MultilineBytes(s)] => s))
    }
    fn SimpleString(input: Node) -> Result<ast::Interpolation> {
        Ok(ast::Interpolation {
            is_bytes: false,
            elements: interpolation_elements(input)?,
        })
    }
    fn SimpleBytes(input: Node) -> Result<ast::Interpolation> {
        println!("{}", input);
        Ok(ast::Interpolation {
            is_bytes: true,
            elements: interpolation_elements(input)?,
        })
    }
    fn MultilineString(input: Node) -> Result<ast::Interpolation> {
        Ok(ast::Interpolation {
            is_bytes: false,
            elements: interpolation_elements(input)?,
        })
    }
    fn MultilineBytes(input: Node) -> Result<ast::Interpolation> {
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
    fn bool_lit(input: Node) -> Result<()> {
        Ok(())
    }
    fn StructLit(input: Node) -> Result<()> {
        Ok(())
    }
    fn Declaration(input: Node) -> Result<ast::Declaration> {
        Ok(ast::Declaration::BadDecl)
    }
    fn Ellipsis(input: Node) -> Result<()> {
        Ok(())
    }
    fn Embedding(input: Node) -> Result<()> {
        Ok(())
    }
    fn Field(input: Node) -> Result<()> {
        Ok(())
    }
    fn Label(input: Node) -> Result<()> {
        Ok(())
    }
    fn LabelExpr(input: Node) -> Result<()> {
        Ok(())
    }
    fn LabelName(input: Node) -> Result<()> {
        Ok(())
    }
    fn attribute(input: Node) -> Result<()> {
        Ok(())
    }
    fn attr_tokens(input: Node) -> Result<()> {
        Ok(())
    }
    fn attr_token(input: Node) -> Result<()> {
        Ok(())
    }
    fn AliasExpr(input: Node) -> Result<()> {
        Ok(())
    }
    fn ListLit(input: Node) -> Result<()> {
        Ok(())
    }
    fn ElementList(input: Node) -> Result<()> {
        Ok(())
    }
    fn Literal(input: Node) -> Result<()> {
        Ok(())
    }
    fn Operand(input: Node) -> Result<()> {
        Ok(())
    }
    fn BasicLit(input: Node) -> Result<()> {
        Ok(())
    }
    fn OperandName(input: Node) -> Result<()> {
        Ok(())
    }
    fn QualifiedIdent(input: Node) -> Result<()> {
        Ok(())
    }
    fn Selector(input: Node) -> Result<()> {
        Ok(())
    }
    fn Index(input: Node) -> Result<()> {
        Ok(())
    }
    fn Slice(input: Node) -> Result<()> {
        Ok(())
    }
    fn Argument(input: Node) -> Result<()> {
        Ok(())
    }
    fn Arguments(input: Node) -> Result<()> {
        Ok(())
    }
    fn PrimaryExpr(input: Node) -> Result<()> {
        Ok(())
    }
    fn UnaryExpr(input: Node) -> Result<()> {
        Ok(())
    }
    fn Expression(input: Node) -> Result<ast::Expr> {
        Ok(ast::Expr::Bad)
    }
    fn binary_op(input: Node) -> Result<()> {
        Ok(())
    }
    fn add_op(input: Node) -> Result<()> {
        Ok(())
    }
    fn mul_op(input: Node) -> Result<()> {
        Ok(())
    }
    fn unary_op(input: Node) -> Result<()> {
        Ok(())
    }
    fn Comprehension(input: Node) -> Result<()> {
        Ok(())
    }
    fn Clauses(input: Node) -> Result<()> {
        Ok(())
    }
    fn StartClause(input: Node) -> Result<()> {
        Ok(())
    }
    fn Clause(input: Node) -> Result<()> {
        Ok(())
    }
    fn ForClause(input: Node) -> Result<()> {
        Ok(())
    }
    fn GuardClause(input: Node) -> Result<()> {
        Ok(())
    }
    fn LetClause(input: Node) -> Result<()> {
        Ok(())
    }
    fn PackageClause(input: Node) -> Result<()> {
        Ok(())
    }
    fn PackageName(input: Node) -> Result<()> {
        Ok(())
    }
    fn ImportDecl(input: Node) -> Result<()> {
        Ok(())
    }
    fn ImportSpec(input: Node) -> Result<()> {
        Ok(())
    }
    fn ImportLocation(input: Node) -> Result<()> {
        Ok(())
    }
    fn ImportPath(input: Node) -> Result<()> {
        Ok(())
    }
    fn SourceFile(input: Node) -> Result<()> {
        Ok(())
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
    let parse_float = |str| CUEParser::float_lit(CUEParser::parse(Rule::float_lit, str)?.single()?);
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
    let parse_int = |str| CUEParser::int_lit(CUEParser::parse(Rule::int_lit, str)?.single()?);
    assert_eq!(parse_int("1"), Ok(1));
    assert_eq!(parse_int("0"), Ok(0));
    assert_eq!(parse_int("100_000"), Ok(100_000));
    assert_eq!(parse_int("99"), Ok(99));
    assert_eq!(parse_int("1K"), Ok(1000));
    assert_eq!(parse_int("1.2K"), Ok(1200));
    assert_eq!(parse_int("3Ki"), Ok(3 * 1024));
}

#[test]
fn strings() {
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
    let parse_str = |str| {
        let parsed = CUEParser::parse(Rule::String, str)?.single()?;
        println!("{:#?}", parsed);
        CUEParser::String(parsed)
    };
    assert_eq!(
        parse_str(r###"#'\#test'#"###),
        Ok(ast::Interpolation {
            is_bytes: true,
            elements: vec![new_str("\test".to_string())]
        })
    );
}
