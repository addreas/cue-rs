use crate::ast;
use pest_consume::{match_nodes, Error, Parser};

#[derive(Parser)]
#[grammar = "cue.pest"]
pub struct CUEParser;

type Result<T> = std::result::Result<T, Error<Rule>>;
type Node<'i> = pest_consume::Node<'i, Rule, ()>;

// This is the other half of the parser, using pest_consume.
#[pest_consume::parser]
impl CUEParser {
    fn identifier(input: Node) -> Result<ast::Ident> {
        Ok(ast::Ident {
            name: input.as_str(),
        })
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
            [decimal_lit(a), fraction(b), multiplier(m)] => ((a as f64 + b) * m as f64) as i64,
            [decimal_lit(a), multiplier(m)] => ((a * m) as f64) as i64,
            [fraction(b), multiplier(m)] => (b * m as f64) as i64))
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
        input
            .as_str()
            .trim_start_matches(['E', 'e'])
            .parse()
            .map_err(|e| {
                input.error(format!(
                    "exponent parsing failed for {}: {}",
                    input.as_str(),
                    e
                ))
            })
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
            [decimal_lit(a), fraction(b), exponent(e)] => (a as f64 + b)  * f64::powi(10.0, e),
            [decimal_lit(a), exponent(e)] => a as f64 * f64::powi(10.0 , e),
            [_, fraction(b), exponent(e)] => b * f64::powi(10.0, e),
            [fraction(b), exponent(e)] => b * f64::powi(10.0, e),
            [decimal_lit(a), fraction(b)] => a as f64 + b,
            [fraction(b)] => b,
        ))
    }
    fn escaped_char(input: Node) -> Result<char> {
        input
            .as_str()
            .chars()
            .last()
            .map_or(Err(input.error("no last char in escape sequence")), |c| {
                Ok(c)
            })
    }
    fn octal_byte_value(input: Node) -> Result<()> {
        Ok(())
    }
    fn hex_byte_value(input: Node) -> Result<()> {
        Ok(())
    }
    fn little_u_value(input: Node) -> Result<()> {
        Ok(())
    }
    fn big_u_value(input: Node) -> Result<()> {
        Ok(())
    }
    fn Interpolation(input: Node) -> Result<()> {
        Ok(())
    }
    fn unicode_value(input: Node) -> Result<()> {
        Ok(())
    }
    fn byte_value(input: Node) -> Result<()> {
        Ok(())
    }
    fn String(input: Node) -> Result<()> {
        Ok(())
    }
    fn SimpleString(input: Node) -> Result<()> {
        Ok(())
    }
    fn SimpleBytes(input: Node) -> Result<()> {
        Ok(())
    }
    fn MultilineString(input: Node) -> Result<()> {
        Ok(())
    }
    fn MultilineBytes(input: Node) -> Result<()> {
        Ok(())
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
    fn Expression(input: Node) -> Result<()> {
        Ok(())
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
    assert_eq!(parse_float("1.0"), Ok(1.0));
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
