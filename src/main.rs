#![feature(iter_intersperse)]
#![feature(iterator_try_collect)]
pub mod ast;
pub mod parser;

extern crate pest;
#[allow(unused_imports)]
#[macro_use]
extern crate pest_derive;

fn main() {
    // let expressions = vec![
    //     "-273.15",
    //     "-273.15e-3",
    //     "some.thing[0]",
    //     "a + b",
    //     "(1 + 1) * (5 - 3)",
    //     "1 + 1 + 1",
    //     "[1, 2, 3]",
    //     "~ expressions cant start with a ~",
    // ];
    // for e in expressions {
    //     let res = CUEParser::parse(Rule::Expression, e);
    //     println!("{}: {:?}", e, res);

    //     if let Ok(pairs) = res {
    //         for pair in pairs {
    //             // A pair is a combination of the rule which matched and a span of input
    //             println!("Rule:    {:?}", pair.as_rule());
    //             println!("Span:    {:?}", pair.as_span());
    //             println!("Text:    {}", pair.as_str());

    //             // A pair can be converted to an iterator of the tokens which make it up:
    //             for inner_pair in pair.into_inner() {
    //                 match inner_pair.as_rule() {
    //                     Rule::decimals => println!("Letter:  {}", inner_pair.as_str()),
    //                     Rule::float_lit => println!("Digit:   {}", inner_pair.as_str()),
    //                     x => println!("{:?}", x),
    //                 };
    //             }
    //         }
    //     }
    // }
}
