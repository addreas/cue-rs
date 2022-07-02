extern crate pest;
#[macro_use]
extern crate pest_derive;

use pest::Parser;

#[derive(Parser)]
#[grammar = "cue.pest"]
pub struct CUEParser;

fn main() {
    // let successful_parse = CUEParser::parse(Rule::BasicLit, "-273.15");
    // println!("{:?}", successful_parse);

    // let unsuccessful_parse = CUEParser::parse(Rule::BasicLit, "this is not a number");
    // println!("{:?}", unsuccessful_parse);
}
