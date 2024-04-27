extern crate proc_macro;

use proc_macro::TokenStream;
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::{
    braced, bracketed, parenthesized, parse_macro_input, token, Field, Ident, LitStr,
    Result, Token,
};

use quote::quote;

#[proc_macro]
pub fn ebnf(tokens: TokenStream) -> TokenStream {
    let grammar = parse_macro_input!(tokens as Grammar);

    return grammar.0.iter().map(|a| {
        let name = &a.name;
        TokenStream::from(quote! {
            struct #name {}
            impl From<&str> for #name {
                fn from(value: &str) -> Self {
                    Self {}
                }
            }
        })
    }).collect();

    // let expanded = quote! {
    //     impl SomeTrait for Somethind {}
    // };

    // return TokenStream::from(expanded);
}

/*
Production  = production_name "=" [ Expression ] "." .
Expression  = Alternative { "|" Alternative } .
Alternative = Term { Term } .
Term        = production_name | token [ "…" token ] | Group | Option | Repetition .
Group       = "(" Expression ")" .
Option      = "[" Expression "]" .
Repetition  = "{" Expression "}" .
*/

struct Grammar(Punctuated<Production, Token![.]>);

impl Parse for Grammar {
    fn parse(input: ParseStream) -> Result<Self> {
        input
            .parse_terminated(Production::parse, Token![.])
            .map(Grammar)
    }
}
struct Production {
    name: Ident,
    eq_token: Token![=],
    expression: Box<Expression>,
}

impl Parse for Production {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Production {
            name: input.parse()?,
            eq_token: input.parse()?,
            expression: Box::new(input.parse()?),
        })
    }
}

struct Expression(Vec<Alternative>);

impl Parse for Expression {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut res = vec![];
        while !input.is_empty() {
            let lookahead = input.lookahead1();
            if lookahead.peek(Token![.]) {
                break;
            }
            res.push(Alternative::parse(input)?);
            let _ = input.parse::<Token![|]>();
        }
        Ok(Expression(res))
    }
}
struct Alternative(Vec<Term>);

impl Parse for Alternative {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut res = vec![];
        while !input.is_empty() {
            let lookahead = input.lookahead1();
            if lookahead.peek(Token![.]) || lookahead.peek(Token![|]) {
                break;
            }
            res.push(Term::parse(input)?);
        }
        return Ok(Alternative(res));
    }
}

enum Term {
    ProductionName(Ident),
    Token(LitStr),
    TokenRange(LitStr, LitStr),
    Group(Expression),
    Option(Expression),
    Repetition(Expression),
}
impl Parse for Term {
    fn parse(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(Ident) {
            input.parse().map(Term::ProductionName)
        } else if lookahead.peek(LitStr) {
            let first = input.parse()?;
            if !input.lookahead1().peek(Token![...]) {
                Ok(Term::Token(first))
            } else {
                let _range: Token![...] = input.parse()?;
                let second = input.parse()?;
                Ok(Term::TokenRange(first, second))
            }
        } else if lookahead.peek(token::Paren) {
            let content;
            let _ = parenthesized!(content in input);
            content.parse().map(Term::Group)
        } else if lookahead.peek(token::Bracket) {
            let content;
            let _ = bracketed!(content in input);
            content.parse().map(Term::Option)
        } else if lookahead.peek(token::Brace) {
            let content;
            let _ = braced!(content in input);
            content.parse().map(Term::Repetition)
        } else {
            Err(input.error("failed to parse"))
        }
    }
}
