use ebnf::ebnf;

// unicode_letter = /* a Unicode code point classified as "Letter" */ .
// unicode_digit  = /* a Unicode code point classified as "Number, decimal digit" */ .

ebnf! {
    letter        = unicode_letter | "_" | "$" .
    decimal_digit = "0" ... "9" .

    identifier  = [ "#" | "_#" ] letter { letter | unicode_digit } .

    int_lit     = "0" | ( "1" ... "9" ) { [ "_" ] decimal_digit } .

    null_lit   = "null" .
    bool_lit   = "true" | "false" .
    bottom_lit = "_|_" .

    StructLit  = "{" { Field  "," } "}" .
    Field      = Label ":" { Label ":" } Expression.
    Label      = identifier .

    Operand    = Literal | identifier | "(" Expression ")" .
    Literal    = int_lit | null_lit | bool_lit | bottom_lit | StructLit .

    PrimaryExpr =
        Operand |
        PrimaryExpr Selector |
        PrimaryExpr Index |
        PrimaryExpr Slice |
        PrimaryExpr Arguments .

    Selector       = "." (identifier | simple_string_lit) .
    Index          = "[" Expression "]" .
    Slice          = "[" Expression ":" Expression "]" .
    Argument       = Expression .
    Arguments      = "(" [ ( Argument { "," Argument } ) [ "," ] ] ")" .

    Expression = UnaryExpr | Expression binary_op Expression .
    UnaryExpr  = PrimaryExpr | unary_op UnaryExpr .

    binary_op  = "|" | "&" | "||" | "&&" | "==" | rel_op | add_op | mul_op  .
    rel_op     = "!=" | "<" | "<=" | ">" | ">=" | "=~" | "!~" .
    add_op     = "+" | "-" .
    mul_op     = "*" | "/" .
    unary_op   = "+" | "-" | "!" | "*" | rel_op .
}
