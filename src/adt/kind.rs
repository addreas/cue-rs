#[derive(PartialEq, Eq, PartialOrd, Ord)]
pub enum Kind {
    TopKind, // all kinds, but not references

    ScalarKinds, // NullKind | BoolKind | IntKind | FloatKind | StringKind | BytesKind
    NumKind,     // IntKind | FloatKind

    StructKind,
    ListKind,
    FuncKind,
    BytesKind,
    StringKind,
    FloatKind,
    IntKind,
    BoolKind,
    NullKind,

    BottomKind,
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
pub enum Concreteness {
    Any,
    Type,
    Constraint,
    Concrete,
    BottomLevel,
}
