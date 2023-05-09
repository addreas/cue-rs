pub enum Feature {
    AnyDefinition,
    AnyHidden,
    AnyString,
    AnyIndex,

    InvalidLabelType,

    StringLabel(usize),
    IntLabel(usize),
    DefinitionLabel(usize),
    HiddenLabel(usize),
    HiddenDefinitionLabel(usize),
}

pub trait StringIndexer {
    fn str_to_index(&self, s: &str) -> usize;
    fn index_to_str(&self, index: usize) -> String;
}
