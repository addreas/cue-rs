use super::adt::Node;

pub struct CloseStats {
    generation: usize,

    requiredCount: usize,
    acceptedCount: usize,

    accepted: bool,

    required: bool,
    next: Box<CloseStats>,
}
pub struct CloseInfo {
    location: Box<dyn Node>,

    parent: Box<CloseInfo>,

    mode: closeNodeType,

    noCheck: bool,

    root: SpanType,
    span: SpanType,
}

enum closeNodeType {
    closeRef,
    closeDef,
    closeEmbed,
}

enum SpanType {
    EmbeddingSpan,
    ConstraintSpan,
    ComprehensionSpan,
    DefinitionSpan,
}
