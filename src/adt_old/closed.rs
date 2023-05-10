use super::adt::Node;
#[allow(dead_code)]
pub struct CloseStats {
    generation: usize,

    required_count: usize,
    accepted_count: usize,

    accepted: bool,

    required: bool,
    next: Box<CloseStats>,
}
#[allow(dead_code)]
pub struct CloseInfo {
    location: Box<dyn Node>,

    parent: Box<CloseInfo>,

    mode: CloseNodeType,

    no_check: bool,

    root: SpanType,
    span: SpanType,
}
#[allow(dead_code)]
enum CloseNodeType {
    CloseRef,
    CloseDef,
    CloseEmbed,
}
#[allow(dead_code)]
enum SpanType {
    EmbeddingSpan,
    ConstraintSpan,
    ComprehensionSpan,
    DefinitionSpan,
}
