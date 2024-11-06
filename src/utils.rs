use std::fmt::Debug;

#[derive(Clone, Debug)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

#[derive(Clone, Debug)]
pub struct Spanned<T: Clone + Debug> {
    pub span: Span,
    pub node: T,
}
