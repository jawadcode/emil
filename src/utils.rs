use ariadne::Span as AriadneSpan;
use chumsky::Span as ChumskySpan;
use std::{
    fmt::{self, Debug, Display},
    hash::Hash,
    ops::Range,
};

#[derive(Debug, Clone)]
pub struct Span {
    start: usize,
    end: usize,
}

impl From<Range<usize>> for Span {
    fn from(value: Range<usize>) -> Self {
        Self {
            start: value.start,
            end: value.end,
        }
    }
}

impl ChumskySpan for Span {
    type Context = ();

    type Offset = usize;

    fn new((): Self::Context, range: Range<Self::Offset>) -> Self {
        range.into()
    }

    fn context(&self) -> Self::Context {
        ()
    }

    fn start(&self) -> Self::Offset {
        self.start
    }

    fn end(&self) -> Self::Offset {
        self.end
    }
}

#[derive(Debug, Clone)]
pub struct SpanInfo(pub String, pub Span);

impl AriadneSpan for SpanInfo {
    type SourceId = String;

    fn source(&self) -> &Self::SourceId {
        &self.0
    }

    fn start(&self) -> usize {
        self.1.start
    }

    fn end(&self) -> usize {
        self.1.end
    }
}

impl Into<Range<usize>> for Span {
    fn into(self) -> Range<usize> {
        self.start..self.end
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Debug::fmt(&self.start, f)?;
        f.write_str("..")?;
        Debug::fmt(&self.end, f)
    }
}
