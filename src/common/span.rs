use std::fmt;
use std::ops::Range;

/// A simple Span type, very similar to [`Range<usize>`] but [Copy]
///
/// It implements [chumsky::Span] and can be transformed to [miette::SourceSpan].
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Span { start, end }
    }

    pub fn single(offset: usize) -> Self {
        Span::new(offset, offset + 1)
    }

    pub fn marker(offset: usize) -> Self {
        Span::new(offset, offset)
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}..{:?}", self.start, self.end)
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}..{:?}", self.start, self.end)
    }
}

impl From<Range<usize>> for Span {
    fn from(value: Range<usize>) -> Self {
        Span {
            start: value.start,
            end: value.end,
        }
    }
}

impl From<Span> for Range<usize> {
    fn from(value: Span) -> Self {
        Range {
            start: value.start,
            end: value.end,
        }
    }
}

impl chumsky::Span for Span {
    type Context = ();
    type Offset = usize;

    fn new(_context: Self::Context, range: Range<Self::Offset>) -> Self {
        range.into()
    }

    fn context(&self) -> Self::Context {}

    fn start(&self) -> Self::Offset {
        self.start
    }

    fn end(&self) -> Self::Offset {
        self.end
    }
}

impl From<Span> for miette::SourceSpan {
    fn from(value: Span) -> Self {
        let range: Range<usize> = value.into();
        range.into()
    }
}
