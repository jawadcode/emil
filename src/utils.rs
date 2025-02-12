use std::{
    fmt::{self, Debug, Display},
    ops::{Add, Index, Range},
};

#[derive(Debug, Clone, Copy)]
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

impl Add for Span {
    type Output = Span;

    fn add(self, rhs: Self) -> Self::Output {
        Self {
            start: self.start,
            end: rhs.end,
        }
    }
}

impl Index<Span> for str {
    type Output = str;

    fn index(&self, index: Span) -> &Self::Output {
        &self[index.start..index.end]
    }
}

#[derive(Debug, Clone)]
pub struct Spanned<T: Spannable> {
    pub span: Span,
    pub node: T,
}

pub trait Spannable {}

impl<T> Spannable for T where T: Debug + Clone {}

impl<T> Copy for Spanned<T> where T: Spannable + Copy {}

impl<T: Spannable> Spanned<T> {
    pub fn map<U: Spannable>(self, f: impl FnOnce(T) -> U) -> Spanned<U> {
        Spanned {
            span: self.span,
            node: f(self.node),
        }
    }

    /// Totally not `liftA2`
    pub fn merge<U: Spannable, V: Spannable>(
        self,
        other: Spanned<U>,
        f: impl FnOnce(T, U) -> V,
    ) -> Spanned<V> {
        Spanned {
            span: self.span + other.span,
            node: f(self.node, other.node),
        }
    }
}

impl From<Span> for Range<usize> {
    fn from(value: Span) -> Self {
        value.start..value.end
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Debug::fmt(&self.start, f)?;
        f.write_str("..")?;
        Debug::fmt(&self.end, f)
    }
}

pub struct DisplaySlice<'a, T: Display>(&'a [T]);

impl<T: Display> Display for DisplaySlice<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            [] => Ok(()),
            [kind] => Display::fmt(kind, f),
            [first, mid @ .., last] => {
                Display::fmt(first, f)?;
                for kind in mid {
                    f.write_str(", ")?;
                    Display::fmt(kind, f)?;
                }
                f.write_str(" or ")?;
                Display::fmt(last, f)
            }
        }
    }
}
