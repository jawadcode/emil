use std::{
    fmt::{self, Debug, Display},
    ops::{Add, AddAssign, Index, Range},
};

pub fn trim_ends(s: &str) -> &str {
    &s[1..(s.len() - 1)]
}

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub start: usize,
    pub end: usize,
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
            start: self.start.min(rhs.start),
            end: rhs.end.max(rhs.end),
        }
    }
}

impl AddAssign for Span {
    fn add_assign(&mut self, rhs: Self) {
        self.start = self.start.min(rhs.start);
        self.end = self.end.max(rhs.end);
    }
}

impl Index<Span> for str {
    type Output = str;

    fn index(&self, index: Span) -> &Self::Output {
        &self[index.start..index.end]
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.start, f)?;
        f.write_str("..")?;
        Display::fmt(&self.end, f)
    }
}

/* A little bit of fun */

#[derive(Debug, Clone)]
pub struct Spanned<T: Debug + Clone> {
    pub span: Span,
    pub node: T,
}

impl<T: Debug + Clone> Copy for Spanned<T> where T: Copy {}

/// Definitely not an applicative functor 😉
impl<T: Debug + Clone> Spanned<T> {
    pub fn get_node(self) -> T {
        self.node
    }

    /// Totally not `fmap`
    pub fn map<U: Debug + Clone>(self, f: impl FnOnce(T) -> U) -> Spanned<U> {
        Spanned {
            span: self.span,
            node: f(self.node),
        }
    }

    #[allow(dead_code)]
    /// Totally not just `liftA2`
    pub fn merge<U: Debug + Clone, V: Debug + Clone>(
        self,
        other: Spanned<U>,
        f: impl FnOnce(T, U) -> V,
    ) -> Spanned<V> {
        Spanned {
            span: self.span + other.span,
            node: f(self.node, other.node),
        }
    }

    pub fn as_ref(&self) -> Spanned<&T> {
        Spanned {
            span: self.span,
            node: &self.node,
        }
    }

    pub fn fmt_many<'a, F>(
        f: &mut std::fmt::Formatter,
        things: &'a [Spanned<T>],
        fmt_thing: F,
        sep: &'static str,
    ) -> std::fmt::Result
    where
        F: Fn(&mut std::fmt::Formatter, &'a T) -> std::fmt::Result,
    {
        match things {
            [] => Ok(()),
            [sole] => fmt_thing(f, &sole.node),
            [first, rest @ ..] => {
                fmt_thing(f, &first.node)?;
                for x in rest {
                    f.write_str(sep)?;
                    fmt_thing(f, &x.node)?;
                    f.write_str("@")?;
                    Display::fmt(&x.span, f)?;
                }
                Ok(())
            }
        }
    }
}

impl From<Span> for Range<usize> {
    fn from(value: Span) -> Self {
        value.start..value.end
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

/// The venerable `Arrow.(>>>)`
pub fn then<A, B, C, F, G>(f: F, g: G) -> impl FnOnce(A) -> C
where
    F: FnOnce(A) -> B,
    G: FnOnce(B) -> C,
{
    |x| g(f(x))
}
