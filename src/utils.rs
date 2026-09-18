use std::{
    error::Error,
    fmt::{self, Debug, Display, Write},
    ops::{Add, AddAssign, Index, Range},
};

pub fn trim_ends(s: &str) -> &str {
    &s[1..(s.len() - 1)]
}

#[derive(Debug, Default, Clone, Copy)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl From<Range<usize>> for Span {
    fn from(value: Range<usize>) -> Self {
        Self { start: value.start, end: value.end }
    }
}

impl Add for Span {
    type Output = Span;

    fn add(self, rhs: Self) -> Self::Output {
        Self { start: self.start.min(rhs.start), end: rhs.end.max(rhs.end) }
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

pub struct Spanned<T: Debug> {
    pub span: Span,
    pub node: T,
}

impl<T: Debug + Clone> Debug for Spanned<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Debug::fmt(&self.node, f)?;
        f.write_char('@')?;
        Display::fmt(&self.span, f)
    }
}

impl<T: Debug> Clone for Spanned<T>
where
    T: Clone,
{
    fn clone(&self) -> Self {
        Spanned { span: self.span, node: self.node.clone() }
    }
}

impl<T: Debug + Clone> Copy for Spanned<T> where T: Copy {}

impl<T: Debug + Clone> Spanned<&T> {
    pub fn cloned(self) -> Spanned<T> {
        Spanned { span: self.span, node: self.node.clone() }
    }
}

impl<T: Debug + Clone, E: Error> Spanned<Result<T, E>> {
    pub fn transpose(self: Spanned<Result<T, E>>) -> Result<Spanned<T>, E> {
        let Spanned { span, node } = self;
        node.map(|node| Spanned { span, node })
    }
}

/// Definitely not an applicative functor 😉
impl<T: Debug + Clone> Spanned<T> {
    pub fn get_node(self) -> T {
        self.node
    }

    /// Totally not `fmap`
    pub fn map<U: Debug + Clone>(self, f: impl FnOnce(T) -> U) -> Spanned<U> {
        Spanned { span: self.span, node: f(self.node) }
    }

    pub fn map_span(self, f: impl FnOnce(Span) -> Span) -> Self {
        Spanned { span: f(self.span), node: self.node }
    }

    /// Totally not just `liftA2`
    pub fn merge<U: Debug + Clone, V: Debug + Clone>(
        self,
        other: Spanned<U>,
        f: impl FnOnce(T, U) -> V,
    ) -> Spanned<V> {
        Spanned { span: self.span + other.span, node: f(self.node, other.node) }
    }

    pub fn as_ref(&self) -> Spanned<&T> {
        Spanned { span: self.span, node: &self.node }
    }

    pub fn as_mut(&mut self) -> Spanned<&mut T> {
        Spanned { span: self.span, node: &mut self.node }
    }

    pub fn fmt_many<'a, F, Ctx>(
        things: &'a [Spanned<T>],
        fmt_thing: F,
        f: &mut std::fmt::Formatter,
        ctx: &'a Ctx,
        sep: &'static str,
    ) -> std::fmt::Result
    where
        F: Fn(&'a T, &mut std::fmt::Formatter, &'a Ctx) -> std::fmt::Result,
    {
        match things {
            [] => Ok(()),
            [sole] => fmt_thing(&sole.node, f, ctx),
            [first, rest @ ..] => {
                fmt_thing(&first.node, f, ctx)?;
                for x in rest {
                    f.write_str(sep)?;
                    fmt_thing(&x.node, f, ctx)?;
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
