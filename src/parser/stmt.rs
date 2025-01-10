use std::marker::PhantomData;

use chumsky::{error::Rich, extra, input::ValueInput, prelude::*, span::SimpleSpan, Parser};

use crate::lexer::Token;

#[derive(Debug, Clone)]
pub struct Stmt<'source> {
    _x: PhantomData<&'source ()>,
}

pub(super) fn stmt<'source, I>(
) -> impl Parser<'source, I, Stmt<'source>, extra::Err<Rich<'source, Token<'source>>>>
where
    I: ValueInput<'source, Token = Token<'source>, Span = SimpleSpan>,
{
    todo()
}
