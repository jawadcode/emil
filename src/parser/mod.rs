mod expr;
mod program;
mod stmt;

use chumsky::{
    input::{Stream, ValueInput},
    prelude::*,
};
use program::{program, Program};

use crate::lexer::{Lexer, Token};

pub type Span = SimpleSpan;
pub type Spanned<T> = (T, Span);

pub fn parse(source: &str) -> Result<Program<'_>, Vec<Rich<'_, Token<'_>>>> {
    let token_iter = Lexer::new(source).map(|(token, span)| (token, span.into()));

    let token_stream = Stream::from_iter(token_iter).spanned((source.len()..source.len()).into());

    program().parse(token_stream).into_result()
}

fn ident<'source, I>(
) -> impl Parser<'source, I, &'source str, extra::Err<Rich<'source, Token<'source>>>> + Clone
where
    I: ValueInput<'source, Token = Token<'source>, Span = SimpleSpan>,
{
    select! {
        Token::Ident(ident) => ident,
    }
    .labelled("identifier")
}

fn tokes<'source, I, const N: usize>(
    tokens: [Token<'source>; N],
) -> impl Parser<'source, I, Token<'source>, extra::Err<Rich<'source, Token<'source>>>> + Clone
where
    I: ValueInput<'source, Token = Token<'source>, Span = SimpleSpan>,
{
    choice(tokens.map(just))
}
