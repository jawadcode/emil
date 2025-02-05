use std::marker::PhantomData;

use chumsky::{error::Rich, extra, input::ValueInput, prelude::*, span::SimpleSpan, Parser};

use crate::lexer::Token;

#[derive(Debug, Clone)]
pub struct Stmt<'source> {
    _x: PhantomData<&'source ()>,
}

pub type CompoundStmt<'source> = Vec<Stmt<'source>>;

pub(super) fn compound_stmt<'source, I>(
) -> impl Parser<'source, I, CompoundStmt<'source>, extra::Err<Rich<'source, Token<'source>>>> + Clone
where
    I: ValueInput<'source, Token = Token<'source>, Span = SimpleSpan>,
{
    stmt()
        .separated_by(just(Token::Semicolon))
        .collect()
        .delimited_by(just(Token::Begin), just(Token::End))
}

fn stmt<'source, I>(
) -> impl Parser<'source, I, Stmt<'source>, extra::Err<Rich<'source, Token<'source>>>> + Clone
where
    I: ValueInput<'source, Token = Token<'source>, Span = SimpleSpan>,
{
    todo()
}
