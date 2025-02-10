mod expr;
mod program;
mod stmt;

use chumsky::{prelude::*, Stream};
use program::{program, Program};

use crate::{
    lexer::{Lexer, Token},
    utils::Span,
};

type ParserError<'source> = Simple<Token<'source>, Span>;

pub fn parse<'source>(source: &'source str) -> Result<Program<'source>, Vec<ParserError<'source>>> {
    let token_iter = Lexer::new(source).map(|(token, span)| (token, span));

    let token_stream = Stream::from_iter((source.len()..source.len()).into(), token_iter);

    program().parse(token_stream)
}

fn ident<'source>(
) -> impl Parser<Token<'source>, &'source str, Error = ParserError<'source>> + Clone {
    select! {
        Token::Ident(ident) => ident,
    }
    .labelled("identifier")
}

fn tokes<'source, const N: usize>(
    tokens: [Token<'source>; N],
) -> impl Parser<Token<'source>, Token<'source>, Error = ParserError<'source>> + Clone {
    choice(tokens.map(just))
}
