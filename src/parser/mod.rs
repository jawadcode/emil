use std::{
    fmt::{self, Display},
    iter::Peekable,
};

use crate::{
    lexer::{Lexer, TokenKind},
    utils::Span,
};

pub mod expr;
pub mod program;
pub mod stmt;

pub struct Parser<'source> {
    source: &'source str,
    lexer: Peekable<Lexer<'source>>,
}

pub struct SyntaxError {
    span: Span,
    expected: String,
    got: TokenKind,
}

impl Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "SyntaxError [{}] - Expected '{}', got '{}'",
            self.span, self.expected, self.got
        )
    }
}

pub type ParseResult<T> = Result<T, SyntaxError>;

impl<'source> Parser<'source> {
    pub fn new(source: &'source str) -> Self {
        Self {
            source,
            lexer: Lexer::new(source).peekable(),
        }
    }
}
