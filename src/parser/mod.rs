use std::{
    fmt::{self, Display},
    iter::Peekable,
};

use crate::{
    lexer::{Lexer, Token, TokenKind},
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

    /// Peeks the next token and checks that it is as [expected].
    ///
    /// # Panics
    ///
    /// Panics if the lexer returns [`None`].
    ///
    fn at(&mut self, expected: TokenKind) -> bool {
        self.lexer.peek().unwrap().node == expected
    }

    /// Peeks the next token and checks that it is in the list of expected [tokens].
    ///
    /// # Panics
    ///
    /// Panics if the lexer returns [`None`].
    ///
    fn any<const N: usize>(&mut self, tokens: [TokenKind; N]) -> bool {
        tokens.contains(&self.lexer.peek().unwrap().node)
    }

    /// Gets the next token and checks that it is as [expected].
    ///
    /// # Errors
    ///
    /// Errors if the lexer returns a token that does not match [expected].
    ///
    /// # Panics
    ///
    /// Panics if the lexer returns [`None`].
    ///
    fn expect(&mut self, expected: TokenKind) -> ParseResult<Token> {
        match self.lexer.next().unwrap() {
            Token { span, node } if expected != node => Err(SyntaxError {
                span,
                expected: expected.to_string(),
                got: node,
            }),
            tok => Ok(tok),
        }
    }

    /// Advances the lexer by one token, usually for when a token has been peeked already.
    fn advance(&mut self) {
        self.lexer.next().unwrap();
    }

    /// [`expect`]s a [token] and then extracts its source.
    fn expect_source(&mut self, token: TokenKind) -> ParseResult<&'source str> {
        self.expect(token).map(|tok| &self.source[tok.span])
    }

    /// Gets the next token and extracts its source.
    fn next_source(&mut self) -> &'source str {
        &self.source[self.lexer.next().unwrap().span]
    }
}
