use std::{
    fmt::{self, Display},
    iter::Peekable,
};

use crate::lexer::{Lexer, Token, TokenKind};

pub mod expr;
pub mod program;
pub mod stmt;

pub struct ParserState<'source> {
    source: &'source str,
    lexer: Peekable<Lexer<'source>>,
}

trait TokenPred: Clone {
    fn apply(self, parser: &mut ParserState) -> bool;
}

impl TokenPred for TokenKind {
    fn apply(self, parser: &mut ParserState) -> bool {
        parser.peek() == self
    }
}

impl TokenPred for &[TokenKind] {
    fn apply(self, parser: &mut ParserState) -> bool {
        self.contains(&parser.peek())
    }
}

impl<const N: usize> TokenPred for &[TokenKind; N] {
    fn apply(self, parser: &mut ParserState) -> bool {
        self.contains(&parser.peek())
    }
}

impl<F> TokenPred for F
where
    F: Fn(&mut ParserState) -> bool + Clone,
{
    fn apply(self, parser: &mut ParserState) -> bool {
        self(parser)
    }
}

pub struct SyntaxError {
    pub expected: String,
    pub got: Token,
}

pub type ParseResult<T> = Result<T, SyntaxError>;

impl<'source> ParserState<'source> {
    pub fn new(source: &'source str) -> Self {
        Self {
            source,
            lexer: Lexer::new(source).peekable(),
        }
    }

    /// Gets the next token from the lexer.
    ///
    /// # Errors
    ///
    /// Errors if the next token is [`TokenKind::Eof`].
    fn next(&mut self) -> ParseResult<Token> {
        self.lexer.next().ok_or_else(|| self.error_eof())
    }

    /// Peeks the next token's kind.
    fn peek(&mut self) -> TokenKind {
        self.lexer
            .peek()
            .map(|tok| tok.node)
            .unwrap_or(TokenKind::Eof)
    }

    /// Gets the next token from the lexer, checking that it is as [expected].
    ///
    /// # Errors
    ///
    /// Errors if the lexer returns `None`, or the next token does not match [expected].
    fn expect(&mut self, expected: TokenKind) -> ParseResult<Token> {
        self.next().and_then(|got| {
            if got.node != expected {
                Err(SyntaxError {
                    expected: expected.to_string(),
                    got,
                })
            } else {
                Ok(got)
            }
        })
    }

    /// Peeks the next token's kind and checks that it is as [expected].
    fn is(&mut self, expected: impl TokenPred) -> bool {
        expected.apply(self)
    }

    /// Gets the next token, panicking if the lexer returns [`None`].
    fn advance(&mut self) -> Token {
        self.lexer.next().unwrap()
    }

    /// Gets the next token, checks that it is as expected, and then returns its corresponding
    /// source string slice.
    ///
    /// # Errors
    ///
    /// Errors if the lexer returns [`None`], or the next token and [expected] do not match.
    fn expect_source(&mut self, expected: TokenKind) -> ParseResult<&'source str> {
        let Token { span, .. } = self.expect(expected)?;
        Ok(&self.source[span])
    }

    /// Gets the next token and returns its corresponding source string slice.
    ///
    /// # Panics
    ///
    /// Panics if the lexer returns [`None`].
    fn advance_source(&mut self) -> &'source str {
        let Token { span, .. } = self.advance();
        &self.source[span]
    }

    /// Parses 0 or more of [parser], repeating on the apperance of the token [on].
    ///
    /// # TODO
    ///
    /// Ideally the [parser] would return [`ControlFlow`] so we can allow it to break out of the
    /// loop early.
    fn repeated<T>(
        &mut self,
        on: impl TokenPred,
        parser: impl Fn(&mut ParserState<'source>) -> ParseResult<T>,
    ) -> ParseResult<Vec<T>> {
        let mut items = Vec::new();
        while on.clone().apply(self) {
            items.push(parser(self)?);
        }
        Ok(items)
    }

    /// Parses one or more of [parser], repeating based on the apperance of the token [on] and [separator].
    fn repeat_sep<T>(
        &mut self,
        separator: impl TokenPred,
        parser: impl Fn(&mut ParserState<'source>) -> ParseResult<T>,
    ) -> ParseResult<Vec<T>> {
        let mut items = Vec::new();
        items.push(parser(self)?);

        while self.is(separator.clone()) {
            self.advance();
            items.push(parser(self)?);
        }

        Ok(items)
    }

    /// Repeatedly apply an extension parser [ext] to the result of an initial parser [init].
    fn repeat_fold<T, P, I>(&mut self, start: impl TokenPred, ext: P, init: I) -> ParseResult<T>
    where
        P: Fn(&mut ParserState<'source>, T) -> ParseResult<T>,
        I: Fn(&mut ParserState<'source>) -> ParseResult<T>,
    {
        let mut result = init(self)?;

        while self.is(start.clone()) {
            result = ext(self, result)?;
        }

        Ok(result)
    }

    /// Constructs an error that consumes the next token.
    /// For use with [`Parser::peek`] or [`Parser::is`].
    fn next_error<T>(&mut self, expected: &str) -> ParseResult<T> {
        let expected = expected.to_owned();
        let got = self.next()?;
        Err(SyntaxError { expected, got })
    }

    /// Constructs an error for an unexpected [`TokenKind::Eof`].
    fn error_eof(&self) -> SyntaxError {
        let end = self.source.len();
        SyntaxError {
            expected: "token".to_string(),
            got: Token {
                span: (end..end).into(),
                node: TokenKind::Eof,
            },
        }
    }
}

impl Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "SyntaxError [{}] - Expected '{}', got '{}'",
            self.got.span, self.expected, self.got.node
        )
    }
}
