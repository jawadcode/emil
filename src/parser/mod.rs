use std::{
    fmt::{self, Debug, Display},
    iter::Peekable,
};

use lasso::Rodeo;

use crate::{
    ast::{Ident, UnspanIdent},
    lexer::{Lexer, Token, TokenKind},
    utils::{Span, Spanned},
};

pub mod expr;
pub mod program;
pub mod stmt;

pub struct ParserState<'source> {
    source: &'source str,
    rodeo: Rodeo,
    lexer: Peekable<Lexer<'source>>,
}

pub struct SyntaxError {
    pub expected: String,
    pub got: Token,
}

pub type SpanParseResult<T: Clone + Debug> = Result<Spanned<T>, SyntaxError>;

pub type ParseResult<T> = Result<T, SyntaxError>;

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

enum Builtin {
    Write,
    Writeln,
    Read,
    Readln,
}

impl<'source> ParserState<'source> {
    pub fn new(source: &'source str) -> Self {
        Self {
            source,
            rodeo: Rodeo::new(),
            lexer: Lexer::new(source).peekable(),
        }
    }

    /// Expects an identifier, extracts its source and then returns an interned identifier.
    fn ident(&mut self) -> SpanParseResult<UnspanIdent> {
        Ok(self
            .expect_source(TokenKind::Ident)?
            .map(|ident| self.rodeo.get_or_intern(ident.to_lowercase())))
    }

    fn advance_ident(&mut self) -> Ident {
        self.advance_source()
            .map(|ident| self.rodeo.get_or_intern(ident.to_lowercase()))
    }

    fn builtin_name(&mut self, name: Builtin) -> UnspanIdent {
        self.rodeo.get_or_intern(match name {
            Builtin::Write => "write",
            Builtin::Writeln => "writeln",
            Builtin::Read => "read",
            Builtin::Readln => "readln",
        })
    }

    /// Gets the next token from the lexer.
    ///
    /// # Errors
    ///
    /// Errors if the next token is [`TokenKind::Eof`].
    fn next(&mut self) -> SpanParseResult<TokenKind> {
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
    fn expect(&mut self, expected: TokenKind) -> SpanParseResult<TokenKind> {
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
    fn expect_source(&mut self, expected: TokenKind) -> SpanParseResult<&'source str> {
        let Token { span, .. } = self.expect(expected)?;
        Ok(Spanned {
            span,
            node: &self.source[span],
        })
    }

    /// Gets the next token and returns its corresponding source string slice.
    ///
    /// # Panics
    ///
    /// Panics if the lexer returns [`None`].
    fn advance_source(&mut self) -> Spanned<&'source str> {
        let Token { span, .. } = self.advance();
        Spanned {
            span,
            node: &self.source[span],
        }
    }

    /// Parses 0 or more of [parser], repeating on the apperance of the token [on].
    ///
    /// # TODO
    ///
    /// Ideally the [parser] would return [`ControlFlow`] so we can allow it to break out of the
    /// loop early.
    fn repeated<T: Clone + Debug>(
        &mut self,
        on: impl TokenPred,
        parser: impl Fn(&mut ParserState<'source>) -> SpanParseResult<T>,
    ) -> SpanParseResult<Vec<Spanned<T>>> {
        let mut items = Vec::new();
        let mut span = (0..0).into();
        while on.clone().apply(self) {
            let parsed = parser(self)?;
            span = span + parsed.span;
            items.push(parsed);
        }
        Ok(Spanned { span, node: items })
    }

    /// Parses one or more of [parser], repeating based on the apperance of [separator].
    fn repeat_sep<T: Debug + Clone>(
        &mut self,
        separator: impl TokenPred,
        parser: impl Fn(&mut ParserState<'source>) -> SpanParseResult<T>,
    ) -> ParseResult<Vec<Spanned<T>>> {
        let mut items = Vec::new();
        let first = parser(self)?;
        items.push(first);

        while self.is(separator.clone()) {
            self.advance();
            items.push(parser(self)?);
        }

        Ok(items)
    }

    /// Repeatedly apply an extension parser [ext] to the result of an initial parser [init].
    fn repeat_fold<T: Clone + Debug, P, I>(
        &mut self,
        start: impl TokenPred,
        ext: P,
        init: I,
    ) -> SpanParseResult<T>
    where
        P: Fn(&mut ParserState<'source>, Spanned<T>) -> SpanParseResult<T>,
        I: Fn(&mut ParserState<'source>) -> SpanParseResult<T>,
    {
        let mut result = init(self)?;

        while self.is(start.clone()) {
            let lhs_span = result.span;
            let ext = ext(self, result)?;
            result = Spanned {
                span: lhs_span + ext.span,
                node: ext,
            };
        }

        Ok(result)
    }

    /// Constructs an error that consumes the next token.
    /// For use with [`Parser::peek`] or [`Parser::is`].
    fn next_error<T: Clone + Debug>(&mut self, expected: &str) -> SpanParseResult<T> {
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

/// Return an empty list with a nonsense span as it should never be accessed
fn empty_list<T: Clone + std::fmt::Debug>() -> Spanned<Vec<T>> {
    Spanned {
        span: (0..0).into(),
        node: Vec::new(),
    }
}

fn add_span_opt(lhs: Span, rhs: Option<Span>) -> Span {
    if let Some(rhs) = rhs {
        lhs + rhs
    } else {
        lhs
    }
}
