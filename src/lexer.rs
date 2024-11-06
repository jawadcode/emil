use logos::{Lexer as LogosLexer, Logos, Skip, SpannedIter};

use crate::utils::{Span, Spanned};

/// A wrapper around [logos::SpannedIter] that flattens the [Result] returned by [Iterator::next] into just [TK] by making use of the [TK::Error] variant
pub struct Lexer<'source> {
    source_length: usize,
    logos_iter: SpannedIter<'source, Token>,
    eof: bool,
}

impl<'source> Lexer<'source> {
    pub fn new(source: &'source str) -> Self {
        Self {
            source_length: source.len(),
            logos_iter: Token::lexer(source).spanned(),
            eof: false,
        }
    }
}

impl<'source> Iterator for Lexer<'source> {
    type Item = Spanned<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        match self
            .logos_iter
            .next()
            .map(|(res, range)| (res.unwrap_or(Token::Error), range))
        {
            Some((token, span)) => Some(Spanned {
                span: Span {
                    start: span.start,
                    end: span.end,
                },
                node: token,
            }),
            None if self.eof => None,
            None => {
                self.eof = true;
                Some(Spanned {
                    span: Span {
                        start: self.source_length,
                        end: self.source_length,
                    },
                    node: Token::Eof,
                })
            }
        }
    }
}

#[rustfmt::skip]
#[derive(Logos, Debug, Clone, PartialEq)]
pub enum Token {
	/* KEYWORDS */
	#[token("program")]   Program,
	#[token("label")]     Label,
	#[token("const")]     Const,
	#[token("type")]      Type,
	#[token("procedure")] Procedure,
	#[token("function")]  Function,
	#[token("var")]       Var,
	#[token("begin")]     Begin,
	#[token("end")]       End,
	#[token("div")]       Div,
	#[token("mod")]       Mod,
	#[token("and")]       And,
	#[token("not")]       Not,
	#[token("or")]        Or,
	#[token("in")]        In,
	#[token("array")]     Array,
	#[token("file")]      File,
	#[token("record")]    Record,
	#[token("set")]       Set,
	#[token("packed")]    Packed,
	#[token("case")]      Case,
	#[token("of")]        Of,
	#[token("for")]       For,
	#[token("to")]        To,
	#[token("downto")]    DownTo,
	#[token("do")]        Do,
	#[token("if")]        If,
	#[token("then")]      Then,
	#[token("else")]      Else,
	#[token("goto")]      Goto,
	#[token("nil")]       Nil,
	#[token("repeat")]    Repeat,
	#[token("until")]     Until,
	#[token("while")]     While,
	#[token("with")]      With,

	/* SYMBOLS */
	#[token("+")]  Plus,
	#[token("-")]  Minus,
	#[token("*")]  Asterisk,
	#[token("/")]  Slash,
	#[token("=")]  Equal,
	#[token("<")]  LessThan,
	#[token(">")]  GreaterThan,
	#[token("[")]
	#[token("(.")] LeftSquare,
	#[token("]")]
	#[token(".)")] RightSquare,
	#[token(".")]  Period,
	#[token(",")]  Comma,
	#[token(":")]  Colon,
	#[token(";")]  Semicolon,
	#[token("^")]
	#[token("@")]
	#[token("↑")]  Deref,
	#[token("(")]  LeftParen,
	#[token(")")]  RightParen,
	#[token("<>")] NotEqual,
	#[token("<=")] LessOrEqual,
    #[token(">=")] GreaterOrEqual,
	#[token(":=")] Becomes,
	#[token("..")] Elipsis,

	/* MORE COMPLEX TOKENS */
	#[regex(r"([a-z][a-z0-9]*)")] Ident,
	#[regex(r"(\+|-)?[0-9]+")] IntegerLiteral,
	#[regex(r"(\+|-)?[0-9]+\.[0-9]+((e|E)(\+|-)[0-9]+)?")] RealLiteral,
	#[regex(r"'([^']|'')+'")] StringLiteral,

	Eof,

	#[regex(r"\{|\(\*", comment_lexer)]
    #[regex(r"[\r\n\t\f\v ]+", logos::skip)]
    Error,
}

fn comment_lexer(lex: &mut LogosLexer<Token>) -> Skip {
    let rem = lex.remainder();
    let mut previous = None;

    for (idx, current) in rem.char_indices() {
        if let Some(previous) = previous {
            match (previous, current) {
                ('{', _) | ('(', '*') => (),
                (_, '}') | ('*', ')') => {
                    lex.bump(idx + 1);
                    return Skip;
                }
                _ => (),
            }
        }

        previous = Some(current);
    }

    Skip
}
