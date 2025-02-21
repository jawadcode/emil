use std::{
    fmt::{self, Display, Write},
    num::NonZero,
};

use lexical_core::{parse_with_options, NumberFormatBuilder, ParseFloatOptions};
use logos::{Lexer as LogosLexer, Logos, Skip, SpannedIter};

use crate::utils::Spanned;

/// A wrapper around [`logos::SpannedIter`] that flattens the [`Result<Token>`] returned by
/// [`Iterator::next`] into just [`Token`] by making use of the [`Token::Error`] variant.
///
/// N.B. This is an awful API change on the part of [`logos`] and I hope it is reverted.
pub struct Lexer<'source> {
    eof: bool,
    logos_iter: SpannedIter<'source, TokenKind>,
    end: usize,
}

impl<'source> Lexer<'source> {
    pub fn new(source: &'source str) -> Self {
        Self {
            eof: false,
            logos_iter: TokenKind::lexer(source).spanned(),
            end: source.len(),
        }
    }
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        match self.logos_iter.next() {
            None if self.eof => None,
            None => Some(Token {
                span: (self.end..self.end).into(),
                node: TokenKind::Eof,
            }),
            Some((tok, span)) => Some(Token {
                span: span.into(),
                node: tok.unwrap_or(TokenKind::Error),
            }),
        }
    }
}

pub type Token = Spanned<TokenKind>;

#[rustfmt::skip]
#[derive(Logos, Debug, Clone, Copy, PartialEq, Hash)]
pub enum TokenKind {
    /* KEYWORDS */
    #[token("program", ignore(case))]   Program,
    #[token("label", ignore(case))]     Label,
    #[token("const", ignore(case))]     Const,
    #[token("type", ignore(case))]      Type,
    #[token("procedure", ignore(case))] Procedure,
    #[token("function", ignore(case))]  Function,
    #[token("var", ignore(case))]       Var,
    #[token("begin", ignore(case))]     Begin,
    #[token("end", ignore(case))]       End,
    #[token("div", ignore(case))]       Div,
    #[token("mod", ignore(case))]       Mod,
    #[token("and", ignore(case))]       And,
    #[token("not", ignore(case))]       Not,
    #[token("or", ignore(case))]        Or,
    #[token("in", ignore(case))]        In,
    #[token("array", ignore(case))]     Array,
    #[token("file", ignore(case))]      File,
    #[token("record", ignore(case))]    Record,
    #[token("set", ignore(case))]       Set,
    #[token("packed", ignore(case))]    Packed,
    #[token("case", ignore(case))]      Case,
    #[token("of", ignore(case))]        Of,
    #[token("for", ignore(case))]       For,
    #[token("to", ignore(case))]        To,
    #[token("downto", ignore(case))]    DownTo,
    #[token("do", ignore(case))]        Do,
    #[token("if", ignore(case))]        If,
    #[token("then", ignore(case))]      Then,
    #[token("else", ignore(case))]      Else,
    #[token("goto", ignore(case))]      Goto,
    #[token("repeat", ignore(case))]    Repeat,
    #[token("until", ignore(case))]     Until,
    #[token("while", ignore(case))]     While,
    #[token("with", ignore(case))]      With,

    /* SYMBOLS */
    #[token("+")]  Plus,
    #[token("-")]  Minus,
    #[token("*")]  Asterisk,
    #[token("/")]  Slash,
    #[token("=")]  Eq,
    #[token("<")]  LT,
    #[token(">")]  GT,
    #[token("[")]  LSquare,
    #[token("(.")] LSquareAlt,
    #[token("]")]  RSquare,
    #[token(".)")] RSquareAlt,
    #[token(".")]  Dot,
    #[token(",")]  Comma,
    #[token(":")]  Colon,
    #[token(";")]  Semicolon,
    #[token("↑")]  UpArrow,
    #[token("^")]  Caret,
    #[token("@")]  At,
    #[token("(")]  LParen,
    #[token(")")]  RParen,
    #[token("<>")] NEq,
    #[token("<=")] LEq,
    #[token(">=")] GEq,
    #[token(":=")] Becomes,
    #[token("..")] Ellipsis,

    /* LITERALS */
    #[token("nil", ignore(case))]                          Nil,
    // Both are just built-in constant identifiers 🙄
    // #[token("true", ignore(case))] True,
    // #[token("false", ignore(case))] False,
    #[regex(r"([A-Za-z][A-Za-z0-9]*)")]                         Ident,
    #[regex(r"[0-9]+")]                                   UIntLit,
    #[regex(r"(\+|-)?[0-9]+(e|E)(\+|-)[0-9]+")]
    #[regex(r"[0-9]+\.[0-9]+((e|E)(\+|-)[0-9]+)?")]       URealLit,
    #[regex(r"'([^']|'')+'")]                             StrLit,

    #[regex(r"\{|\(\*", comment_lexer)]
    #[regex(r"[\r\n\t\f\v ]+", logos::skip)]
    Error,

    Eof,
}

fn parse_uint<C: Iterator<Item = u8>>(chars: &mut C) -> u64 {
    chars
        .map(|c| c - b'0')
        .fold(0, |acc, d| acc * 10 + (d as u64))
}

pub fn parse_unsigned_integer(tok: &str) -> u64 {
    parse_uint(&mut tok.bytes())
}

const REAL_LITERAL_FORMAT: u128 = NumberFormatBuilder::new()
    .digit_separator(NonZero::new(b'.'))
    .required_integer_digits(true)
    .required_fraction_digits(true)
    .required_mantissa_digits(true)
    .required_mantissa_sign(true)
    .build();

pub fn parse_unsigned_real(tok: &str) -> f64 {
    parse_with_options::<f64, REAL_LITERAL_FORMAT>(tok.as_bytes(), &ParseFloatOptions::new())
        .unwrap()
}

fn comment_lexer(lex: &mut LogosLexer<'_, TokenKind>) -> Skip {
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

impl Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenKind::Program => f.write_str("program"),
            TokenKind::Label => f.write_str("label"),
            TokenKind::Const => f.write_str("const"),
            TokenKind::Type => f.write_str("type"),
            TokenKind::Procedure => f.write_str("procedure"),
            TokenKind::Function => f.write_str("function"),
            TokenKind::Var => f.write_str("var"),
            TokenKind::Begin => f.write_str("begin"),
            TokenKind::End => f.write_str("end"),
            TokenKind::Div => f.write_str("div"),
            TokenKind::Mod => f.write_str("mod"),
            TokenKind::And => f.write_str("and"),
            TokenKind::Not => f.write_str("not"),
            TokenKind::Or => f.write_str("or"),
            TokenKind::In => f.write_str("in"),
            TokenKind::Array => f.write_str("array"),
            TokenKind::File => f.write_str("file"),
            TokenKind::Record => f.write_str("record"),
            TokenKind::Set => f.write_str("set"),
            TokenKind::Packed => f.write_str("packed"),
            TokenKind::Case => f.write_str("case"),
            TokenKind::Of => f.write_str("of"),
            TokenKind::For => f.write_str("for"),
            TokenKind::To => f.write_str("to"),
            TokenKind::DownTo => f.write_str("downto"),
            TokenKind::Do => f.write_str("do"),
            TokenKind::If => f.write_str("if"),
            TokenKind::Then => f.write_str("then"),
            TokenKind::Else => f.write_str("else"),
            TokenKind::Goto => f.write_str("goto"),
            TokenKind::Repeat => f.write_str("repeat"),
            TokenKind::Until => f.write_str("until"),
            TokenKind::While => f.write_str("while"),
            TokenKind::With => f.write_str("with"),
            TokenKind::Plus => f.write_char('+'),
            TokenKind::Minus => f.write_char('-'),
            TokenKind::Asterisk => f.write_char('*'),
            TokenKind::Slash => f.write_char('/'),
            TokenKind::Eq => f.write_char('='),
            TokenKind::LT => f.write_char('<'),
            TokenKind::GT => f.write_char('>'),
            TokenKind::LSquare => f.write_char('['),
            TokenKind::LSquareAlt => f.write_str("(."),
            TokenKind::RSquare => f.write_char(']'),
            TokenKind::RSquareAlt => f.write_str(".)"),
            TokenKind::Dot => f.write_char('.'),
            TokenKind::Comma => f.write_char(','),
            TokenKind::Colon => f.write_char(':'),
            TokenKind::Semicolon => f.write_char(';'),
            TokenKind::UpArrow => f.write_char('↑'),
            TokenKind::Caret => f.write_char('^'),
            TokenKind::At => f.write_char('@'),
            TokenKind::LParen => f.write_char('('),
            TokenKind::RParen => f.write_char(')'),
            TokenKind::NEq => f.write_str("<>"),
            TokenKind::LEq => f.write_str("<="),
            TokenKind::GEq => f.write_str(">="),
            TokenKind::Becomes => f.write_str(":="),
            TokenKind::Ellipsis => f.write_str(".."),
            TokenKind::Nil => f.write_str("nil"),
            TokenKind::Ident => f.write_str("identifier"),
            TokenKind::UIntLit => f.write_str("unsigned integer literal"),
            TokenKind::URealLit => f.write_str("unsigned real literal"),
            TokenKind::StrLit => f.write_str("string literal"),
            TokenKind::Error => f.write_str("invalid token"),
            TokenKind::Eof => f.write_str("end of file"),
        }
    }
}
