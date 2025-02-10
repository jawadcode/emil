use std::{
    fmt::{self, Display, Write},
    num::NonZero,
    ops::Range,
};

use lexical_core::{parse_with_options, NumberFormatBuilder, ParseFloatOptions};
use logos::{Lexer as LogosLexer, Logos, Skip, SpannedIter};

use crate::utils::Span;

/// A wrapper around [logos::SpannedIter] that flattens the [Result<Token>] returned by
/// [Iterator::next] into just [Token] by making use of the [Token::Error] variant.
pub struct Lexer<'source> {
    logos_iter: SpannedIter<'source, Token<'source>>,
}

impl<'source> Lexer<'source> {
    pub fn new(source: &'source str) -> Self {
        Self {
            logos_iter: Token::lexer(source).spanned(),
        }
    }
}

impl<'source> Iterator for Lexer<'source> {
    type Item = (Token<'source>, Span);

    fn next(&mut self) -> Option<Self::Item> {
        self.logos_iter
            .next()
            .map(|(res, range)| (res.unwrap_or(Token::Error), range.into()))
    }
}

#[rustfmt::skip]
#[derive(Logos, Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Token<'source> {
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
    // #[token("true", ignore(case))]                      True,
    // #[token("false", ignore(case))]                     False,
    #[regex(r"([a-z][a-z0-9]*)")]   Ident(&'source str),
    #[regex(r"[0-9]+", parse_unsigned_integer)]            UIntLit(u64),
    #[regex(r"(\+|-)[0-9]+", parse_int)]                   IntLit(i64),
    #[regex(r"(\+|-)?[0-9]+(e|E)(\+|-)[0-9]+")]
    #[regex(r"(\+|-)?[0-9]+\.[0-9]+((e|E)(\+|-)[0-9]+)?")] RealLit(&'source str),
    #[regex(r"'([^']|'')+'")]                              StrLit(&'source str),

    #[regex(r"\{|\(\*", comment_lexer)]
    #[regex(r"[\r\n\t\f\v ]+", logos::skip)]
    Error,
}

fn parse_uint<'source, C: Iterator<Item = u8>>(chars: &mut C) -> u64 {
    chars
        .map(|c| c - b'0')
        .fold(0, |acc, d| acc * 10 + (d as u64))
}

fn parse_unsigned_integer<'source>(lex: &mut LogosLexer<'source, Token<'source>>) -> u64 {
    parse_uint(&mut lex.slice().bytes())
}

fn parse_int<'source>(lex: &mut LogosLexer<'source, Token<'source>>) -> i64 {
    let mut chars = lex.slice().bytes().peekable();
    let sign = chars.next_if(|c| [b'+', b'-'].contains(c));
    let num = parse_uint(&mut chars) as i64;
    match sign {
        Some(b'-') => -num,
        Some(b'+') | None => num,
        _ => unreachable!(),
    }
}

const REAL_LITERAL_FORMAT: u128 = NumberFormatBuilder::new()
    .digit_separator(NonZero::new(b'.'))
    .required_integer_digits(true)
    .required_fraction_digits(true)
    .required_mantissa_digits(true)
    .required_mantissa_sign(true)
    .build();

pub(super) fn parse_real<'source>(tok: &'source str) -> f64 {
    parse_with_options::<f64, REAL_LITERAL_FORMAT>(tok.as_bytes(), &ParseFloatOptions::new())
        .unwrap()
}

fn comment_lexer<'source>(lex: &mut LogosLexer<'source, Token<'source>>) -> Skip {
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

impl Display for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Program => f.write_str("program"),
            Token::Label => f.write_str("label"),
            Token::Const => f.write_str("const"),
            Token::Type => f.write_str("type"),
            Token::Procedure => f.write_str("procedure"),
            Token::Function => f.write_str("function"),
            Token::Var => f.write_str("var"),
            Token::Begin => f.write_str("begin"),
            Token::End => f.write_str("end"),
            Token::Div => f.write_str("div"),
            Token::Mod => f.write_str("mod"),
            Token::And => f.write_str("and"),
            Token::Not => f.write_str("not"),
            Token::Or => f.write_str("or"),
            Token::In => f.write_str("in"),
            Token::Array => f.write_str("array"),
            Token::File => f.write_str("file"),
            Token::Record => f.write_str("record"),
            Token::Set => f.write_str("set"),
            Token::Packed => f.write_str("packed"),
            Token::Case => f.write_str("case"),
            Token::Of => f.write_str("of"),
            Token::For => f.write_str("for"),
            Token::To => f.write_str("to"),
            Token::DownTo => f.write_str("downto"),
            Token::Do => f.write_str("do"),
            Token::If => f.write_str("if"),
            Token::Then => f.write_str("then"),
            Token::Else => f.write_str("else"),
            Token::Goto => f.write_str("goto"),
            Token::Repeat => f.write_str("repeat"),
            Token::Until => f.write_str("until"),
            Token::While => f.write_str("while"),
            Token::With => f.write_str("with"),
            Token::Plus => f.write_char('+'),
            Token::Minus => f.write_char('-'),
            Token::Asterisk => f.write_char('*'),
            Token::Slash => f.write_char('/'),
            Token::Eq => f.write_char('='),
            Token::LT => f.write_char('<'),
            Token::GT => f.write_char('>'),
            Token::LSquare => f.write_char('['),
            Token::LSquareAlt => f.write_str("(."),
            Token::RSquare => f.write_char(']'),
            Token::RSquareAlt => f.write_str(".)"),
            Token::Dot => f.write_char('.'),
            Token::Comma => f.write_char(','),
            Token::Colon => f.write_char(':'),
            Token::Semicolon => f.write_char(';'),
            Token::UpArrow => f.write_char('↑'),
            Token::Caret => f.write_char('^'),
            Token::At => f.write_char('@'),
            Token::LParen => f.write_char('('),
            Token::RParen => f.write_char(')'),
            Token::NEq => f.write_str("<>"),
            Token::LEq => f.write_str("<="),
            Token::GEq => f.write_str(">="),
            Token::Becomes => f.write_str(":="),
            Token::Ellipsis => f.write_str(".."),
            Token::Nil => f.write_str("nil"),
            Token::Ident(ident) => write!(f, "identifier '{ident}'"),
            Token::UIntLit(uintlit) => {
                write!(f, "unsigned integer literal '{uintlit}'")
            }
            Token::IntLit(intlit) => write!(f, "signed integer literal '{intlit}'"),
            Token::RealLit(reallit) => write!(f, "real literal '{reallit}'"),
            Token::StrLit(strlit) => write!(f, "string literal {strlit}"),
            Token::Error => f.write_str("invalid token"),
        }
    }
}
