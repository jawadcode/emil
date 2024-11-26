//! Credit to https://www.cs.kent.edu/~durand/CS43101Fall2004/resources/Pascal-EBNF.html#expression
//! for providing a simple BNF to base this parser off of.

use chumsky::{
    input::{Stream, ValueInput},
    pratt::{infix, left, prefix},
    prelude::*,
};

use crate::lexer::{Lexer, Token};

pub type Span = SimpleSpan;
pub type Spanned<T> = (T, Span);

#[derive(Debug)]
pub enum Expr<'source> {
    Variable(&'source str),
    ReferencedVariable(&'source str),
    IntLit(i64),
    RealLit(f64),
    StrLit(&'source str),
    UnaryOperation {
        op: UnaryOp,
        operand: Box<Expr<'source>>,
    },
    BinaryOperation {
        op: BinaryOp,
        left: Box<Expr<'source>>,
        right: Box<Expr<'source>>,
    },
}

#[derive(Debug)]
pub enum UnaryOp {
    Not,
    Identity,
    Negation,
}

#[derive(Debug)]
pub enum BinaryOp {
    Multiply,
    Divide,
    Modulo,
    And,

    Add,
    Subtract,
    Or,

    Equals,
    NotEquals,
    LessThan,
    GreaterThan,
    LessEquals,
    GreaterEquals,
    In,
}

pub fn parse<'source>(
    source: &'source str,
) -> Result<Expr<'source>, Vec<Rich<'source, Token<'source>>>> {
    let token_iter = Lexer::new(source).map(|(token, span)| (token, span.into()));

    let token_stream = Stream::from_iter(token_iter).spanned((source.len()..source.len()).into());

    expr_parser().parse(token_stream).into_result()
}

macro_rules! binop {
    ($op:path) => {
        |left, right| Expr::BinaryOperation {
            op: $op,
            left,
            right,
        }
    };
}

fn expr_parser<'source, I>(
) -> impl Parser<'source, I, Expr<'source>, extra::Err<Rich<'source, Token<'source>>>>
where
    I: ValueInput<'source, Token = Token<'source>, Span = SimpleSpan>,
{
    let variable = ident()
        .map(|name| Expr::Variable(name))
        .labelled("variable");
    let referenced_variable = ident()
        .then_ignore(choice((
            just(Token::Caret),
            just(Token::UpArrow),
            just(Token::At),
        )))
        .map(|name| Expr::ReferencedVariable(name))
        .labelled("referenced variable");

    let atom = choice((variable, referenced_variable));
    let expr = atom.pratt((
        prefix(4, just(Token::Not), |operand| Expr::UnaryOperation {
            op: UnaryOp::Not,
            operand,
        }),
        infix(left(3), just(Token::Asterisk), |left, right| {
            Expr::BinaryOperation {
                op: BinaryOp::Multiply,
                left,
                right,
            }
        }),
    ));
    todo()
}

fn ident<'source, I>(
) -> impl Parser<'source, I, &'source str, extra::Err<Rich<'source, Token<'source>>>>
where
    I: ValueInput<'source, Token = Token<'source>, Span = SimpleSpan>,
{
    select! {
        Token::Ident(ident) => ident,
    }
}
