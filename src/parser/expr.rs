use chumsky::{error::Rich, extra, input::ValueInput, prelude::*, span::SimpleSpan, Parser};

use crate::lexer::Token;

use super::{ident, tokes};

#[derive(Debug, Clone)]
pub enum Expr<'source> {
    Var(Var<'source>),
    Nil,
    True,
    False,
    IntLit(i64),
    RealLit(f64),
    StrLit(&'source str),
    Set(Vec<Expr<'source>>),
    FuncCall(&'source str, Vec<Expr<'source>>),
    UnaryOp {
        op: UnaryOp,
        operand: Box<Expr<'source>>,
    },
    BinOp {
        op: BinOp,
        left: Box<Expr<'source>>,
        right: Box<Expr<'source>>,
    },
}

#[derive(Debug, Clone)]
pub enum Var<'source> {
    Plain(&'source str),
    RefVar(Box<Var<'source>>),
    IndexedVar(Box<Var<'source>>, Vec<Expr<'source>>),
    FieldAccess(Box<Var<'source>>, &'source str),
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Not,
    Identity,
    Negation,
}

impl<'source> From<Token<'source>> for UnaryOp {
    fn from(value: Token<'source>) -> Self {
        match value {
            Token::Plus => UnaryOp::Identity,
            Token::Minus => UnaryOp::Negation,
            Token::Not => UnaryOp::Not,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum BinOp {
    Mult,
    Div,
    Mod,
    And,

    Add,
    Sub,
    Or,

    Eq,
    NEq,
    LT,
    GT,
    LEq,
    GEq,
    In,
}

impl<'source> From<Token<'source>> for BinOp {
    fn from(value: Token<'source>) -> Self {
        match value {
            Token::Asterisk => Self::Mult,
            Token::Slash => Self::Div,
            Token::Mod => Self::Mod,
            Token::And => Self::And,
            Token::Plus => Self::Add,
            Token::Minus => Self::Sub,
            Token::Or => Self::Or,
            Token::Eq => Self::Eq,
            Token::NEq => Self::NEq,
            Token::LT => Self::LT,
            Token::GT => Self::GT,
            Token::LEq => Self::LEq,
            Token::GEq => Self::GEq,
            Token::In => Self::In,
            _ => unreachable!(),
        }
    }
}

pub(super) fn expr<'source, I>(
) -> impl Parser<'source, I, Expr<'source>, extra::Err<Rich<'source, Token<'source>>>>
where
    I: ValueInput<'source, Token = Token<'source>, Span = SimpleSpan>,
{
    recursive(|expr| {
        let var = recursive(|variable| {
            let ref_var = variable
                .clone()
                .then_ignore(tokes([Token::Caret, Token::UpArrow, Token::At]))
                .map(|var| Var::RefVar(Box::new(var)));
            let indexed_var = variable
                .clone()
                .then(
                    expr.clone()
                        .separated_by(just(Token::Comma))
                        .collect::<Vec<_>>()
                        .delimited_by(just(Token::LSquare), just(Token::RSquare)),
                )
                .map(|(ident, exprs)| Var::IndexedVar(Box::new(ident), exprs));
            let field_access = variable
                .then(just(Token::Dot).ignore_then(ident()))
                .map(|(ident, field)| Var::FieldAccess(Box::new(ident), field));
            let var = ident().map(Var::Plain);
            choice((ref_var, indexed_var, field_access, var))
        });

        let set = expr
            .clone()
            .separated_by(just(Token::Comma))
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LSquare), just(Token::RSquare))
            .map(Expr::Set);

        let func_designator = ident()
            .then(
                expr.clone()
                    .separated_by(just(Token::Comma))
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::LParen), just(Token::RParen)),
            )
            .map(|(name, args)| Expr::FuncCall(name, args));

        let factor = recursive(|factor| {
            choice((
                func_designator,
                var.map(Expr::Var),
                select! {
                    Token::IntLit(num) => Expr::IntLit(num),
                    Token::RealLit(num) => Expr::RealLit(num),
                    Token::StrLit(s) => Expr::StrLit(s),
                    Token::Nil => Expr::Nil,
                    Token::True => Expr::True,
                    Token::False => Expr::False,
                },
                set,
                expr.delimited_by(just(Token::LParen), just(Token::RParen)),
                just(Token::Not).ignore_then(factor),
            ))
        });

        let multiplicative_op = tokes([
            Token::Asterisk,
            Token::Slash,
            Token::Div,
            Token::Mod,
            Token::And,
        ]);
        let term = ops(multiplicative_op, factor);

        let additive_op = tokes([Token::Plus, Token::Minus, Token::Or]);
        let sign = choice((just(Token::Plus), just(Token::Minus)));
        let simple_expr =
            sign.or_not()
                .then(ops(additive_op, term))
                .map(|(sign, expr)| match sign {
                    Some(Token::Plus) => Expr::UnaryOp {
                        op: UnaryOp::Identity,
                        operand: Box::new(expr),
                    },
                    Some(Token::Minus) => Expr::UnaryOp {
                        op: UnaryOp::Negation,
                        operand: Box::new(expr),
                    },
                    None => expr,
                    _ => unreachable!(),
                });

        let relational_op = tokes([
            Token::Eq,
            Token::NEq,
            Token::LT,
            Token::LEq,
            Token::GT,
            Token::GEq,
            Token::In,
        ]);
        simple_expr
            .clone()
            .then(relational_op.then(simple_expr).or_not())
            .map(|(expr, rhs)| match rhs {
                Some((op, rhs)) => Expr::BinOp {
                    op: op.into(),
                    left: Box::new(expr),
                    right: Box::new(rhs),
                },
                None => expr,
            })
    })
}

fn ops<'source, I>(
    ops: impl Parser<'source, I, Token<'source>, extra::Err<Rich<'source, Token<'source>>>> + Clone,
    factor: impl Parser<'source, I, Expr<'source>, extra::Err<Rich<'source, Token<'source>>>> + Clone,
) -> impl Parser<'source, I, Expr<'source>, extra::Err<Rich<'source, Token<'source>>>> + Clone
where
    I: ValueInput<'source, Token = Token<'source>, Span = SimpleSpan>,
{
    factor
        .clone()
        .foldl(ops.then(factor).repeated(), |lhs, (op, rhs)| Expr::BinOp {
            op: op.into(),
            left: Box::new(lhs),
            right: Box::new(rhs),
        })
}
