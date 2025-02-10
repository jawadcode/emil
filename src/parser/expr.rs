use chumsky::{error::Simple, prelude::*, Parser};

use crate::lexer::{parse_real, Token};

use super::{ident, tokes, ParserError};

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
    Ref(Box<Var<'source>>),
    Indexed(Box<Var<'source>>, Vec<Expr<'source>>),
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

pub(super) fn expr<'source>(
) -> impl Parser<Token<'source>, Expr<'source>, Error = ParserError<'source>> + Clone {
    recursive(|expr| {
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
                var().map(Expr::Var),
                select! {
                    Token::IntLit(num) => Expr::IntLit(num),
                    Token::RealLit(num) => Expr::RealLit(parse_real(num)),
                    Token::StrLit(s) => Expr::StrLit(s),
                    Token::Nil => Expr::Nil,
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

enum VarExt<'source> {
    Field(&'source str),
    Ref,
    Index(Vec<Expr<'source>>),
}

pub(super) fn var<'source>(
) -> impl Parser<Token<'source>, Var<'source>, Error = ParserError<'source>> + Clone {
    ident()
        .map(Var::Plain)
        .then(
            choice((
                just(Token::Dot).ignore_then(ident()).map(VarExt::Field),
                tokes([Token::UpArrow, Token::Caret]).map(|_| VarExt::Ref),
                just(Token::LSquare)
                    .ignore_then(
                        expr()
                            .separated_by(just(Token::Comma))
                            .at_least(1)
                            .collect::<Vec<_>>(),
                    )
                    .map(VarExt::Index),
            ))
            .repeated(),
        )
        .foldl(|var, ext| match ext {
            VarExt::Field(field) => Var::FieldAccess(Box::new(var), field),
            VarExt::Ref => Var::Ref(Box::new(var)),
            VarExt::Index(vec) => Var::Indexed(Box::new(var), vec),
        })
}

fn ops<'source>(
    ops: impl Parser<Token<'source>, Token<'source>, Error = ParserError<'source>> + Clone,
    factor: impl Parser<Token<'source>, Expr<'source>, Error = ParserError<'source>> + Clone,
) -> impl Parser<Token<'source>, Expr<'source>, Error = ParserError<'source>> + Clone {
    factor
        .clone()
        .then(ops.then(factor).repeated())
        .foldl(|lhs, (op, rhs)| Expr::BinOp {
            op: op.into(),
            left: Box::new(lhs),
            right: Box::new(rhs),
        })
}
