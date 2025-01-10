use chumsky::{error::Rich, extra, input::ValueInput, prelude::*, span::SimpleSpan, Parser};

use crate::lexer::Token;

use super::{ident, tokes};

#[derive(Debug, Clone)]
pub enum Expr<'source> {
    Variable(Variable<'source>),
    Nil,
    True,
    False,
    IntLit(i64),
    RealLit(f64),
    StrLit(&'source str),
    Set(Vec<Expr<'source>>),
    FunctionCall(&'source str, Vec<Expr<'source>>),
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

#[derive(Debug, Clone)]
pub enum Variable<'source> {
    Plain(&'source str),
    ReferencedVariable(Box<Variable<'source>>),
    IndexedVariable(Box<Variable<'source>>, Vec<Expr<'source>>),
    FieldAccess(Box<Variable<'source>>, &'source str),
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

impl<'source> From<Token<'source>> for BinaryOp {
    fn from(value: Token<'source>) -> Self {
        match value {
            Token::Asterisk => Self::Multiply,
            Token::Slash => Self::Divide,
            Token::Mod => Self::Modulo,
            Token::And => Self::And,
            Token::Plus => Self::Add,
            Token::Minus => Self::Subtract,
            Token::Or => Self::Or,
            Token::Equals => Self::Equals,
            Token::NotEqual => Self::NotEquals,
            Token::LessThan => Self::LessThan,
            Token::GreaterThan => Self::GreaterThan,
            Token::LessOrEqual => Self::LessEquals,
            Token::GreaterOrEqual => Self::GreaterEquals,
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
        let variable = recursive(|variable| {
            let referenced_variable = variable
                .clone()
                .then_ignore(tokes([Token::Caret, Token::UpArrow, Token::At]))
                .map(|var| Variable::ReferencedVariable(Box::new(var)));
            let indexed_variable = variable
                .clone()
                .then(
                    expr.clone()
                        .separated_by(just(Token::Comma))
                        .collect::<Vec<_>>()
                        .delimited_by(just(Token::LeftSquare), just(Token::RightSquare)),
                )
                .map(|(ident, exprs)| Variable::IndexedVariable(Box::new(ident), exprs));
            let field_access = variable
                .then(just(Token::Period).ignore_then(ident()))
                .map(|(ident, field)| Variable::FieldAccess(Box::new(ident), field));
            let variable = ident().map(Variable::Plain);
            choice((
                referenced_variable,
                indexed_variable,
                field_access,
                variable,
            ))
        });

        let set = expr
            .clone()
            .separated_by(just(Token::Comma))
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LeftSquare), just(Token::RightSquare))
            .map(Expr::Set);

        let function_designator = ident()
            .then(
                expr.clone()
                    .separated_by(just(Token::Comma))
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::LeftParen), just(Token::RightParen)),
            )
            .map(|(name, args)| Expr::FunctionCall(name, args));

        let factor = recursive(|factor| {
            choice((
                function_designator,
                variable.map(Expr::Variable),
                select! {
                    Token::IntegerLiteral(num) => Expr::IntLit(num),
                    Token::RealLiteral(num) => Expr::RealLit(num),
                    Token::StringLiteral(s) => Expr::StrLit(s),
                    Token::Nil => Expr::Nil,
                    Token::True => Expr::True,
                    Token::False => Expr::False,
                },
                set,
                expr.delimited_by(just(Token::LeftParen), just(Token::RightParen)),
                just(Token::Not).ignore_then(factor),
            ))
        });

        let multiplicative_operator = tokes([
            Token::Asterisk,
            Token::Slash,
            Token::Div,
            Token::Mod,
            Token::And,
        ]);
        let term = operations(multiplicative_operator, factor);

        let additive_operator = tokes([Token::Plus, Token::Minus, Token::Or]);
        let sign = choice((just(Token::Plus), just(Token::Minus)));
        let simple_expression = sign.or_not().then(operations(additive_operator, term)).map(
            |(sign, expr)| match sign {
                Some(Token::Plus) => Expr::UnaryOperation {
                    op: UnaryOp::Identity,
                    operand: Box::new(expr),
                },
                Some(Token::Minus) => Expr::UnaryOperation {
                    op: UnaryOp::Negation,
                    operand: Box::new(expr),
                },
                None => expr,
                _ => unreachable!(),
            },
        );

        let relational_operator = tokes([
            Token::Equals,
            Token::NotEqual,
            Token::LessThan,
            Token::LessOrEqual,
            Token::GreaterThan,
            Token::GreaterOrEqual,
            Token::In,
        ]);
        simple_expression
            .clone()
            .then(relational_operator.then(simple_expression).or_not())
            .map(|(expr, rhs)| match rhs {
                Some((op, rhs)) => Expr::BinaryOperation {
                    op: op.into(),
                    left: Box::new(expr),
                    right: Box::new(rhs),
                },
                None => expr,
            })
    })
}

fn operations<'source, I>(
    operators: impl Parser<'source, I, Token<'source>, extra::Err<Rich<'source, Token<'source>>>>
        + Clone,
    factor: impl Parser<'source, I, Expr<'source>, extra::Err<Rich<'source, Token<'source>>>> + Clone,
) -> impl Parser<'source, I, Expr<'source>, extra::Err<Rich<'source, Token<'source>>>> + Clone
where
    I: ValueInput<'source, Token = Token<'source>, Span = SimpleSpan>,
{
    factor
        .clone()
        .foldl(operators.then(factor).repeated(), |lhs, (op, rhs)| {
            Expr::BinaryOperation {
                op: op.into(),
                left: Box::new(lhs),
                right: Box::new(rhs),
            }
        })
}
