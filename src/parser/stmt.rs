use chumsky::{error::Simple, prelude::*, Parser};

use crate::lexer::Token;

use super::{
    expr::{expr, var, Expr, Var},
    ident,
    program::constexpr,
    ParserError,
};

#[derive(Debug, Clone)]
pub struct MaybeLabelledStmt<'source> {
    label: Option<u64>,
    stmt: Box<Stmt<'source>>,
}

#[derive(Debug, Clone)]
pub enum Stmt<'source> {
    Empty,
    Assign {
        var: Var<'source>,
        value: Expr<'source>,
    },
    ProcStmt {
        name: &'source str,
        params: Params<'source>,
    },
    Goto(u64),
    Compound(CompoundStmt<'source>),
    If {
        cond: Expr<'source>,
        then: MaybeLabelledStmt<'source>,
        r#else: Option<MaybeLabelledStmt<'source>>,
    },
    Case {
        index: Expr<'source>,
        cases: Vec<Case<'source>>,
    },
    While {
        cond: Expr<'source>,
        body: MaybeLabelledStmt<'source>,
    },
    Repeat {
        body: Vec<MaybeLabelledStmt<'source>>,
        cond: Expr<'source>,
    },
    For {
        control_var: Var<'source>,
        from: Expr<'source>,
        direction: ForDirection,
        to: Expr<'source>,
        body: MaybeLabelledStmt<'source>,
    },
    With {
        vars: Vec<Var<'source>>,
        body: MaybeLabelledStmt<'source>,
    },
}

#[derive(Debug, Clone)]
pub enum ForDirection {
    To,
    DownTo,
}

#[derive(Debug, Clone)]
pub struct Case<'source> {
    labels: Vec<Expr<'source>>,
    body: MaybeLabelledStmt<'source>,
}

#[derive(Debug, Clone)]
pub enum Params<'source> {
    Actual(Vec<Expr<'source>>),
    Write(InitWriteParam<'source>, Vec<WriteParam<'source>>),
}

#[derive(Debug, Clone)]
pub enum InitWriteParam<'source> {
    FileVar(Var<'source>),
    WriteParam(WriteParam<'source>),
}

#[derive(Debug, Clone)]
pub struct WriteParam<'source> {
    expr: Expr<'source>,
    field_widths: Option<(Expr<'source>, Option<Expr<'source>>)>,
}

pub type CompoundStmt<'source> = Vec<MaybeLabelledStmt<'source>>;

pub(super) fn compound_stmt<'source>(
) -> impl Parser<Token<'source>, CompoundStmt<'source>, Error = ParserError<'source>> + Clone {
    stmt_seq().delimited_by(just(Token::Begin), just(Token::End))
}

pub(super) fn stmt_seq<'source>(
) -> impl Parser<Token<'source>, Vec<MaybeLabelledStmt<'source>>, Error = ParserError<'source>> + Clone
{
    stmt().separated_by(just(Token::Semicolon)).collect()
}

fn stmt<'source>(
) -> impl Parser<Token<'source>, MaybeLabelledStmt<'source>, Error = ParserError<'source>> + Clone {
    let label = select! { Token::UIntLit(label) => label };

    let assign = var()
        .then_ignore(just(Token::Becomes))
        .then(expr())
        .map(|(var, value)| Stmt::Assign { var, value });

    let width_spec = just(Token::Colon).ignore_then(expr(/* integer expr */));
    let write_param = expr()
        .then(width_spec.clone().then(width_spec.or_not()).or_not())
        .map(|(expr, field_widths)| WriteParam { expr, field_widths });
    let write_param_list = var(/* file var */)
        .map(InitWriteParam::FileVar)
        .or(write_param.clone().map(InitWriteParam::WriteParam))
        .then(
            just(Token::Comma)
                .ignore_then(write_param)
                .repeated()
                .collect::<Vec<_>>(),
        )
        .delimited_by(just(Token::LParen), just(Token::RParen))
        .map(|(init, rest)| Params::Write(init, rest));
    let actual_param_list = expr()
        .separated_by(just(Token::Comma))
        .at_least(1)
        .collect::<Vec<_>>()
        .delimited_by(just(Token::LParen), just(Token::RParen))
        .map(Params::Actual);

    let proc = ident()
        .then(write_param_list.or(actual_param_list))
        .map(|(name, params)| Stmt::ProcStmt { name, params });

    let goto = just(Token::Goto).ignore_then(label).map(Stmt::Goto);

    recursive(|stmt| {
        let r#if = just(Token::If)
            .ignore_then(expr(/* boolean expression */))
            .then_ignore(just(Token::Then))
            .then(stmt.clone())
            .then(just(Token::Else).ignore_then(stmt.clone()).or_not())
            .map(|((cond, then), r#else)| Stmt::If { cond, then, r#else });

        let case = constexpr()
            .separated_by(just(Token::Comma))
            .at_least(1)
            .collect::<Vec<_>>()
            .then_ignore(just(Token::Colon))
            .then(stmt.clone())
            .map(|(labels, body)| Case { labels, body });
        let case_list = case
            .separated_by(just(Token::Semicolon))
            .collect::<Vec<_>>()
            .then_ignore(just(Token::Semicolon).or_not());
        let case = just(Token::Case)
            .ignore_then(expr(/* ordinal expr */))
            .then_ignore(just(Token::Of))
            .then(case_list)
            .then_ignore(just(Token::End))
            .map(|(index, cases)| Stmt::Case { index, cases });

        let repeat = just(Token::Repeat)
            .ignore_then(stmt_seq())
            .then_ignore(just(Token::Until))
            .then(expr(/* boolean expression */))
            .map(|(body, cond)| Stmt::Repeat { body, cond });

        let r#while = just(Token::While)
            .ignore_then(expr(/* boolean expression */))
            .then_ignore(just(Token::Do))
            .then(stmt.clone())
            .map(|(cond, body)| Stmt::While { cond, body });

        let r#for = just(Token::For)
            .ignore_then(var(/* control variable */))
            .then_ignore(just(Token::Becomes))
            .then(expr(/* ordinal expression */))
            .then(
                just(Token::To)
                    .map(|_| ForDirection::To)
                    .or(just(Token::DownTo).map(|_| ForDirection::DownTo)),
            )
            .then(expr(/* ordinal expression */))
            .then_ignore(just(Token::Do))
            .then(stmt.clone())
            .map(|((((control_var, from), direction), to), body)| Stmt::For {
                control_var,
                from,
                direction,
                to,
                body,
            });

        let with = just(Token::With)
            .ignore_then(
                var(/* record variable */)
                    .separated_by(just(Token::Comma))
                    .at_least(1)
                    .collect::<Vec<_>>(),
            )
            .then_ignore(just(Token::Do))
            .then(stmt)
            .map(|(vars, body)| Stmt::With { vars, body });

        label
            .or_not()
            .then(
                choice((
                    // Simple Statements
                    empty().map(|()| Stmt::Empty),
                    assign,
                    proc,
                    goto,
                    // Structured Statements
                    compound_stmt().map(Stmt::Compound),
                    r#if,
                    case,
                    repeat,
                    r#while,
                    r#for,
                    with,
                ))
                .map(Box::new),
            )
            .map(|(label, stmt)| MaybeLabelledStmt { label, stmt })
    })
}
