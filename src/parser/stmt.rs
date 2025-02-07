use chumsky::{error::Rich, extra, input::ValueInput, prelude::*, span::SimpleSpan, Parser};

use crate::lexer::Token;

use super::expr::{Expr, Var};

#[derive(Debug, Clone)]
pub struct MaybeLabelledStmt<'source> {
    label: Option<u64>,
    stmt: Box<Stmt<'source>>,
}

#[derive(Debug, Clone)]
pub enum Stmt<'source> {
    Empty,
    Assign {
        name: &'source str,
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
    For {
        control_var: &'source str,
        from: Expr<'source>,
        direction: ForDirection,
        to: Expr<'source>,
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
    Write(Vec<WriteParam<'source>>),
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

pub(super) fn compound_stmt<'source, I>(
) -> impl Parser<'source, I, CompoundStmt<'source>, extra::Err<Rich<'source, Token<'source>>>> + Clone
where
    I: ValueInput<'source, Token = Token<'source>, Span = SimpleSpan>,
{
    stmt()
        .separated_by(just(Token::Semicolon))
        .collect()
        .delimited_by(just(Token::Begin), just(Token::End))
}

fn stmt<'source, I>(
) -> impl Parser<'source, I, MaybeLabelledStmt<'source>, extra::Err<Rich<'source, Token<'source>>>> + Clone
where
    I: ValueInput<'source, Token = Token<'source>, Span = SimpleSpan>,
{
    let assign = todo();
    let proc = todo();
    select! { Token::UIntLit(label) => label }
        .or_not()
        .then(choice((empty().map(|()| Stmt::Empty), assign, proc)))
        .map(|(label, stmt)| MaybeLabelledStmt { label, stmt })
}
