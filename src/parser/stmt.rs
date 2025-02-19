use crate::lexer::{parse_unsigned_integer, TokenKind};

use super::{
    expr::{Expr, Var},
    ParseResult, ParserState,
};

pub type CompoundStmt<'source> = Vec<MaybeLabelledStmt<'source>>;

pub fn compound_stmt<'source>(
    parser: &mut ParserState<'source>,
) -> ParseResult<CompoundStmt<'source>> {
    parser.expect(TokenKind::Begin)?;
    let mut stmts = Vec::new();
    while parser.is(TokenKind::End) {
        stmts.push(maybe_labelled_stmt(parser)?);
    }
    parser.expect(TokenKind::End)?;
    Ok(stmts)
}

#[derive(Debug, Clone)]
pub struct MaybeLabelledStmt<'source> {
    label: Option<u64>,
    stmt: Box<Stmt<'source>>,
}

fn maybe_labelled_stmt<'source>(
    parser: &mut ParserState<'source>,
) -> ParseResult<MaybeLabelledStmt<'source>> {
    let label = if parser.is(TokenKind::UIntLit) {
        let num = parse_unsigned_integer(parser.advance_source());
        parser.expect(TokenKind::Colon)?;
        Some(num)
    } else {
        None
    };
    let stmt = Box::new(stmt(parser)?);

    Ok(MaybeLabelledStmt { label, stmt })
}

#[derive(Debug, Clone)]
pub enum Stmt<'source> {
    Empty,
    Assign {
        var: Var<'source>,
        value: Expr<'source>,
    },
    ProcCall {
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

fn stmt<'source>(parser: &mut ParserState<'source>) -> ParseResult<Stmt<'source>> {
    match parser.peek() {
        TokenKind::Ident => assign_or_proc_call(parser),
        TokenKind::Goto => goto(parser),
        TokenKind::Begin => compound_stmt(parser).map(Stmt::Compound),
        TokenKind::If => r#if(parser),
        TokenKind::Case => case(parser),
        TokenKind::While => r#while(parser),
        TokenKind::Repeat => repeat(parser),
        TokenKind::For => r#for(parser),
        TokenKind::With => with(parser),
        TokenKind::Semicolon | TokenKind::End | TokenKind::Until => Ok(Stmt::Empty),
        _ => parser.next_error("label or statement"),
    }
}

fn assign_or_proc_call<'source>(parser: &mut ParserState<'source>) -> ParseResult<Stmt<'source>> {
    todo!()
}

fn goto<'source>(parser: &mut ParserState<'source>) -> ParseResult<Stmt<'source>> {
    todo!()
}

fn r#if<'source>(parser: &mut ParserState<'source>) -> ParseResult<Stmt<'source>> {
    todo!()
}

fn case<'source>(parser: &mut ParserState<'source>) -> ParseResult<Stmt<'source>> {
    todo!()
}

fn r#while<'source>(parser: &mut ParserState<'source>) -> ParseResult<Stmt<'source>> {
    todo!()
}

fn repeat<'source>(parser: &mut ParserState<'source>) -> ParseResult<Stmt<'source>> {
    todo!()
}

fn r#for<'source>(parser: &mut ParserState<'source>) -> ParseResult<Stmt<'source>> {
    todo!()
}

fn with<'source>(parser: &mut ParserState<'source>) -> ParseResult<Stmt<'source>> {
    todo!()
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
