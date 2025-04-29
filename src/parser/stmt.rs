use crate::{
    lexer::{parse_unsigned_integer, TokenKind},
    parser::expr::VAR_EXT_START,
};

use super::{
    expr::{expr, params, var, var_ext, Expr, Params, Var},
    program::constexpr,
    ParseResult, ParserState,
};

pub type CompoundStmt<'source> = Vec<MaybeLabelledStmt<'source>>;

pub fn compound_stmt<'source>(
    parser: &mut ParserState<'source>,
) -> ParseResult<CompoundStmt<'source>> {
    parser.expect(TokenKind::Begin)?;
    let stmts = stmt_seq(parser)?;
    parser.expect(TokenKind::End)?;
    Ok(stmts)
}

fn stmt_seq<'source>(
    parser: &mut ParserState<'source>,
) -> ParseResult<Vec<MaybeLabelledStmt<'source>>> {
    parser.repeat_sep(TokenKind::Semicolon, maybe_labelled_stmt)
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
        control_var: &'source str,
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
        _ => parser.next_error("'label' or statement"),
    }
}

// TODO: Implement write (and read??) parameters
fn assign_or_proc_call<'source>(parser: &mut ParserState<'source>) -> ParseResult<Stmt<'source>> {
    let name = parser.advance_source();
    let peeked = parser.peek();
    if VAR_EXT_START.contains(&peeked) {
        let var = parser.repeat_fold(VAR_EXT_START, var_ext, |_| Ok(Var::Plain(name)))?;
        parser.expect(TokenKind::Becomes)?;
        let value = expr(parser)?;
        Ok(Stmt::Assign { var, value })
    } else if peeked == TokenKind::Becomes {
        let var = Var::Plain(name);
        parser.advance();
        let value = expr(parser)?;
        Ok(Stmt::Assign { var, value })
    } else if peeked == TokenKind::LParen {
        let params = params(parser)?;
        Ok(Stmt::ProcCall { name, params })
    } else if peeked == TokenKind::Semicolon {
        Ok(Stmt::ProcCall {
            name,
            params: Params::MaybeActual(Vec::new()),
        })
    } else {
        parser.next_error("'^', '↑', '[', '.', ':=' or '('")
    }
}

fn goto<'source>(parser: &mut ParserState<'source>) -> ParseResult<Stmt<'source>> {
    parser.advance();
    let label = parser
        .expect_source(TokenKind::UIntLit)
        .map(parse_unsigned_integer)?;
    Ok(Stmt::Goto(label))
}

fn r#if<'source>(parser: &mut ParserState<'source>) -> ParseResult<Stmt<'source>> {
    parser.advance();
    let cond = expr(parser)?;
    parser.expect(TokenKind::Then)?;
    let then = maybe_labelled_stmt(parser)?;
    let r#else = if parser.is(TokenKind::Else) {
        parser.advance();
        Some(maybe_labelled_stmt(parser)?)
    } else {
        None
    };
    Ok(Stmt::If { cond, then, r#else })
}

fn case<'source>(parser: &mut ParserState<'source>) -> ParseResult<Stmt<'source>> {
    parser.advance();
    let index = expr(parser)?;
    parser.expect(TokenKind::Of)?;
    let cases = parser.repeat_sep(TokenKind::Semicolon, |parser| {
        let labels = parser.repeat_sep(TokenKind::Comma, constexpr)?;
        parser.expect(TokenKind::Colon)?;
        let body = maybe_labelled_stmt(parser)?;
        Ok(Case { labels, body })
    })?;
    if parser.is(TokenKind::Semicolon) {
        parser.advance();
    }
    parser.expect(TokenKind::End)?;
    Ok(Stmt::Case { index, cases })
}

fn r#while<'source>(parser: &mut ParserState<'source>) -> ParseResult<Stmt<'source>> {
    parser.advance();
    let cond = expr(parser)?;
    parser.expect(TokenKind::Do)?;
    let body = maybe_labelled_stmt(parser)?;
    Ok(Stmt::While { cond, body })
}

fn repeat<'source>(parser: &mut ParserState<'source>) -> ParseResult<Stmt<'source>> {
    parser.advance();
    let body = stmt_seq(parser)?;
    parser.expect(TokenKind::Until)?;
    let cond = expr(parser)?;
    Ok(Stmt::Repeat { body, cond })
}

fn r#for<'source>(parser: &mut ParserState<'source>) -> ParseResult<Stmt<'source>> {
    parser.advance();
    let control_var = parser.expect_source(TokenKind::Ident)?;
    parser.expect(TokenKind::Becomes)?;
    let from = expr(parser)?;
    let direction = match parser.peek() {
        TokenKind::To => ForDirection::To,
        TokenKind::DownTo => ForDirection::DownTo,
        _ => return parser.next_error("'to' or 'downto'"),
    };
    parser.advance();
    let to = expr(parser)?;
    parser.expect(TokenKind::Do)?;
    let body = maybe_labelled_stmt(parser)?;
    Ok(Stmt::For {
        control_var,
        from,
        direction,
        to,
        body,
    })
}

fn with<'source>(parser: &mut ParserState<'source>) -> ParseResult<Stmt<'source>> {
    parser.advance();
    let vars = parser.repeat_sep(TokenKind::Comma, var)?;
    parser.expect(TokenKind::Do)?;
    let body = maybe_labelled_stmt(parser)?;
    Ok(Stmt::With { vars, body })
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
