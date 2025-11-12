use crate::{
    ast::{
        expr::Var,
        stmt::{Case, CompoundStmt, ForDirection, MaybeLabelledStmt, Stmt, WriteParam},
    },
    lexer::{parse_unsigned_integer, TokenKind},
    parser::expr::VAR_EXT_START,
};

use super::{
    expr::{expr, params, var, var_ext},
    program::constexpr,
    ParseResult, ParserState,
};

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

fn assign_or_proc_call<'source>(parser: &mut ParserState<'source>) -> ParseResult<Stmt<'source>> {
    let name = parser.advance_source();
    let peeked = parser.peek();
    match name.to_lowercase().as_str() {
        "writeln" if peeked != TokenKind::LParen => return Ok(Stmt::WritelnCall(Vec::new())),
        "readln" if peeked != TokenKind::LParen => return Ok(Stmt::ReadlnCall(Vec::new())),
        _ => (),
    }

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
        let stmt = match name.to_lowercase().as_str() {
            // We assume there must be at least one argument for writeln/readln as we're inside parens
            "write" => write_params(parser).map(Stmt::WriteCall),
            "writeln" => write_params(parser).map(Stmt::WritelnCall),
            "read" => read_params(parser).map(Stmt::ReadCall),
            "readln" => read_params(parser).map(Stmt::ReadlnCall),
            _ => params(parser).map(|params| Stmt::ProcCall { name, params }),
        }?;
        parser.expect(TokenKind::RParen)?;
        Ok(stmt)
    } else if peeked == TokenKind::Semicolon {
        Ok(Stmt::ProcCall {
            name,
            params: Vec::new(),
        })
    } else {
        parser.next_error("'^', '↑', '[', '.', ':=' or '('")
    }
}

fn write_params<'source>(
    parser: &mut ParserState<'source>,
) -> ParseResult<Vec<WriteParam<'source>>> {
    parser.repeat_sep(TokenKind::Comma, |parser: &mut ParserState<'source>| {
        let param = expr(parser)?;
        let specifiers = if parser.is(TokenKind::Colon) {
            parser.advance();
            Some((
                expr(parser)?,
                if parser.is(TokenKind::Colon) {
                    parser.advance();
                    Some(expr(parser)?)
                } else {
                    None
                },
            ))
        } else {
            None
        };
        Ok(WriteParam { param, specifiers })
    })
}

fn read_params<'source>(parser: &mut ParserState<'source>) -> ParseResult<Vec<Var<'source>>> {
    parser.repeat_sep(TokenKind::Comma, var)
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
