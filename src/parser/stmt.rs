use crate::{
    ast::{
        expr::{SpanVar, Var},
        stmt::{
            Case, CompoundStmt, ForDirection, MaybeLabelledStmt, Stmt, WriteParam, WriteParamSpecs,
        },
    },
    lexer::{parse_unsigned_integer, TokenKind},
    parser::{add_span_opt, empty_list, expr::VAR_EXT_START, Builtin, ParseResult},
    utils::Spanned,
};

use super::{
    expr::{expr, params, var, var_ext},
    program::constexpr,
    ParserState, SpanParseResult,
};

pub fn compound_stmt<'source>(parser: &mut ParserState<'source>) -> SpanParseResult<CompoundStmt> {
    let span_start = parser.expect(TokenKind::Begin)?.span;
    let stmts = stmt_seq(parser)?;
    let span_end = parser.expect(TokenKind::End)?.span;
    Ok(Spanned {
        span: span_start + span_end,
        node: stmts,
    })
}

fn stmt_seq<'source>(
    parser: &mut ParserState<'source>,
) -> ParseResult<Vec<Spanned<MaybeLabelledStmt>>> {
    parser.repeat_sep(TokenKind::Semicolon, maybe_labelled_stmt)
}

fn maybe_labelled_stmt<'source>(
    parser: &mut ParserState<'source>,
) -> SpanParseResult<MaybeLabelledStmt> {
    let label = if parser.is(TokenKind::UIntLit) {
        let num = parser.advance_source().map(parse_unsigned_integer);
        parser.expect(TokenKind::Colon)?;
        Some(num)
    } else {
        None
    };
    let stmt = Box::new(stmt(parser)?);

    Ok(Spanned {
        span: if let Some(label) = label {
            label.span + stmt.span
        } else {
            stmt.span
        },
        node: MaybeLabelledStmt { label, stmt },
    })
}

fn stmt<'source>(parser: &mut ParserState<'source>) -> SpanParseResult<Stmt> {
    match parser.peek() {
        TokenKind::Ident => assign_or_proc_call(parser),
        TokenKind::Goto => goto(parser),
        TokenKind::Begin => compound_stmt(parser).map(|stmt| stmt.map(Stmt::Compound)),
        TokenKind::If => r#if(parser),
        TokenKind::Case => case(parser),
        TokenKind::While => r#while(parser),
        TokenKind::Repeat => repeat(parser),
        TokenKind::For => r#for(parser),
        TokenKind::With => with(parser),
        TokenKind::Semicolon | TokenKind::End | TokenKind::Until => Ok(Spanned {
            span: (0..0).into(), // nonsense span cus this should hopefully never be accessed
            node: Stmt::Empty,
        }),
        _ => parser.next_error("'label' or statement"),
    }
}

fn assign_or_proc_call<'source>(parser: &mut ParserState<'source>) -> SpanParseResult<Stmt> {
    let name = parser.advance_ident();
    let peeked = parser.peek();

    if name.node == parser.builtin_name(Builtin::Writeln) {
        return Ok(Spanned {
            span: name.span,
            node: Stmt::WritelnCall(empty_list()),
        });
    } else if name.node == parser.builtin_name(Builtin::Readln) {
        return Ok(Spanned {
            span: name.span,
            node: Stmt::ReadlnCall(empty_list()),
        });
    }

    if peeked == TokenKind::Becomes {
        let var = name.map(Var::Plain);
        parser.advance();
        let value = expr(parser)?;
        Ok(Spanned {
            span: var.span + value.span,
            node: Stmt::Assign { var, value },
        })
    } else if peeked == TokenKind::LParen {
        match parser.rodeo.resolve(&name.node) {
            // We assume there must be at least one argument for writeln/readln as we're inside parens
            "write" => write_params(parser).map(|params| Spanned {
                span: name.span + params.span,
                node: Stmt::WriteCall(params),
            }),
            "writeln" => write_params(parser).map(|params| Spanned {
                span: name.span + params.span,
                node: Stmt::WritelnCall(params),
            }),
            "read" => read_params(parser).map(|params| Spanned {
                span: name.span + params.span,
                node: Stmt::ReadCall(params),
            }),
            "readln" => read_params(parser).map(|params| Spanned {
                span: name.span + params.span,
                node: Stmt::ReadCall(params),
            }),
            _ => params(parser).map(|params| Spanned {
                span: name.span + params.span,
                node: Stmt::ProcCall { name, params },
            }),
        }
    } else if peeked == TokenKind::Semicolon {
        Ok(Spanned {
            span: (0..0).into(),
            node: Stmt::Empty,
        })
    } else if VAR_EXT_START.contains(&peeked) {
        let var = parser.repeat_fold(VAR_EXT_START, var_ext, |_| Ok(name.map(Var::Plain)))?;
        parser.expect(TokenKind::Becomes)?;
        let value = expr(parser)?;
        Ok(Spanned {
            span: var.span + value.span,
            node: Stmt::Assign { var, value },
        })
    } else {
        parser.next_error("'^', '↑', '[', '.', ':=' or '('")
    }
}

fn write_params<'source>(
    parser: &mut ParserState<'source>,
) -> SpanParseResult<Vec<Spanned<WriteParam>>> {
    let span_start = parser.advance().span;
    let params = parser.repeat_sep(TokenKind::Comma, |parser: &mut ParserState<'source>| {
        let param = expr(parser)?;
        let specifiers = if parser.is(TokenKind::Colon) {
            let colon_span = parser.advance().span;
            let field_width = expr(parser)?;
            let frac_digits = if parser.is(TokenKind::Colon) {
                Some((parser.advance().span, expr(parser)?))
            } else {
                None
            };
            Some(Spanned {
                span: add_span_opt(colon_span, frac_digits.as_ref().map(|(span, _)| *span)),
                node: WriteParamSpecs {
                    colon_span,
                    field_width,
                    frac_digits,
                },
            })
        } else {
            None
        };
        let span = if let Some(ref specs) = specifiers {
            param.span + specs.span
        } else {
            param.span
        };
        Ok(Spanned {
            span,
            node: WriteParam { param, specifiers },
        })
    })?;
    let span_end = parser.expect(TokenKind::RParen)?.span;
    Ok(Spanned {
        span: span_start + span_end,
        node: params,
    })
}

fn read_params<'source>(parser: &mut ParserState<'source>) -> SpanParseResult<Vec<SpanVar>> {
    parser.repeat_sep(TokenKind::Comma, var)
}

fn goto<'source>(parser: &mut ParserState<'source>) -> SpanParseResult<Stmt> {
    parser.advance();
    let label = parser
        .expect_source(TokenKind::UIntLit)
        .map(parse_unsigned_integer)?;
    Ok(Stmt::Goto(label))
}

fn r#if<'source>(parser: &mut ParserState<'source>) -> SpanParseResult<Stmt> {
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

fn case<'source>(parser: &mut ParserState<'source>) -> SpanParseResult<Stmt> {
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

fn r#while<'source>(parser: &mut ParserState<'source>) -> SpanParseResult<Stmt> {
    parser.advance();
    let cond = expr(parser)?;
    parser.expect(TokenKind::Do)?;
    let body = maybe_labelled_stmt(parser)?;
    Ok(Stmt::While { cond, body })
}

fn repeat<'source>(parser: &mut ParserState<'source>) -> SpanParseResult<Stmt> {
    parser.advance();
    let body = stmt_seq(parser)?;
    parser.expect(TokenKind::Until)?;
    let cond = expr(parser)?;
    Ok(Stmt::Repeat { body, cond })
}

fn r#for<'source>(parser: &mut ParserState<'source>) -> SpanParseResult<Stmt> {
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

fn with<'source>(parser: &mut ParserState<'source>) -> SpanParseResult<Stmt> {
    parser.advance();
    let vars = parser.repeat_sep(TokenKind::Comma, var)?;
    parser.expect(TokenKind::Do)?;
    let body = maybe_labelled_stmt(parser)?;
    Ok(Stmt::With { vars, body })
}
