use crate::{
    ast::expr::{Expr, Params, SpanVar, UnaryOp, Var},
    lexer::{parse_unsigned_integer, parse_unsigned_real, TokenKind},
    utils::{trim_ends, Spanned},
};

use super::{ParserState, SpanParseResult};

pub fn expr<'source>(parser: &mut ParserState<'source>) -> SpanParseResult<Expr> {
    parser.repeat_fold(
        &[
            TokenKind::Eq,
            TokenKind::NEq,
            TokenKind::LT,
            TokenKind::GT,
            TokenKind::LEq,
            TokenKind::GEq,
            TokenKind::In,
        ],
        |parser, left| {
            let left = Box::new(left);
            let op = parser.advance().node.into();
            let right = simple_expr(parser).map(Box::new)?;

            Ok(Spanned {
                span: (&left).span + right.span,
                node: Expr::BinOp { op, left, right },
            })
        },
        simple_expr,
    )
}

fn simple_expr<'source>(parser: &mut ParserState<'source>) -> SpanParseResult<Expr> {
    let sign: Option<UnaryOp> = match parser.peek() {
        TokenKind::Plus | TokenKind::Minus => Some(parser.advance().node.into()),
        _ => None,
    };
    parser.repeat_fold(
        &[TokenKind::Plus, TokenKind::Minus, TokenKind::Or],
        |parser, left| {
            let left = Box::new(left);
            let left_span = left.span;

            let op = parser.advance().node.into();

            let right = term(parser).map(Box::new)?;
            let right_span = right.span;

            let binop = Spanned {
                span: left_span + right_span,
                node: Expr::BinOp { op, left, right },
            };

            match sign {
                Some(op) => Ok(Spanned {
                    span: left_span + right_span,
                    node: Expr::UnaryOp {
                        op,
                        operand: Box::new(binop),
                    },
                }),
                None => Ok(binop),
            }
        },
        term,
    )
}

fn term<'source>(parser: &mut ParserState<'source>) -> SpanParseResult<Expr> {
    parser.repeat_fold(
        &[
            TokenKind::Asterisk,
            TokenKind::Slash,
            TokenKind::Div,
            TokenKind::Mod,
            TokenKind::And,
        ],
        |parser, left| {
            let left = Box::new(left);
            let op = parser.advance().node.into();
            let right = factor(parser).map(Box::new)?;

            Ok(Spanned {
                span: left.span + right.span,
                node: Expr::BinOp { op, left, right },
            })
        },
        factor,
    )
}

fn factor<'source>(parser: &mut ParserState<'source>) -> SpanParseResult<Expr> {
    match parser.peek() {
        TokenKind::UIntLit => Ok(parser
            .advance_source()
            .map(parse_unsigned_integer)
            .map(Expr::UIntLit)),
        TokenKind::URealLit => Ok(parser
            .advance_source()
            .map(parse_unsigned_real)
            .map(Expr::URealLit)),
        TokenKind::StrLit => Ok(parser
            .advance_source()
            .map(trim_ends)
            .map(|s| Expr::StrLit(s.to_string()))),
        TokenKind::Nil => Ok(parser.advance_source().map(|_| Expr::Nil)),
        TokenKind::Ident => factor_ident(parser),
        TokenKind::LSquare => {
            let start_span = parser.advance().span;
            let elems = parser.repeat_sep(TokenKind::Comma, expr)?;
            let end_span = parser.expect(TokenKind::RSquare)?.span;

            Ok(Spanned {
                span: start_span + end_span,
                node: Expr::Set(elems),
            })
        }
        TokenKind::Not => {
            let start_span = parser.advance().span;
            let operand = factor(parser).map(Box::new)?;

            Ok(Spanned {
                span: start_span + operand.span,
                node: Expr::UnaryOp {
                    op: UnaryOp::Not,
                    operand,
                },
            })
        }
        TokenKind::LParen => {
            let start_span = parser.advance().span;
            let expr = expr(parser)?;
            let end_span = parser.expect(TokenKind::RParen)?.span;

            Ok(Spanned {
                span: start_span + end_span,
                node: expr.node,
            })
        }
        _ => parser.next_error("factor"),
    }
}

pub(super) const VAR_EXT_START: &[TokenKind] = &[
    TokenKind::Caret,
    TokenKind::UpArrow,
    TokenKind::LSquare,
    TokenKind::Dot,
];

fn factor_ident<'source>(parser: &mut ParserState<'source>) -> SpanParseResult<Expr> {
    let ident = parser.advance_ident();
    Ok(if parser.is(VAR_EXT_START) {
        parser
            .repeat_fold(VAR_EXT_START, var_ext, |_| Ok(ident.map(Var::Plain)))?
            .map(Expr::Var)
    } else if parser.is(TokenKind::LParen) {
        let params = params(parser)?;
        Spanned {
            span: params.span,
            node: Expr::FuncCall {
                name: ident,
                params,
            },
        }
    } else {
        ident.map(Var::Plain).map(Expr::Var)
    })
}

pub(super) fn var<'source>(parser: &mut ParserState<'source>) -> SpanParseResult<Var> {
    parser.repeat_fold(VAR_EXT_START, var_ext, |parser| {
        Ok(parser.advance_ident().map(Var::Plain))
    })
}

pub(super) fn var_ext<'source>(
    parser: &mut ParserState<'source>,
    var: SpanVar,
) -> SpanParseResult<Var> {
    match parser.peek() {
        TokenKind::Caret | TokenKind::UpArrow => {
            let end_span = parser.advance().span;
            Ok(Spanned {
                span: var.span + end_span,
                node: Var::Ref(Box::new(var)),
            })
        }
        TokenKind::LSquare => {
            let index_start_span = parser.advance().span;
            let indices = parser.repeat_sep(TokenKind::Comma, expr)?;
            let index_end_span = parser.expect(TokenKind::RSquare)?.span;

            Ok(Spanned {
                span: var.span + index_end_span,
                node: Var::Indexed(
                    Box::new(var),
                    Spanned {
                        span: index_start_span + index_end_span,
                        node: indices,
                    },
                ),
            })
        }
        TokenKind::Dot => {
            let dot_span = parser.advance().span;
            let field = parser.ident()?;

            Ok(Spanned {
                span: var.span + field.span,
                node: Var::FieldAccess {
                    record: Box::new(var),
                    dot_span,
                    field,
                },
            })
        }
        _ => parser.next_error("'^', '↑', '[' or '.'"),
    }
}

pub(super) fn params<'source>(parser: &mut ParserState<'source>) -> SpanParseResult<Params> {
    let start_span = parser.advance().span;
    let params = parser.repeat_sep(TokenKind::Comma, expr)?;
    let end_span = parser.expect(TokenKind::RParen)?.span;

    Ok(Spanned {
        span: start_span + end_span,
        node: params,
    })
}
