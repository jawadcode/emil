use crate::{
    ast::{
        expr::{Expr, UnaryOp, Var},
        program::{
            ArraySchema, Block, ConstDef, FieldList, FuncDecl, FuncSig, IndexTypeSpec, OrdinalType,
            Param, ParamType, PostSig, ProcDecl, ProcSig, Program, RoutineDecl, Type, TypeDef,
            UnpackedStructuredType, VarDecl, Variant, VariantField,
        },
        Ident,
    },
    lexer::{parse_unsigned_integer, parse_unsigned_real, TokenKind},
    parser::empty_list,
    utils::{trim_ends, Spanned},
};

use super::{stmt::compound_stmt, ParserState, SpanParseResult};

pub fn program<'source>(parser: &mut ParserState<'source>) -> SpanParseResult<Program> {
    let start_span = parser.expect(TokenKind::Program)?.span;
    let name = parser.ident()?;
    let params = if parser.is(TokenKind::LParen) {
        parser.advance();
        let idents = ident_list(parser)?;
        parser.expect(TokenKind::RParen)?;
        idents
    } else {
        empty_list()
    };
    parser.expect(TokenKind::Semicolon)?;
    let block = block(parser)?;
    let end_span = parser.expect(TokenKind::Dot)?.span;

    Ok(Spanned {
        span: start_span + end_span,
        node: Program {
            name,
            params,
            block,
        },
    })
}

fn block<'source>(parser: &mut ParserState<'source>) -> SpanParseResult<Block> {
    let label_decls = if parser.is(TokenKind::Label) {
        let span_start = parser.advance().span;
        let res = parser.repeat_sep(TokenKind::Comma, |parser| {
            parser
                .expect_source(TokenKind::UIntLit)
                .map(|num| num.map(parse_unsigned_integer))
        })?;
        let span_end = parser.expect(TokenKind::Semicolon)?.span;
        Spanned {
            span: span_start + span_end,
            node: res,
        }
    } else {
        empty_list()
    };

    let const_defs = if parser.is(TokenKind::Const) {
        parser.advance();
        parser.repeated(TokenKind::Ident, |parser| {
            let name = parser.advance_ident();
            parser.expect(TokenKind::Eq)?;
            let value = constexpr(parser)?;
            let span_end = parser.expect(TokenKind::Semicolon)?.span;
            Ok(Spanned {
                span: name.span + span_end,
                node: ConstDef { name, value },
            })
        })?
    } else {
        empty_list()
    };

    let type_defs = if parser.is(TokenKind::Type) {
        parser.advance();
        parser.repeated(TokenKind::Ident, |parser| {
            let name = parser.advance_source();
            parser.expect(TokenKind::Eq)?;
            let def = r#type(parser)?;
            let span_end = parser.expect(TokenKind::Semicolon)?.span;
            Ok(Spanned {
                span: name.span + span_end,
                node: TypeDef { name, def },
            })
        })?
    } else {
        empty_list()
    };

    let var_decls = if parser.is(TokenKind::Var) {
        parser.advance();
        parser.repeated(TokenKind::Ident, |parser| {
            let names = ident_list(parser)?;
            parser.expect(TokenKind::Colon)?;
            let r#type = r#type(parser)?;
            let span_end = parser.expect(TokenKind::Semicolon)?.span;
            // Ok((names, r#type))
            Ok(Spanned {
                span: names.span + span_end,
                node: VarDecl { names, r#type },
            })
        })?
    } else {
        empty_list()
    };

    let routine_decls = routine_decls(parser)?;

    let stmts = compound_stmt(parser)?;

    Ok(Spanned {
        span: label_decls.span + stmts.span,
        node: Block {
            label_decls,
            const_defs,
            type_defs,
            var_decls,
            routine_decls,
            stmts,
        },
    })
}

fn r#type<'source>(parser: &mut ParserState<'source>) -> SpanParseResult<Type<'source>> {
    match parser.peek() {
        TokenKind::Ident => Ok(Type::Ordinal(OrdinalType::Identifier(
            parser.advance_source(),
        ))),
        TokenKind::Packed => {
            parser.advance();
            unpacked_structured_type(parser).map(|ty| Type::Structured {
                packed: true,
                r#type: Box::new(ty),
            })
        }
        TokenKind::Array | TokenKind::Record | TokenKind::Set | TokenKind::File => {
            unpacked_structured_type(parser).map(|ty| Type::Structured {
                packed: false,
                r#type: Box::new(ty),
            })
        }
        TokenKind::Caret | TokenKind::UpArrow => {
            parser.advance();
            let ty = parser.expect_source(TokenKind::Ident)?;
            Ok(Type::Pointer(ty))
        }
        _ => parser.next_error("type"),
    }
}

fn ordinal_type<'source>(
    parser: &mut ParserState<'source>,
) -> SpanParseResult<OrdinalType<'source>> {
    match parser.peek() {
        TokenKind::Ident => {
            let ident = parser.advance_source();
            if parser.is(TokenKind::Ellipsis) {
                parser.advance();
                let upper = constexpr(parser)?;
                Ok(OrdinalType::Subrange {
                    lower: Expr::Var(Var::Plain(ident)),
                    upper,
                })
            } else {
                Ok(OrdinalType::Identifier(ident))
            }
        }
        TokenKind::Plus
        | TokenKind::Minus
        | TokenKind::UIntLit
        | TokenKind::URealLit
        | TokenKind::StrLit => subrange_type(parser),
        TokenKind::LParen => {
            parser.advance();
            let values = ident_list(parser)?;
            parser.expect(TokenKind::RParen)?;
            Ok(OrdinalType::Enumerated(values))
        }
        _ => parser.next_error("ordinal type"),
    }
}

fn subrange_type<'source>(
    parser: &mut ParserState<'source>,
) -> SpanParseResult<OrdinalType<'source>> {
    let lower = constexpr(parser)?;
    parser.expect(TokenKind::Ellipsis)?;
    let upper = constexpr(parser)?;
    Ok(OrdinalType::Subrange { lower, upper })
}

fn unpacked_structured_type<'source>(
    parser: &mut ParserState<'source>,
) -> SpanParseResult<UnpackedStructuredType<'source>> {
    match parser.peek() {
        TokenKind::Array => {
            parser.advance();
            parser.expect(TokenKind::LSquare)?;
            let indices = parser.repeat_sep(TokenKind::Comma, ordinal_type)?;
            parser.expect(TokenKind::RSquare)?;
            parser.expect(TokenKind::Of)?;
            let elem = r#type(parser)?;
            Ok(UnpackedStructuredType::Array { indices, elem })
        }
        TokenKind::Record => {
            parser.advance();
            let field_list = field_list(parser)?;
            parser.expect(TokenKind::End)?;
            Ok(UnpackedStructuredType::Record(field_list))
        }
        TokenKind::Set => {
            parser.advance();
            parser.expect(TokenKind::Of)?;
            let elem = ordinal_type(parser)?;
            Ok(UnpackedStructuredType::Set(elem))
        }
        TokenKind::File => {
            parser.advance();
            parser.expect(TokenKind::Of)?;
            let of = r#type(parser)?;
            Ok(UnpackedStructuredType::File(of))
        }
        _ => parser.next_error("'array', 'record', 'set' or 'file'"),
    }
}

fn field_list<'source>(parser: &mut ParserState<'source>) -> SpanParseResult<FieldList<'source>> {
    let body = match parser.peek() {
        TokenKind::Ident => {
            let fixed = parser.repeat_sep(TokenKind::Semicolon, |parser| {
                let names = ident_list(parser)?;
                parser.expect(TokenKind::Semicolon)?;
                let field_type = r#type(parser)?;
                Ok((names, field_type))
            })?;

            let variant = if parser.is(TokenKind::Semicolon) {
                parser.expect(TokenKind::Case)?;
                Some(variant_field(parser)?)
            } else {
                None
            };

            match variant {
                Some(variant) => Ok(FieldList::Both(fixed, variant)),
                None => Ok(FieldList::FixedOnly(fixed)),
            }
        }
        TokenKind::Case => variant_field(parser).map(FieldList::VariantOnly),
        TokenKind::End => Ok(FieldList::Empty),
        _ => parser.next_error("field list"),
    }?;
    if parser.is(TokenKind::Semicolon) {
        parser.advance();
    }
    Ok(body)
}

fn variant_field<'source>(
    parser: &mut ParserState<'source>,
) -> SpanParseResult<VariantField<'source>> {
    parser.advance();
    let tag_field = if parser.is(TokenKind::Ident) {
        let ident = parser.advance_source();
        parser.expect(TokenKind::Colon)?;
        Some(ident)
    } else {
        None
    };
    let tag_type = parser.expect_source(TokenKind::Ident)?;
    parser.expect(TokenKind::Of)?;
    let variants = parser.repeat_sep(TokenKind::Semicolon, |parser| {
        let case_labels = parser.repeat_sep(TokenKind::Comma, constexpr)?;
        parser.expect(TokenKind::Colon)?;
        let fields = field_list(parser)?;
        Ok(Variant {
            case_labels,
            fields,
        })
    })?;
    Ok(VariantField {
        tag_field,
        tag_type,
        variants,
    })
}

fn routine_decls<'source>(
    parser: &mut ParserState<'source>,
) -> SpanParseResult<Vec<RoutineDecl<'source>>> {
    parser.repeated(&[TokenKind::Procedure, TokenKind::Function], |parser| {
        let routine_decl = match parser.peek() {
            TokenKind::Procedure => proc_decl(parser).map(RoutineDecl::Proc)?,
            TokenKind::Function => func_decl(parser).map(RoutineDecl::Func)?,
            _ => unreachable!(),
        };
        parser.expect(TokenKind::Semicolon)?;
        Ok(routine_decl)
    })
}

fn post_sig<'source>(parser: &mut ParserState<'source>) -> SpanParseResult<PostSig<'source>> {
    match parser.peek() {
        TokenKind::Ident => Ok(PostSig::Directive(parser.advance_source().into())),
        TokenKind::Begin => block(parser).map(PostSig::Block),
        _ => parser.next_error("'begin' or directive"),
    }
}

fn proc_decl<'source>(parser: &mut ParserState<'source>) -> SpanParseResult<ProcDecl<'source>> {
    parser.advance();
    let name = parser.expect_source(TokenKind::Ident)?;
    let params = if parser.is(TokenKind::LParen) {
        params(parser)?
    } else {
        Vec::new()
    };
    parser.expect(TokenKind::Semicolon)?;
    let post = post_sig(parser)?;
    Ok(ProcDecl {
        sig: ProcSig { name, params },
        post,
    })
}

fn func_decl<'source>(parser: &mut ParserState<'source>) -> SpanParseResult<FuncDecl<'source>> {
    parser.advance();
    let name = parser.expect_source(TokenKind::Ident)?;
    let params = if parser.is(TokenKind::LParen) {
        params(parser)?
    } else {
        Vec::new()
    };

    match parser.peek() {
        TokenKind::Colon => {
            parser.advance();
            let result = parser.expect_source(TokenKind::Ident)?;
            parser.expect(TokenKind::Semicolon)?;
            let post = post_sig(parser)?;
            Ok(FuncDecl::Heading(
                FuncSig {
                    name,
                    params,
                    result,
                },
                post,
            ))
        }
        TokenKind::Semicolon if params.is_empty() => {
            parser.expect(TokenKind::Semicolon)?;
            let block = block(parser)?;
            Ok(FuncDecl::Ident(&name, block))
        }
        TokenKind::Semicolon => parser.next_error("':'"),
        _ => parser.next_error("colon or semicolon"),
    }
}

fn params<'source>(parser: &mut ParserState<'source>) -> SpanParseResult<Vec<Param<'source>>> {
    parser.advance();
    let params = parser.repeat_sep(TokenKind::Comma, param)?;
    parser.expect(TokenKind::RParen)?;

    Ok(params)
}

fn param<'source>(parser: &mut ParserState<'source>) -> SpanParseResult<Param<'source>> {
    match parser.peek() {
        TokenKind::Ident => {
            let params = ident_list(parser)?;
            parser.expect(TokenKind::Colon)?;
            let ty = param_type(parser)?;

            Ok(Param::Value(params, ty))
        }
        TokenKind::Var => {
            parser.advance();
            let params = ident_list(parser)?;
            parser.expect_source(TokenKind::Colon)?;
            let ty = param_type(parser)?;

            Ok(Param::Var(params, ty))
        }
        TokenKind::Procedure => {
            parser.advance();
            let name = parser.expect_source(TokenKind::Ident)?;
            let params = params(parser)?;

            Ok(Param::Proc(ProcSig { name, params }))
        }
        TokenKind::Function => {
            parser.advance();
            let name = parser.expect_source(TokenKind::Ident)?;
            let params = params(parser)?;
            parser.expect(TokenKind::Colon)?;
            let result = parser.expect_source(TokenKind::Ident)?;

            Ok(Param::Func(FuncSig {
                name,
                params,
                result,
            }))
        }
        _ => parser.next_error("identifier, 'var', 'proc', 'func', 'packed' or 'array'"),
    }
}

fn param_type<'source>(parser: &mut ParserState<'source>) -> SpanParseResult<ParamType<'source>> {
    match parser.peek() {
        TokenKind::Ident => Ok(ParamType::TypeIdent(parser.advance_source())),
        TokenKind::Packed => {
            parser.advance();
            parser.expect(TokenKind::Array)?;
            parser.expect(TokenKind::LSquare)?;
            let index = index_type_spec(parser)?;
            parser.expect(TokenKind::RSquare)?;
            parser.expect(TokenKind::Of)?;
            let elem = parser.expect_source(TokenKind::Ident)?;

            Ok(ParamType::ArraySchema(Box::new(ArraySchema::Packed {
                index,
                elem,
            })))
        }
        TokenKind::Array => {
            parser.advance();
            parser.expect(TokenKind::LSquare)?;
            let mut indices = Vec::new();
            indices.push(index_type_spec(parser)?);
            while parser.is(TokenKind::Semicolon) {
                parser.advance();
                indices.push(index_type_spec(parser)?);
            }
            parser.expect(TokenKind::RSquare)?;
            parser.expect(TokenKind::Of)?;
            let elem = param_type(parser)?;
            Ok(ParamType::ArraySchema(Box::new(ArraySchema::Unpacked {
                indices,
                elem,
            })))
        }
        _ => parser.next_error("type identifier, 'array' or 'packed'"),
    }
}

fn index_type_spec<'source>(
    parser: &mut ParserState<'source>,
) -> SpanParseResult<IndexTypeSpec<'source>> {
    let lower = parser.expect_source(TokenKind::Ident)?;
    parser.expect(TokenKind::Ellipsis)?;
    let upper = parser.expect_source(TokenKind::Ident)?;
    parser.expect(TokenKind::Colon)?;
    let r#type = parser.expect_source(TokenKind::Ident)?;

    Ok(IndexTypeSpec {
        lower,
        upper,
        r#type,
    })
}

fn ident_list<'source>(parser: &mut ParserState<'source>) -> SpanParseResult<Vec<Ident>> {
    parser.repeat_sep(TokenKind::Comma, |parser| parser.ident())
}

pub(super) fn constexpr<'source>(parser: &mut ParserState<'source>) -> SpanParseResult<Expr> {
    Ok(match parser.peek() {
        TokenKind::UIntLit => Expr::UIntLit(parse_unsigned_integer(parser.advance_source())),
        TokenKind::URealLit => Expr::URealLit(parse_unsigned_real(parser.advance_source())),
        TokenKind::StrLit => Expr::StrLit(trim_ends(&parser.advance_source())),
        TokenKind::Ident => Expr::Var(Var::Plain(parser.advance_source())),
        op @ TokenKind::Plus | op @ TokenKind::Minus => {
            parser.advance();
            let op: UnaryOp = op.into();
            let operand = Box::new(match parser.peek() {
                TokenKind::UIntLit => {
                    Expr::UIntLit(parse_unsigned_integer(parser.advance_source()))
                }
                TokenKind::URealLit => Expr::URealLit(parse_unsigned_real(parser.advance_source())),
                TokenKind::StrLit => Expr::StrLit(trim_ends(&parser.advance_source())),
                TokenKind::Ident => Expr::Var(Var::Plain(parser.advance_source())),
                _ => {
                    return parser
                        .next_error("unsigned integer literal, string literal or identifier")
                }
            });
            Expr::UnaryOp { op, operand }
        }
        _ => return parser.next_error("numeric literal, string literal, identifier, '+' or '-'"),
    })
}
