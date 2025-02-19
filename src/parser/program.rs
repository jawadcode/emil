use crate::lexer::{parse_int, parse_real, parse_unsigned_integer, TokenKind};

use super::{
    expr::{Expr, UnaryOp, Var},
    stmt::{compound_stmt, CompoundStmt},
    ParseResult, ParserState,
};

#[derive(Debug, Clone)]
pub struct Program<'source> {
    pub name: &'source str,
    pub params: Vec<&'source str>,
    pub block: Block<'source>,
}

pub fn program<'source>(parser: &mut ParserState<'source>) -> ParseResult<Program<'source>> {
    parser.expect(TokenKind::Program)?;
    let name = parser.expect_source(TokenKind::Ident)?;

    let params = if parser.is(TokenKind::LParen) {
        let idents = ident_list(parser)?;
        parser.expect(TokenKind::RParen)?;
        idents
    } else {
        Vec::new()
    };

    parser.expect(TokenKind::Semicolon)?;
    let block = block(parser)?;
    parser.expect(TokenKind::Dot)?;

    Ok(Program {
        name,
        params,
        block,
    })
}

#[derive(Debug, Clone)]
pub struct Block<'source> {
    label_decls: Vec<u64>,
    const_defs: Vec<(&'source str, Expr<'source>)>,
    type_defs: Vec<(&'source str, Type<'source>)>,
    var_decls: Vec<(Vec<&'source str>, Type<'source>)>,
    routine_decls: Vec<RoutineDecl<'source>>,
    stmts: CompoundStmt<'source>,
}

fn block<'source>(parser: &mut ParserState<'source>) -> ParseResult<Block<'source>> {
    let label_decls = if parser.is(TokenKind::Label) {
        let res = parser.repeat_sep(TokenKind::Comma, |parser| {
            parser
                .expect_source(TokenKind::UIntLit)
                .map(parse_unsigned_integer)
        })?;
        parser.expect(TokenKind::Semicolon)?;
        res
    } else {
        Vec::new()
    };

    let const_defs = if parser.is(TokenKind::Const) {
        parser.repeated(TokenKind::Ident, |parser| {
            let name = parser.advance_source();
            parser.expect(TokenKind::Eq)?;
            let value = constexpr(parser)?;
            parser.expect(TokenKind::Semicolon)?;
            Ok((name, value))
        })?
    } else {
        Vec::new()
    };

    let type_defs = if parser.is(TokenKind::Type) {
        parser.repeated(TokenKind::Ident, |parser| {
            let name = parser.advance_source();
            parser.expect(TokenKind::Eq)?;
            let r#type = r#type(parser)?;
            parser.expect(TokenKind::Semicolon)?;
            Ok((name, r#type))
        })?
    } else {
        Vec::new()
    };

    let var_decls = if parser.is(TokenKind::Var) {
        parser.repeated(TokenKind::Ident, |parser| {
            let names = ident_list(parser)?;
            parser.expect(TokenKind::Colon)?;
            let r#type = r#type(parser)?;
            parser.expect(TokenKind::Semicolon)?;
            Ok((names, r#type))
        })?
    } else {
        Vec::new()
    };

    let routine_decls = routine_decls(parser)?;

    let stmts = compound_stmt(parser)?;

    Ok(Block {
        label_decls,
        const_defs,
        type_defs,
        var_decls,
        routine_decls,
        stmts,
    })
}

#[derive(Debug, Clone)]
pub enum Type<'source> {
    Ordinal(OrdinalType<'source>),
    Structured {
        packed: bool,
        r#type: Box<UnpackedStructuredType<'source>>,
    },
    Pointer(&'source str),
}

fn r#type<'source>(parser: &mut ParserState<'source>) -> ParseResult<Type<'source>> {
    match parser.peek() {
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
        _ => parser.next_error("ordinal type"),
    }
}

#[derive(Debug, Clone)]
pub enum OrdinalType<'source> {
    Enumerated(Vec<&'source str>),
    Subrange {
        lower: Expr<'source>,
        upper: Expr<'source>,
    },
    Identifier(&'source str),
}

fn ordinal_type<'source>(parser: &mut ParserState<'source>) -> ParseResult<OrdinalType<'source>> {
    match parser.peek() {
        TokenKind::Ident => {
            if parser.is(TokenKind::Ellipsis) {
                subrange_type(parser)
            } else {
                Ok(OrdinalType::Identifier(parser.advance_source()))
            }
        }
        TokenKind::Plus
        | TokenKind::Minus
        | TokenKind::UIntLit
        | TokenKind::IntLit
        | TokenKind::RealLit
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

fn subrange_type<'source>(parser: &mut ParserState<'source>) -> ParseResult<OrdinalType<'source>> {
    let lower = constexpr(parser)?;
    parser.advance();
    let upper = constexpr(parser)?;
    Ok(OrdinalType::Subrange { lower, upper })
}

#[derive(Clone, Debug)]
pub enum UnpackedStructuredType<'source> {
    Array {
        indices: Vec<OrdinalType<'source>>,
        elem: Type<'source>,
    },
    Record(FieldList<'source>),
    Set(OrdinalType<'source>),
    File(Type<'source>),
}

fn unpacked_structured_type<'source>(
    parser: &mut ParserState<'source>,
) -> ParseResult<UnpackedStructuredType<'source>> {
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

#[derive(Clone, Debug)]
pub enum FieldList<'source> {
    FixedOnly(FixedPart<'source>),
    Both(FixedPart<'source>, VariantField<'source>),
    VariantOnly(VariantField<'source>),
    Empty,
}

type FixedPart<'source> = Vec<(Vec<&'source str>, Type<'source>)>;

fn field_list<'source>(parser: &mut ParserState<'source>) -> ParseResult<FieldList<'source>> {
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

#[derive(Clone, Debug)]
pub struct VariantField<'source> {
    tag_field: Option<&'source str>,
    tag_type: &'source str,
    variants: Vec<Variant<'source>>,
}

#[derive(Clone, Debug)]
pub struct Variant<'source> {
    case_labels: Vec<Expr<'source>>,
    fields: FieldList<'source>,
}

fn variant_field<'source>(parser: &mut ParserState<'source>) -> ParseResult<VariantField<'source>> {
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

#[derive(Clone, Debug)]
pub enum RoutineDecl<'source> {
    Proc(ProcDecl<'source>),
    Func(FuncDecl<'source>),
}

fn routine_decls<'source>(
    parser: &mut ParserState<'source>,
) -> ParseResult<Vec<RoutineDecl<'source>>> {
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

#[derive(Debug, Clone)]
pub enum PostSig<'source> {
    Block(Block<'source>),
    Directive(Directive<'source>),
}

fn post_sig<'source>(parser: &mut ParserState<'source>) -> ParseResult<PostSig<'source>> {
    match parser.peek() {
        TokenKind::Ident => Ok(PostSig::Directive(parser.advance_source().into())),
        TokenKind::Begin => block(parser).map(PostSig::Block),
        _ => parser.next_error("'begin' or directive"),
    }
}

#[derive(Clone, Debug)]
pub struct ProcDecl<'source> {
    sig: ProcSig<'source>,
    post: PostSig<'source>,
}

#[derive(Clone, Debug)]
pub struct ProcSig<'source> {
    name: &'source str,
    /// params.is_empty() => procedure identification
    /// otherwise         => procedure heading
    params: Vec<Param<'source>>,
}

fn proc_decl<'source>(parser: &mut ParserState<'source>) -> ParseResult<ProcDecl<'source>> {
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

#[derive(Debug, Clone)]
pub enum FuncDecl<'source> {
    Ident(&'source str, Block<'source>),
    Heading(FuncSig<'source>, PostSig<'source>),
}

#[derive(Debug, Clone)]
pub struct FuncSig<'source> {
    name: &'source str,
    params: Vec<Param<'source>>,
    result: &'source str,
}

fn func_decl<'source>(parser: &mut ParserState<'source>) -> ParseResult<FuncDecl<'source>> {
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

#[derive(Debug, Clone)]
pub enum Directive<'source> {
    Forward,
    External,
    Unknown(&'source str),
}

impl<'source> From<&'source str> for Directive<'source> {
    fn from(other: &'source str) -> Self {
        match other.to_lowercase().as_str() {
            "forward" => Self::Forward,
            "external" => Self::External,
            _ => Self::Unknown(other),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Param<'source> {
    Value(Vec<&'source str>, ParamType<'source>),
    Var(Vec<&'source str>, ParamType<'source>),
    Proc(ProcSig<'source>),
    Func(FuncSig<'source>),
}

fn params<'source>(parser: &mut ParserState<'source>) -> ParseResult<Vec<Param<'source>>> {
    parser.advance();
    let params = parser.repeat_sep(TokenKind::Comma, param)?;
    parser.expect(TokenKind::RParen)?;

    Ok(params)
}

fn param<'source>(parser: &mut ParserState<'source>) -> ParseResult<Param<'source>> {
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

#[derive(Clone, Debug)]
pub enum ParamType<'source> {
    TypeIdent(&'source str),
    ArraySchema(Box<ArraySchema<'source>>),
}

#[derive(Clone, Debug)]
pub enum ArraySchema<'source> {
    Packed {
        index: IndexTypeSpec<'source>,
        elem: &'source str,
    },
    Unpacked {
        indices: Vec<IndexTypeSpec<'source>>,
        elem: ParamType<'source>,
    },
}

fn param_type<'source>(parser: &mut ParserState<'source>) -> ParseResult<ParamType<'source>> {
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

#[derive(Clone, Debug)]
pub struct IndexTypeSpec<'source> {
    lower: &'source str,
    upper: &'source str,
    r#type: &'source str,
}

fn index_type_spec<'source>(
    parser: &mut ParserState<'source>,
) -> ParseResult<IndexTypeSpec<'source>> {
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

// FIXME: This does not allow a space between the sign and the literal when it comes
// to real numbers. Don't think this is standard compliant.
fn constexpr<'source>(parser: &mut ParserState<'source>) -> ParseResult<Expr<'source>> {
    let trim_ends = |s: &'source str| &s[1..(s.len() - 1)];
    Ok(match parser.peek() {
        TokenKind::UIntLit => Expr::UIntLit(parse_unsigned_integer(parser.advance_source())),
        TokenKind::IntLit => Expr::IntLit(parse_int(parser.advance_source())),
        TokenKind::RealLit => Expr::RealLit(parse_real(parser.advance_source())),
        TokenKind::StrLit => Expr::StrLit(trim_ends(&parser.advance_source())),
        TokenKind::Ident => Expr::Var(Var::Plain(parser.advance_source())),
        op @ TokenKind::Plus | op @ TokenKind::Minus => {
            parser.advance();
            let op: UnaryOp = op.into();
            let operand = Box::new(match parser.peek() {
                TokenKind::UIntLit => {
                    Expr::UIntLit(parse_unsigned_integer(parser.advance_source()))
                }
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

fn ident_list<'source>(parser: &mut ParserState<'source>) -> ParseResult<Vec<&'source str>> {
    parser.repeat_sep(TokenKind::Comma, |parser| {
        parser.expect_source(TokenKind::Ident)
    })
}
