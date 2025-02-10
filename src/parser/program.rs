use chumsky::{error::Simple, prelude::*, Parser};

use crate::lexer::{parse_real, Token};

use super::{
    expr::{Expr, Var},
    ident,
    stmt::{compound_stmt, CompoundStmt},
    tokes, ParserError,
};

#[derive(Debug, Clone)]
pub struct Program<'source> {
    pub name: &'source str,
    pub params: Vec<&'source str>,
    pub block: Block<'source>,
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

#[derive(Debug, Clone)]
pub enum Type<'source> {
    Ordinal(OrdinalType<'source>),
    Structured {
        packed: bool,
        r#type: Box<UnpackedStructuredType<'source>>,
    },
    Pointer(&'source str),
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

#[derive(Clone, Debug)]
pub struct FieldList<'source> {
    fixed: Vec<(Vec<&'source str>, Type<'source>)>,
    variant: Option<VariantType<'source>>,
}

#[derive(Clone, Debug)]
pub struct VariantType<'source> {
    tag_field: Option<&'source str>,
    tag_type: &'source str,
    variants: Vec<Variant<'source>>,
}

#[derive(Clone, Debug)]
pub struct Variant<'source> {
    case_labels: Vec<Expr<'source>>,
    fields: FieldList<'source>,
}

#[derive(Clone, Debug)]
pub enum RoutineDecl<'source> {
    Proc(ProcDecl<'source>),
    Func(FuncDecl<'source>),
}

#[derive(Clone, Debug)]
pub enum ProcDecl<'source> {
    Heading(ProcHeading<'source>, PostHeading<'source>),
    Id(&'source str, Block<'source>),
}

#[derive(Clone, Debug)]
pub struct ProcHeading<'source> {
    name: &'source str,
    params: Vec<Param<'source>>,
}

#[derive(Debug, Clone)]
pub enum FuncDecl<'source> {
    Heading(FuncHeading<'source>, PostHeading<'source>),
    Id {
        name: &'source str,
        block: Block<'source>,
    },
}

#[derive(Debug, Clone)]
pub struct FuncHeading<'source> {
    name: &'source str,
    params: Vec<Param<'source>>,
    result_type: &'source str,
}

#[derive(Debug, Clone)]
pub enum PostHeading<'source> {
    Block(Block<'source>),
    Directive(Directive<'source>),
}

#[derive(Debug, Clone)]
pub enum Directive<'source> {
    Forward,
    Extern,
    Unknown(&'source str),
}

#[derive(Clone, Debug)]
pub enum Param<'source> {
    Value(Vec<&'source str>, ParamType<'source>),
    Var(Vec<&'source str>, ParamType<'source>),
    Proc(ProcHeading<'source>),
    Func(FuncHeading<'source>),
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
        element: &'source str,
    },
    Unpacked {
        indices: Vec<IndexTypeSpec<'source>>,
        element: ParamType<'source>,
    },
}

#[derive(Clone, Debug)]
pub struct IndexTypeSpec<'source> {
    start: &'source str,
    end: &'source str,
    r#type: &'source str,
}

pub(super) fn program<'source>(
) -> impl Parser<Token<'source>, Program<'source>, Error = ParserError<'source>> + Clone {
    just(Token::Program)
        .ignore_then(ident())
        .then(
            ident()
                .separated_by(just(Token::Comma))
                .collect::<Vec<_>>()
                .delimited_by(just(Token::LParen), just(Token::RParen))
                .or_not(),
        )
        .then_ignore(just(Token::Semicolon))
        .then(block())
        .then_ignore(just(Token::Dot))
        .map(|((name, params), block)| Program {
            name,
            params: params.unwrap_or_default(),
            block,
        })
}

fn block<'source>(
) -> impl Parser<Token<'source>, Block<'source>, Error = ParserError<'source>> + Clone {
    recursive(|block| {
        let label_decls = just(Token::Label)
            .ignore_then(
                select! {Token::UIntLit(num) => num}
                    .separated_by(just(Token::Comma))
                    .at_least(1)
                    .collect::<Vec<_>>(),
            )
            .then_ignore(just(Token::Semicolon))
            .or_not()
            .map(Option::unwrap_or_default);

        let const_defs = just(Token::Const)
            .ignore_then(
                ident()
                    .then_ignore(just(Token::Eq))
                    .then(constexpr())
                    .then_ignore(just(Token::Semicolon))
                    .repeated()
                    .at_least(1)
                    .collect::<Vec<_>>(),
            )
            .or_not()
            .map(Option::unwrap_or_default);

        let var_decls = just(Token::Var)
            .ignore_then(
                ident()
                    .repeated()
                    .at_least(1)
                    .collect::<Vec<_>>()
                    .then(r#type())
                    .then_ignore(just(Token::Semicolon))
                    .repeated()
                    .at_least(1)
                    .collect::<Vec<_>>(),
            )
            .or_not()
            .map(Option::unwrap_or_default);

        let type_defs = just(Token::Type)
            .ignore_then(
                ident()
                    .then_ignore(just(Token::Eq))
                    .then(r#type())
                    .then_ignore(just(Token::Semicolon))
                    .repeated()
                    .at_least(1)
                    .collect::<Vec<_>>(),
            )
            .or_not()
            .map(Option::unwrap_or_default);

        let index_type = ident()
            .then_ignore(just(Token::Ellipsis))
            .then(ident())
            .then_ignore(just(Token::Colon))
            .then(ident(/* Ordinal type identifier */))
            .map(
                |((start, end), r#type): ((&'source str, _), _)| IndexTypeSpec {
                    start,
                    end,
                    r#type,
                },
            );
        let packed_array_schema = just(Token::Packed)
            .ignore_then(just(Token::Array))
            .ignore_then(
                index_type
                    .clone()
                    .delimited_by(just(Token::LParen), just(Token::RParen)),
            )
            .then_ignore(just(Token::Of))
            .then(ident(/* Type identifier */))
            .map(|(index, element)| ArraySchema::Packed { index, element });
        let param_type = recursive(|param_type| {
            let unpacked_array_schema = just(Token::Array)
                .ignore_then(
                    index_type
                        .separated_by(just(Token::Semicolon))
                        .at_least(1)
                        .collect::<Vec<_>>()
                        .delimited_by(just(Token::LSquare), just(Token::RSquare)),
                )
                .then_ignore(just(Token::Of))
                .then(param_type)
                .map(|(indices, element)| ArraySchema::Unpacked { indices, element });
            choice((
                ident().map(ParamType::TypeIdent),
                packed_array_schema
                    .map(Box::new)
                    .map(ParamType::ArraySchema),
                unpacked_array_schema
                    .map(Box::new)
                    .map(ParamType::ArraySchema),
            ))
        });

        let idents = ident().repeated().at_least(1).collect::<Vec<_>>();
        let value_param = idents
            .clone()
            .then_ignore(just(Token::Colon))
            .then(param_type.clone())
            .map(|(names, r#type)| Param::Value(names, r#type));
        let var_param = just(Token::Var)
            .ignore_then(idents)
            .then_ignore(just(Token::Colon))
            .then(param_type)
            .map(|(names, r#type)| Param::Var(names, r#type));

        let mut formal_param_list = Recursive::declare();

        let proc_heading = just(Token::Procedure)
            .ignore_then(ident())
            .then(formal_param_list.clone())
            .map(|(name, params)| ProcHeading { name, params });
        let func_heading = just(Token::Function)
            .ignore_then(ident())
            .then(formal_param_list.clone())
            .then(ident(/* result type */))
            .map(|((name, params), result_type)| FuncHeading {
                name,
                params,
                result_type,
            });

        formal_param_list.define(
            choice((
                value_param,
                var_param,
                proc_heading.clone().map(Param::Proc),
                func_heading.clone().map(Param::Func),
            ))
            .separated_by(just(Token::Semicolon))
            .at_least(1)
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LParen), just(Token::RParen)),
        );

        let proc_identification =
            just(Token::Procedure).ignore_then(ident(/* procedure identifier */));
        let func_identification =
            just(Token::Function).ignore_then(ident(/* function identifier */));

        let post_heading = ident()
            .map(|s| match s {
                "forward" => Directive::Forward,
                "extern" => Directive::Extern,
                s => Directive::Unknown(s),
            })
            .map(PostHeading::Directive)
            .or(block.clone().map(PostHeading::Block));

        let proc_decl = proc_heading
            .clone()
            .then_ignore(just(Token::Semicolon))
            .then(post_heading.clone())
            .map(|(heading, post)| ProcDecl::Heading(heading, post))
            .or(proc_identification
                .then_ignore(just(Token::Semicolon))
                .then(block.clone())
                .map(|(name, block)| ProcDecl::Id(name, block)));
        let func_decl = func_heading
            .then_ignore(just(Token::Semicolon))
            .then(post_heading)
            .map(|(heading, post)| FuncDecl::Heading(heading, post))
            .or(func_identification
                .then_ignore(just(Token::Semicolon))
                .then(block)
                .map(|(name, block)| FuncDecl::Id { name, block }));

        let routine_decl = proc_decl
            .map(RoutineDecl::Proc)
            .or(func_decl.map(RoutineDecl::Func));

        let routine_decls = routine_decl
            .then_ignore(just(Token::Semicolon))
            .repeated()
            .at_least(1)
            .collect::<Vec<RoutineDecl>>();

        label_decls
            .then(const_defs)
            .then(var_decls)
            .then(type_defs)
            .then(routine_decls)
            .then(compound_stmt())
            .map(
                |(((((labels, const_defs), var_decls), type_defs), routine_decls), stmts)| Block {
                    label_decls: labels,
                    const_defs,
                    type_defs,
                    var_decls,
                    routine_decls,
                    stmts,
                },
            )
    })
}

fn r#type<'source>(
) -> impl Parser<Token<'source>, Type<'source>, Error = ParserError<'source>> + Clone {
    let ordinal_type = ordinal_type();

    let pointer_type =
        tokes([Token::Caret, Token::UpArrow]).ignore_then(ident().map(Type::Pointer));

    let set_type = just(Token::Set)
        .ignore_then(just(Token::Of))
        .ignore_then(ordinal_type.clone())
        .map(UnpackedStructuredType::Set);

    recursive(|r#type| {
        let array_type = just(Token::Array)
            .ignore_then(
                ordinal_type
                    .clone()
                    .separated_by(just(Token::Comma))
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::LSquare), just(Token::RSquare)),
            )
            .then_ignore(just(Token::Of))
            .then(r#type.clone())
            .map(|(indices, elem)| UnpackedStructuredType::Array { indices, elem });

        let field_list = recursive(|field_list| {
            let fixed_part = ident()
                .separated_by(just(Token::Comma))
                .collect::<Vec<_>>()
                .then_ignore(just(Token::Colon))
                .then(r#type.clone())
                .separated_by(just(Token::Semicolon))
                .collect::<Vec<_>>();
            let variant_part = just(Token::Case)
                .ignore_then(ident().then_ignore(just(Token::Colon)).or_not())
                .then(ident())
                .then_ignore(just(Token::Of))
                .then(
                    constexpr()
                        .separated_by(just(Token::Comma))
                        .collect::<Vec<_>>()
                        .then_ignore(just(Token::Colon))
                        .then(field_list.delimited_by(just(Token::LParen), just(Token::RParen)))
                        .map(|(case_labels, fields)| Variant {
                            case_labels,
                            fields,
                        })
                        .separated_by(just(Token::Semicolon))
                        .collect::<Vec<_>>(),
                )
                .map(|((tag_field, tag_type), variants)| VariantType {
                    tag_field,
                    tag_type,
                    variants,
                });
            choice((
                fixed_part
                    .clone()
                    .then_ignore(just(Token::Semicolon))
                    .then(variant_part.clone().map(Some))
                    .map(|(fixed, variant)| FieldList { fixed, variant }),
                fixed_part.clone().map(|fixed| FieldList {
                    fixed,
                    variant: None,
                }),
                variant_part.map(Some).map(|variant| FieldList {
                    fixed: Vec::new(),
                    variant,
                }),
            ))
        });
        let record_type = just(Token::Record)
            .ignore_then(field_list)
            .then_ignore(just(Token::End))
            .map(|field_list| UnpackedStructuredType::Record(field_list));

        let file_type = just(Token::File)
            .ignore_then(just(Token::Of))
            .ignore_then(r#type)
            .map(UnpackedStructuredType::File);

        let unstructured_type =
            choice((array_type, record_type, set_type, file_type)).map(Box::new);
        let structured_type = just(Token::Packed)
            .or_not()
            .map(|p| p.is_some())
            .then(unstructured_type)
            .map(|(packed, r#type)| Type::Structured { packed, r#type });

        choice((
            ordinal_type.map(Type::Ordinal),
            structured_type,
            pointer_type,
        ))
    })
}

fn r#ordinal_type<'source>(
) -> impl Parser<Token<'source>, OrdinalType<'source>, Error = ParserError<'source>> + Clone {
    let scalar_type = ident()
        .separated_by(just(Token::Comma))
        .collect::<Vec<_>>()
        .delimited_by(just(Token::LParen), just(Token::RParen))
        .map(OrdinalType::Enumerated);
    let subrange_type = constexpr()
        .then_ignore(just(Token::Ellipsis))
        .then(constexpr())
        .map(|(lower, upper)| OrdinalType::Subrange { lower, upper });
    let ordinal_type_ident = ident().map(OrdinalType::Identifier);

    choice((scalar_type, subrange_type, ordinal_type_ident))
}

pub(super) fn constexpr<'source>(
) -> impl Parser<Token<'source>, Expr<'source>, Error = ParserError<'source>> + Clone {
    select! {
        Token::IntLit(num) => Expr::IntLit(num),
        Token::RealLit(num) => Expr::RealLit(parse_real(num)),
        Token::StrLit(s) => Expr::StrLit(s),
        Token::Ident(name) => Expr::Var(Var::Plain(name)),
    }
    .or(tokes([Token::Plus, Token::Minus])
        .then(ident())
        .map(|(op, name)| Expr::UnaryOp {
            op: op.into(),
            operand: Box::new(Expr::Var(Var::Plain(name))),
        }))
}
