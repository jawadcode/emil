use chumsky::{error::Rich, extra, input::ValueInput, prelude::*, span::SimpleSpan, Parser};

use crate::lexer::Token;

use super::{
    expr::{Expr, Variable},
    ident,
    stmt::{stmt, Stmt},
    tokes,
};

#[derive(Debug, Clone)]
pub struct Program<'source> {
    pub name: &'source str,
    pub params: Vec<&'source str>,
    pub block: Block<'source>,
}

#[derive(Debug, Clone)]
pub struct Block<'source> {
    pub decls: Decls<'source>,
    pub stmts: Vec<Stmt<'source>>,
}

#[derive(Debug, Clone)]
pub struct Decls<'source> {
    labels: Vec<u64>,
    const_defs: Vec<(&'source str, Expr<'source>)>,
    type_defs: Vec<(&'source str, Type<'source>)>,
}

#[derive(Debug, Clone)]
pub enum Type<'source> {
    Subrange {
        lower: Expr<'source>,
        upper: Expr<'source>,
    },
    Scalar(Vec<&'source str>),
    Structured {
        packed: bool,
        r#type: Box<UnpackedStructuredType<'source>>,
    },
    Pointer(&'source str),
    Identifier(&'source str),
}

#[derive(Clone, Debug)]
pub struct SubrangeType<'source> {
    lower: Expr<'source>,
    upper: Expr<'source>,
}

#[derive(Clone, Debug)]
pub enum UnpackedStructuredType<'source> {
    Array {
        indices: Vec<Type<'source>>,
        elem: Type<'source>,
    },
    Record(FieldList<'source>),
    Set(Type<'source>),
    File(Type<'source>),
}

#[derive(Clone, Debug)]
pub struct FieldList<'source> {
    fixed: Vec<Field<'source>>,
    variant: Option<VariantType<'source>>,
}

#[derive(Clone, Debug)]
pub struct Field<'source> {
    fields: Vec<&'source str>,
    r#type: Type<'source>,
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

pub(super) fn program<'source, I>(
) -> impl Parser<'source, I, Program<'source>, extra::Err<Rich<'source, Token<'source>>>>
where
    I: ValueInput<'source, Token = Token<'source>, Span = SimpleSpan>,
{
    just(Token::Program)
        .ignore_then(ident())
        .then(
            ident()
                .separated_by(just(Token::Comma))
                .collect::<Vec<_>>()
                .delimited_by(just(Token::LeftParen), just(Token::RightParen))
                .or_not(),
        )
        .then_ignore(just(Token::Semicolon))
        .then(block())
        .then_ignore(just(Token::Period))
        .map(|((name, params), block)| Program {
            name,
            params: params.unwrap_or_default(),
            block,
        })
}

fn block<'source, I>(
) -> impl Parser<'source, I, Block<'source>, extra::Err<Rich<'source, Token<'source>>>>
where
    I: ValueInput<'source, Token = Token<'source>, Span = SimpleSpan>,
{
    decl()
        .then(stmt().repeated().collect::<Vec<_>>())
        .map(|(decls, stmts)| Block { decls, stmts })
}

fn decl<'source, I>(
) -> impl Parser<'source, I, Decls<'source>, extra::Err<Rich<'source, Token<'source>>>>
where
    I: ValueInput<'source, Token = Token<'source>, Span = SimpleSpan>,
{
    let label_decls = just(Token::Label)
        .ignore_then(
            select! {Token::UnsignedIntegerLiteral(num) => num}
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
                .then_ignore(just(Token::Equals))
                .then(constexpr())
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
                .then_ignore(just(Token::Equals))
                .then(r#type())
                .then_ignore(just(Token::Semicolon))
                .repeated()
                .at_least(1)
                .collect::<Vec<_>>(),
        )
        .or_not()
        .map(Option::unwrap_or_default);

    label_decls
        .then(const_defs)
        .then(type_defs)
        .map(|((labels, const_defs), type_defs)| Decls {
            labels,
            const_defs,
            type_defs,
        })
}

fn constexpr<'source, I>(
) -> impl Parser<'source, I, Expr<'source>, extra::Err<Rich<'source, Token<'source>>>> + Clone
where
    I: ValueInput<'source, Token = Token<'source>, Span = SimpleSpan>,
{
    select! {
        Token::IntegerLiteral(num) => Expr::IntLit(num),
        Token::RealLiteral(num) => Expr::RealLit(num),
        Token::StringLiteral(s) => Expr::StrLit(s),
        Token::Ident(name) => Expr::Variable(Variable::Plain(name)),
    }
    .or(tokes([Token::Plus, Token::Minus])
        .then(ident())
        .map(|(op, name)| Expr::UnaryOperation {
            op: op.into(),
            operand: Box::new(Expr::Variable(Variable::Plain(name))),
        }))
}

fn r#type<'source, I>(
) -> impl Parser<'source, I, Type<'source>, extra::Err<Rich<'source, Token<'source>>>>
where
    I: ValueInput<'source, Token = Token<'source>, Span = SimpleSpan>,
{
    let scalar_type = ident()
        .separated_by(just(Token::Comma))
        .collect::<Vec<_>>()
        .delimited_by(just(Token::LeftParen), just(Token::RightParen))
        .map(Type::Scalar);
    let subrange_type = constexpr()
        .then_ignore(just(Token::Elipsis))
        .then(constexpr())
        .map(|(lower, upper)| Type::Subrange { lower, upper });
    let type_identifier = ident().map(Type::Identifier);
    let simple_type = choice((scalar_type, subrange_type, type_identifier));

    let pointer_type =
        tokes([Token::Caret, Token::UpArrow]).ignore_then(ident().map(Type::Pointer));

    let set_type = just(Token::Set)
        .ignore_then(just(Token::Of))
        .ignore_then(simple_type.clone())
        .map(UnpackedStructuredType::Set);

    recursive(|r#type| {
        let array_type = just(Token::Array)
            .ignore_then(
                simple_type
                    .clone()
                    .separated_by(just(Token::Comma))
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::LeftSquare), just(Token::RightSquare)),
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
                .map(|(fields, r#type)| Field { fields, r#type })
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
                        .then(
                            field_list
                                .delimited_by(just(Token::LeftParen), just(Token::RightParen)),
                        )
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

        choice((simple_type, structured_type, pointer_type))
    })
}
