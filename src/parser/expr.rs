use crate::{
    lexer::{parse_unsigned_integer, parse_unsigned_real, TokenKind},
    utils::trim_ends,
};

use super::{ParseResult, ParserState};

#[derive(Debug, Clone)]
pub enum Expr<'source> {
    Var(Var<'source>),
    Nil,
    UIntLit(u64),
    URealLit(f64),
    StrLit(&'source str),
    Set(Vec<Expr<'source>>),
    FuncCall(&'source str, Box<Params<'source>>),
    UnaryOp {
        op: UnaryOp,
        operand: Box<Expr<'source>>,
    },
    BinOp {
        op: BinOp,
        left: Box<Expr<'source>>,
        right: Box<Expr<'source>>,
    },
}

pub fn expr<'source>(parser: &mut ParserState<'source>) -> ParseResult<Expr<'source>> {
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
            Ok(Expr::BinOp { op, left, right })
        },
        simple_expr,
    )
}

fn simple_expr<'source>(parser: &mut ParserState<'source>) -> ParseResult<Expr<'source>> {
    let sign = match parser.peek() {
        TokenKind::Plus | TokenKind::Minus => Some(parser.advance().node.into()),
        _ => None,
    };
    parser.repeat_fold(
        &[TokenKind::Plus, TokenKind::Minus, TokenKind::Or],
        |parser, left| {
            let left = Box::new(left);
            let op = parser.advance().node.into();
            let right = term(parser).map(Box::new)?;
            let binop = Expr::BinOp { op, left, right };
            match sign {
                Some(op) => Ok(Expr::UnaryOp {
                    op,
                    operand: Box::new(binop),
                }),
                None => Ok(binop),
            }
        },
        term,
    )
}

fn term<'source>(parser: &mut ParserState<'source>) -> ParseResult<Expr<'source>> {
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
            Ok(Expr::BinOp { op, left, right })
        },
        factor,
    )
}

fn factor<'source>(parser: &mut ParserState<'source>) -> ParseResult<Expr<'source>> {
    match parser.peek() {
        TokenKind::UIntLit => Ok(Expr::UIntLit(parse_unsigned_integer(
            parser.advance_source(),
        ))),
        TokenKind::URealLit => Ok(Expr::URealLit(parse_unsigned_real(parser.advance_source()))),
        TokenKind::StrLit => Ok(Expr::StrLit(trim_ends(parser.advance_source()))),
        TokenKind::Nil => {
            parser.advance();
            Ok(Expr::Nil)
        }
        TokenKind::Ident => factor_ident(parser),
        TokenKind::LSquare => {
            parser.advance();
            let elems = parser.repeat_sep(TokenKind::Comma, expr)?;
            parser.expect(TokenKind::RSquare)?;
            Ok(Expr::Set(elems))
        }
        TokenKind::Not => {
            parser.advance();
            Ok(Expr::UnaryOp {
                op: UnaryOp::Not,
                operand: factor(parser).map(Box::new)?,
            })
        }
        TokenKind::LParen => {
            parser.advance();
            let expr = expr(parser)?;
            parser.expect(TokenKind::RParen)?;
            Ok(expr)
        }
        _ => parser.next_error("factor"),
    }
}

fn factor_ident<'source>(parser: &mut ParserState<'source>) -> ParseResult<Expr<'source>> {
    let ident = parser.advance_source();
    if parser.is(VAR_EXT_START) {
        parser
            .repeat_fold(VAR_EXT_START, var_ext, |_| Ok(Var::Plain(ident)))
            .map(Expr::Var)
    } else if parser.is(TokenKind::LParen) {
        let params = params(parser)?;
        Ok(Expr::FuncCall(ident, Box::new(params)))
    } else {
        Ok(Expr::Var(Var::Plain(ident)))
    }
}

#[derive(Debug, Clone)]
pub enum Var<'source> {
    Plain(&'source str),
    Ref(Box<Var<'source>>),
    Indexed(Box<Var<'source>>, Vec<Expr<'source>>),
    FieldAccess(Box<Var<'source>>, &'source str),
}

pub(super) const VAR_EXT_START: &[TokenKind] = &[
    TokenKind::Caret,
    TokenKind::UpArrow,
    TokenKind::LSquare,
    TokenKind::Dot,
];

pub(super) fn var<'source>(parser: &mut ParserState<'source>) -> ParseResult<Var<'source>> {
    parser.repeat_fold(VAR_EXT_START, var_ext, |parser| {
        Ok(Var::Plain(parser.advance_source()))
    })
}

pub(super) fn var_ext<'source>(
    parser: &mut ParserState<'source>,
    var: Var<'source>,
) -> ParseResult<Var<'source>> {
    match parser.peek() {
        TokenKind::Caret | TokenKind::UpArrow => {
            parser.advance();
            Ok(Var::Ref(Box::new(var)))
        }
        TokenKind::LSquare => {
            parser.advance();
            let indices = parser.repeat_sep(TokenKind::Comma, expr)?;
            parser.expect(TokenKind::RSquare)?;
            Ok(Var::Indexed(Box::new(var), indices))
        }
        TokenKind::Dot => {
            parser.advance();
            let field = parser.expect_source(TokenKind::Ident)?;
            Ok(Var::FieldAccess(Box::new(var), field))
        }
        _ => parser.next_error("'^', '↑', '[' or '.'"),
    }
}

#[derive(Debug, Clone)]
pub enum Params<'source> {
    // It feels icky to be treating `Write` and `Writeln` differently in the
    // parser, and since <write-param-list> overlaps with <actual-param-list>,
    // our `Params::Actual` may actually be a `Params::Write`, hence the name.
    MaybeActual(Vec<Expr<'source>>),
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

// TODO: Implement write parameters by branching on the existence of a colon at
// the end of the first and second parameters
pub(super) fn params<'source>(parser: &mut ParserState<'source>) -> ParseResult<Params<'source>> {
    parser.advance();
    let params = parser
        .repeat_sep(TokenKind::Comma, expr)
        .map(Params::MaybeActual)?;
    parser.expect(TokenKind::RParen)?;
    Ok(params)
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    Not,
    Identity,
    Negation,
}

impl From<TokenKind> for UnaryOp {
    fn from(value: TokenKind) -> Self {
        match value {
            TokenKind::Plus => UnaryOp::Identity,
            TokenKind::Minus => UnaryOp::Negation,
            TokenKind::Not => UnaryOp::Not,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum BinOp {
    Mult,
    Quot,
    Div,
    Mod,
    And,

    Add,
    Sub,
    Or,

    Eq,
    NEq,
    LT,
    GT,
    LEq,
    GEq,
    In,
}

impl From<TokenKind> for BinOp {
    fn from(value: TokenKind) -> Self {
        match value {
            TokenKind::Asterisk => Self::Mult,
            TokenKind::Slash => Self::Quot,
            TokenKind::Div => Self::Div,
            TokenKind::Mod => Self::Mod,
            TokenKind::And => Self::And,
            TokenKind::Plus => Self::Add,
            TokenKind::Minus => Self::Sub,
            TokenKind::Or => Self::Or,
            TokenKind::Eq => Self::Eq,
            TokenKind::NEq => Self::NEq,
            TokenKind::LT => Self::LT,
            TokenKind::GT => Self::GT,
            TokenKind::LEq => Self::LEq,
            TokenKind::GEq => Self::GEq,
            TokenKind::In => Self::In,
            _ => unreachable!(),
        }
    }
}
