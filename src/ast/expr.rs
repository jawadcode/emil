use crate::{
    ast::{Ident, UnspanIdent},
    lexer::TokenKind,
    utils::Spanned,
};

pub type SpanExpr = Spanned<Expr>;

#[derive(Debug, Clone)]
pub enum Expr {
    Var(Var),
    Nil,
    UIntLit(u64),
    URealLit(f64),
    StrLit(String),
    Set(Vec<SpanExpr>),
    FuncCall(Ident, Box<Spanned<Params>>),
    UnaryOp {
        op: UnaryOp,
        operand: Box<Expr>,
    },
    BinOp {
        op: BinOp,
        left: Box<SpanExpr>,
        right: Box<SpanExpr>,
    },
}

pub type SpanVar = Spanned<Var>;

#[derive(Debug, Clone)]
pub enum Var {
    Plain(UnspanIdent),
    Ref(Box<SpanVar>),
    Indexed(Box<SpanVar>, Vec<SpanExpr>),
    FieldAccess(Box<SpanVar>, Ident),
}

pub type Params = Vec<SpanExpr>;

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
