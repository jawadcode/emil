use crate::{ast::Ident, lexer::TokenKind, utils::Spanned};

pub type Expr = Spanned<UnspanExpr>;

#[derive(Debug, Clone)]
pub enum UnspanExpr {
    Var(Var),
    Nil,
    UIntLit(u64),
    URealLit(f64),
    StrLit(String),
    Set(Vec<Expr>),
    FuncCall(Ident, Box<Params>),
    UnaryOp {
        op: UnaryOp,
        operand: Box<Expr>,
    },
    BinOp {
        op: BinOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
}

#[derive(Debug, Clone)]
pub enum Var {
    Plain(Ident),
    Ref(Box<Var>),
    Indexed(Box<Var>, Vec<Expr>),
    FieldAccess(Box<Var>, Ident),
}

pub type Params = Vec<Expr>;

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
