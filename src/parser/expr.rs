use crate::lexer::TokenKind;

#[derive(Debug, Clone)]
pub enum Expr<'source> {
    Var(Var<'source>),
    Nil,
    UIntLit(u64),
    IntLit(i64),
    RealLit(f64),
    StrLit(&'source str),
    Set(Vec<Expr<'source>>),
    FuncCall(&'source str, Vec<Expr<'source>>),
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

#[derive(Debug, Clone)]
pub enum Var<'source> {
    Plain(&'source str),
    Ref(Box<Var<'source>>),
    Indexed(Box<Var<'source>>, Vec<Expr<'source>>),
    FieldAccess(Box<Var<'source>>, &'source str),
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub enum BinOp {
    Mult,
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
            TokenKind::Slash => Self::Div,
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
