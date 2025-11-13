use crate::{
    ast::{
        expr::{Expr, Params, SpanExpr, SpanVar},
        Ident,
    },
    utils::{Span, Spanned},
};

pub type CompoundStmt = Vec<Spanned<MaybeLabelledStmt>>;

#[derive(Debug, Clone)]
pub struct MaybeLabelledStmt {
    pub label: Option<Spanned<u64>>,
    pub stmt: Box<SpanStmt>,
}

pub type SpanStmt = Spanned<Stmt>;

#[derive(Debug, Clone)]
pub enum Stmt {
    Empty,
    Assign {
        var: SpanVar,
        value: SpanExpr,
    },
    // One or more
    ReadCall(Vec<SpanVar>),
    // Zero or more
    ReadlnCall(Vec<SpanVar>),
    // One or more
    WriteCall(Vec<Spanned<WriteParam>>),
    // Zero or more
    WritelnCall(Vec<Spanned<WriteParam>>),
    ProcCall {
        name: Ident,
        params: Spanned<Params>,
    },
    Goto(u64),
    Compound(CompoundStmt),
    If {
        cond: SpanExpr,
        then: Spanned<MaybeLabelledStmt>,
        r#else: Option<Spanned<MaybeLabelledStmt>>,
    },
    Case {
        index: SpanExpr,
        cases: Spanned<Vec<Spanned<Case>>>,
    },
    While {
        cond: SpanExpr,
        body: Spanned<MaybeLabelledStmt>,
    },
    Repeat {
        body: Spanned<Vec<Spanned<MaybeLabelledStmt>>>,
        cond: SpanExpr,
    },
    For {
        control_var: Ident,
        from: SpanExpr,
        direction: Spanned<ForDirection>,
        to: SpanExpr,
        body: Spanned<MaybeLabelledStmt>,
    },
    With {
        vars: Spanned<Vec<SpanVar>>,
        body: Spanned<MaybeLabelledStmt>,
    },
}

#[derive(Debug, Clone)]
pub struct WriteParam {
    pub param: SpanExpr,
    pub specifiers: Spanned<WriteParamSpecs>,
}

#[derive(Debug, Clone)]
pub struct WriteParamSpecs {
    pub colon_span: Span,
    pub field_width: SpanExpr,
    pub frac_digits: Option<(
        /* colon_span */ Span,
        /* fractional digits specifier */ SpanExpr,
    )>,
}

#[derive(Debug, Clone)]
pub enum ForDirection {
    To,
    DownTo,
}

#[derive(Debug, Clone)]
pub struct Case {
    pub labels: Vec<Expr>,
    pub body: MaybeLabelledStmt,
}
