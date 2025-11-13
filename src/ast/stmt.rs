use crate::ast::{
    expr::{Expr, Params, Var},
    Ident,
};

pub type CompoundStmt = Vec<MaybeLabelledStmt>;

#[derive(Debug, Clone)]
pub struct MaybeLabelledStmt {
    pub label: Option<u64>,
    pub stmt: Box<Stmt>,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Empty,
    Assign {
        var: Var,
        value: Expr,
    },
    // One or more
    ReadCall(Vec<Var>),
    // Zero or more
    ReadlnCall(Vec<Var>),
    // One or more
    WriteCall(Vec<WriteParam>),
    // Zero or more
    WritelnCall(Vec<WriteParam>),
    ProcCall {
        name: Ident,
        params: Params,
    },
    Goto(u64),
    Compound(CompoundStmt),
    If {
        cond: Expr,
        then: MaybeLabelledStmt,
        r#else: Option<MaybeLabelledStmt>,
    },
    Case {
        index: Expr,
        cases: Vec<Case>,
    },
    While {
        cond: Expr,
        body: MaybeLabelledStmt,
    },
    Repeat {
        body: Vec<MaybeLabelledStmt>,
        cond: Expr,
    },
    For {
        control_var: Ident,
        from: Expr,
        direction: ForDirection,
        to: Expr,
        body: MaybeLabelledStmt,
    },
    With {
        vars: Vec<Var>,
        body: MaybeLabelledStmt,
    },
}

#[derive(Debug, Clone)]
pub struct WriteParam {
    pub param: Expr,
    pub specifiers: Option<(
        /* field width */ Expr,
        Option</* fractional digits */ Expr>,
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
