use crate::ast::expr::{Expr, Params, Var};

pub type CompoundStmt<'source> = Vec<MaybeLabelledStmt<'source>>;

#[derive(Debug, Clone)]
pub struct MaybeLabelledStmt<'source> {
    pub label: Option<u64>,
    pub stmt: Box<Stmt<'source>>,
}

#[derive(Debug, Clone)]
pub enum Stmt<'source> {
    Empty,
    Assign {
        var: Var<'source>,
        value: Expr<'source>,
    },
    // One or more
    ReadCall(Vec<Var<'source>>),
    // Zero or more
    ReadlnCall(Vec<Var<'source>>),
    // One or more
    WriteCall(Vec<WriteParam<'source>>),
    // Zero or more
    WritelnCall(Vec<WriteParam<'source>>),
    ProcCall {
        name: &'source str,
        params: Params<'source>,
    },
    Goto(u64),
    Compound(CompoundStmt<'source>),
    If {
        cond: Expr<'source>,
        then: MaybeLabelledStmt<'source>,
        r#else: Option<MaybeLabelledStmt<'source>>,
    },
    Case {
        index: Expr<'source>,
        cases: Vec<Case<'source>>,
    },
    While {
        cond: Expr<'source>,
        body: MaybeLabelledStmt<'source>,
    },
    Repeat {
        body: Vec<MaybeLabelledStmt<'source>>,
        cond: Expr<'source>,
    },
    For {
        control_var: &'source str,
        from: Expr<'source>,
        direction: ForDirection,
        to: Expr<'source>,
        body: MaybeLabelledStmt<'source>,
    },
    With {
        vars: Vec<Var<'source>>,
        body: MaybeLabelledStmt<'source>,
    },
}

#[derive(Debug, Clone)]
pub struct WriteParam<'source> {
    pub param: Expr<'source>,
    pub specifiers: Option<(
        /* field width */ Expr<'source>,
        Option</* fractional digits */ Expr<'source>>,
    )>,
}

#[derive(Debug, Clone)]
pub enum ForDirection {
    To,
    DownTo,
}

#[derive(Debug, Clone)]
pub struct Case<'source> {
    pub labels: Vec<Expr<'source>>,
    pub body: MaybeLabelledStmt<'source>,
}
