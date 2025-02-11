use super::expr::{Expr, Var};

#[derive(Debug, Clone)]
pub struct MaybeLabelledStmt<'source> {
    label: Option<u64>,
    stmt: Box<Stmt<'source>>,
}

#[derive(Debug, Clone)]
pub enum Stmt<'source> {
    Empty,
    Assign {
        var: Var<'source>,
        value: Expr<'source>,
    },
    Proc {
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
        control_var: Var<'source>,
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
pub enum ForDirection {
    To,
    DownTo,
}

#[derive(Debug, Clone)]
pub struct Case<'source> {
    labels: Vec<Expr<'source>>,
    body: MaybeLabelledStmt<'source>,
}

#[derive(Debug, Clone)]
pub enum Params<'source> {
    Actual(Vec<Expr<'source>>),
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

pub type CompoundStmt<'source> = Vec<MaybeLabelledStmt<'source>>;
