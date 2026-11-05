use crate::{
    ast::{Ident, UnspanIdent, stmt},
    sema::context::{Constant, DefId, OrdinalTypeId, ParamId, ProcId, TypeId, WithRecordVarId},
    tast::{
        Typed,
        expr::{Arg, Place, TypedExpr},
    },
};

#[derive(Debug, Clone)]
pub struct MaybeLabelledStmt {
    pub label: Option<u16>,
    pub stmt: Stmt,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Empty,
    Assign {
        var: Typed<Place>,
        value: TypedExpr,
    },
    SetReturn(TypedExpr),
    // One or more
    ReadCall(Vec<Place>),
    // Zero or more
    ReadlnCall(Vec<Place>),
    // One or more
    WriteCall(Vec<WriteParam>),
    // Zero or more
    WritelnCall(Vec<WriteParam>),
    ProcCall {
        name: Ident,
        def: ProcId,
        args: Vec<(ParamId, Arg)>,
    },
    Goto(u16),
    Compound(Vec<MaybeLabelledStmt>),
    If {
        cond: TypedExpr,
        then: Box<MaybeLabelledStmt>,
        r#else: Option<Box<MaybeLabelledStmt>>,
    },
    Case {
        index: TypedExpr,
        cases: Vec<Case>,
    },
    While {
        cond: TypedExpr,
        body: Box<MaybeLabelledStmt>,
    },
    Repeat {
        body: Vec<MaybeLabelledStmt>,
        cond: TypedExpr,
    },
    For {
        control_var: (OrdinalTypeId, UnspanIdent),
        control_var_def: DefId,
        from: TypedExpr,
        direction: ForDirection,
        to: TypedExpr,
        body: Box<MaybeLabelledStmt>,
    },
    With {
        // TODO: Refine `TypedExpr` into `Typed<Place>` given the fact that it cannot possibly be a constant or of an
        // enumerated type
        vars: Vec<(TypedExpr, WithRecordVarId)>,
        body: Box<MaybeLabelledStmt>,
    },
}

#[derive(Debug, Clone)]
pub struct WriteParam {
    pub param: TypedExpr,
    pub specs: Option<WriteParamSpecs>,
}

#[derive(Debug, Clone)]
pub struct WriteParamSpecs {
    pub field_width: TypedExpr,
    pub frac_digits: Option<TypedExpr>,
}

#[derive(Debug, Clone, Copy)]
pub enum ForDirection {
    To,
    DownTo,
}

impl From<stmt::ForDirection> for ForDirection {
    fn from(value: stmt::ForDirection) -> Self {
        match value {
            stmt::ForDirection::To => Self::To,
            stmt::ForDirection::DownTo => Self::DownTo,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Case {
    pub labels: Vec<(TypeId, Constant)>,
    pub body: MaybeLabelledStmt,
}
