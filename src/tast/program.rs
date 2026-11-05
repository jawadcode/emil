use crate::{
    ast::UnspanIdent,
    sema::context::{ConstDefId, Constant, DefId, FuncId, ParamId, ProcId, ProgramParamId, TypeId},
    tast::stmt::MaybeLabelledStmt,
};

#[derive(Debug, Clone)]
pub struct Program {
    pub name: UnspanIdent,
    pub params: Vec<ProgramParamId>,
    pub block: Block,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub label_decls: Vec<u16>,
    pub const_defs: Vec<ConstDef>,
    pub type_defs: Vec<TypeDef>,
    pub var_decls: Vec<VarDecl>,
    pub routine_decls: Vec<RoutineDecl>,
    pub stmts: Vec<MaybeLabelledStmt>,
}

#[derive(Debug, Clone)]
pub struct ConstDef {
    pub name: UnspanIdent,
    pub def: ConstDefId,
    pub value: Constant,
    pub r#type: TypeId,
}

#[derive(Debug, Clone)]
pub struct TypeDef {
    pub name: UnspanIdent,
    pub def: TypeId,
}

#[derive(Debug, Clone)]
pub struct VarDecl {
    pub name: UnspanIdent,
    pub def: DefId,
    pub r#type: TypeId,
}

/* Since this is a fairly unambitious  */

#[derive(Debug, Clone)]
pub enum RoutineDecl {
    /// A forwarded procedure declaration
    ProcFwd(ProcSig),
    /// Corresponding implementation for a forwarded procedure declaration
    ProcImpl { name: UnspanIdent, def: ProcId, body: Block },
    /// A standalone procedure declaration
    WholeProc { sig: ProcSig, body: Block },

    /// A forwarded function declaration
    FuncFwd(FuncSig),
    /// Corresponding implementation for a forwarded function declaration
    FuncImpl { name: UnspanIdent, def: FuncId, body: Block },
    /// A standalone function declaration
    WholeFunc { sig: FuncSig, body: Block },
}

// Decided to flatten parameter sections

#[derive(Debug, Clone)]
pub struct ProcSig {
    pub name: UnspanIdent,
    pub def: ProcId,
    pub params: Vec<ParamId>,
}

#[derive(Debug, Clone)]
pub struct FuncSig {
    pub name: UnspanIdent,
    pub def: FuncId,
    pub params: Vec<ParamId>,
    pub result: TypeId,
}
