use crate::{
    ast::UnspanIdent,
    sema::context::{ConstDefId, Constant, DefId, ParamId, ProgramParamId, TypeId},
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

#[derive(Debug, Clone)]
pub enum RoutineDecl {
    ProcFwd(ProcSig),
    Proc { sig: ProcSig, body: Block },
    FuncFwd(FuncSig),
    Func { sig: FuncSig, body: Block },
}

// Decided to flatten parameter sections

#[derive(Debug, Clone)]
pub struct ProcSig {
    pub name: UnspanIdent,
    pub def: DefId,
    pub params: Vec<ParamId>,
}

#[derive(Debug, Clone)]
pub struct FuncSig {
    pub name: UnspanIdent,
    pub def: DefId,
    pub params: Vec<ParamId>,
    pub result: TypeId,
}
