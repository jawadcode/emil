use crate::{
    ast::UnspanIdent,
    sema::{
        builtins::BuiltinVar,
        context::{
            DefId, EnumMemberId, FieldId, FuncId, ParamId, ProcId, StringId, TypeId,
            WithRecordVarId,
        },
    },
    tast::Typed,
};

pub type TypedExpr = Typed<Expr>;

#[derive(Debug, Clone)]
pub enum Expr {
    Nil,
    True,
    False,
    IntLit(i64),
    RealLit(f64),
    CharLit(u8),
    StrLit(StringId),
    Place(Place),
    Enum(i64, EnumMemberId),
    IntToReal(Box<TypedExpr>),
    Set(Vec<SetMember>),
    // TODO: Rethink whether it's necessary to bundle reference to parameter
    FuncCall { name: UnspanIdent, def: FuncId, args: Vec<(ParamId, Arg)> },
    UnaryOp { op: UnaryOp, operand: Box<TypedExpr> },
    BinOp { op: BinOp, left: Box<TypedExpr>, right: Box<TypedExpr> },
}

#[derive(Debug, Clone)]
pub enum Arg {
    Value(TypedExpr),
    Var(Typed<Place>),
    Proc(UnspanIdent, ProcId),
    Func(UnspanIdent, FuncId),
}

// Name is Rust lingo, see: https://without.boats/blog/pinned-places#places-and-values
#[derive(Debug, Clone)]
pub enum Place {
    Builtin(BuiltinVar),
    Plain { name: UnspanIdent, def: DefId },
    ValueParam { name: UnspanIdent, param: ParamId },
    VarParam { name: UnspanIdent, param: ParamId },
    Deref { referent: Box<Self>, domain_type: TypeId },
    File { var: Box<Self>, component_type: TypeId },
    Indexed { array_var: Box<Self>, indices: Vec<TypedExpr> },
    Field { record_var: Box<Self>, field_name: UnspanIdent, record: TypeId, field: FieldId },
    WithFieldAlias { name: UnspanIdent, record_var: WithRecordVarId, field: FieldId },
}

#[derive(Debug, Clone)]
pub enum SetMember {
    One(TypedExpr),
    Range { start: TypedExpr, end: TypedExpr },
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    BNot,
    INeg,
    RNeg,
}

// This feels dumb
#[derive(Debug, Clone, Copy)]
pub enum BinOp {
    IAdd,
    ISub,
    IMul,

    IDiv,
    IMod,

    Union,
    Diff,
    Inter,

    RAdd,
    RSub,
    RMul,
    RQuot,

    BAnd,
    BOr,

    IEq,
    INEq,
    ILT,
    IGT,
    ILEq,
    IGEq,

    REq,
    RNEq,
    RLT,
    RGT,
    RLEq,
    RGEq,

    BEq,
    BNEq,
    BLT,
    BGT,
    BLEq, // Interestingly this is the same as `p implies q` or `(not p) or q`, a shame the arrow is backwards though
    BGEq,

    CEq,
    CNEq,
    CLT,
    CGT,
    CLEq,
    CGEq,

    EEq,
    ENEq,
    ELT,
    EGT,
    ELEq,
    EGEq,

    SubEq,
    SubNEq,
    SubLT,
    SubGT,
    SubLEq,
    SubGEq,

    PtrEq,
    PtrNEq,

    StrEq,
    StrNEq,
    StrLT,
    StrGT,
    StrLEq,
    StrGEq,

    SetEq,
    SetNEq,
    SetLEq,
    SetGEq,
    SetIn,
}
