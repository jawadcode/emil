use crate::{
    ast::{expr::Expr, stmt::CompoundStmt, Ident},
    utils::Spanned,
};

#[derive(Debug, Clone)]
pub struct Program {
    pub name: Ident,
    pub params: Spanned<Vec<Ident>>,
    pub block: Spanned<Block>,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub label_decls: Spanned<Vec<Spanned<u64>>>,
    pub const_defs: Spanned<Vec<ConstDef>>,
    pub type_defs: Spanned<Vec<TypeDef>>,
    pub var_decls: Spanned<Vec<VarDecl>>,
    pub routine_decls: Spanned<Vec<Spanned<RoutineDecl>>>,
    pub stmts: Spanned<CompoundStmt>,
}

#[derive(Debug, Clone)]
pub struct ConstDef {
    name: Ident,
    value: Expr,
}

#[derive(Debug, Clone)]
pub struct TypeDef {
    name: Ident,
    def: Spanned<Type>,
}

#[derive(Debug, Clone)]
pub struct VarDecl {
    names: Vec<Ident>,
    r#type: Spanned<Type>,
}

#[derive(Debug, Clone)]
pub enum Type {
    Ordinal(OrdinalType),
    Structured {
        packed: bool,
        r#type: Box<UnpackedStructuredType>,
    },
    Pointer(Ident),
}

#[derive(Debug, Clone)]
pub enum OrdinalType {
    Enumerated(Vec<Ident>),
    Subrange {
        lower: Spanned<Expr>,
        upper: Spanned<Expr>,
    },
    Identifier(Ident),
}

#[derive(Clone, Debug)]
pub enum UnpackedStructuredType {
    Array {
        indices: Vec<OrdinalType>,
        elem: Type,
    },
    Record(FieldList),
    Set(OrdinalType),
    File(Type),
}

#[derive(Clone, Debug)]
pub enum FieldList {
    FixedOnly(FixedPart),
    Both(FixedPart, VariantField),
    VariantOnly(VariantField),
    Empty,
}

type FixedPart = Vec<(Vec<Ident>, Type)>;

#[derive(Clone, Debug)]
pub struct VariantField {
    pub tag_field: Option<Ident>,
    pub tag_type: Ident,
    pub variants: Vec<Variant>,
}

#[derive(Clone, Debug)]
pub struct Variant {
    pub case_labels: Vec<Expr>,
    pub fields: FieldList,
}

#[derive(Clone, Debug)]
pub enum RoutineDecl {
    Proc(ProcDecl),
    Func(FuncDecl),
}

#[derive(Debug, Clone)]
pub enum PostSig {
    Block(Block),
    Directive(Directive),
}

#[derive(Clone, Debug)]
pub struct ProcDecl {
    pub sig: ProcSig,
    pub post: PostSig,
}

#[derive(Clone, Debug)]
pub struct ProcSig {
    pub name: Ident,
    /// params.is_empty() => procedure identification
    /// otherwise         => procedure heading
    pub params: Vec<Param>,
}

#[derive(Debug, Clone)]
pub enum FuncDecl {
    Ident(Ident, Block),
    Heading(FuncSig, PostSig),
}

#[derive(Debug, Clone)]
pub struct FuncSig {
    pub name: Ident,
    pub params: Vec<Param>,
    pub result: Ident,
}

#[derive(Debug, Clone)]
pub enum Directive {
    Forward,
    External,
    Unknown(Ident),
}

#[derive(Clone, Debug)]
pub enum Param {
    Value(Vec<Ident>, ParamType),
    Var(Vec<Ident>, ParamType),
    Proc(ProcSig),
    Func(FuncSig),
}

#[derive(Clone, Debug)]
pub enum ParamType {
    TypeIdent(Ident),
    ArraySchema(Box<ArraySchema>),
}

#[derive(Clone, Debug)]
pub enum ArraySchema {
    Packed {
        index: IndexTypeSpec,
        elem: Ident,
    },
    Unpacked {
        indices: Vec<IndexTypeSpec>,
        elem: ParamType,
    },
}

#[derive(Clone, Debug)]
pub struct IndexTypeSpec {
    pub lower: Ident,
    pub upper: Ident,
    pub r#type: Ident,
}
