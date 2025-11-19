use crate::{
    ast::{
        expr::{Expr, SpanExpr},
        stmt::CompoundStmt,
        Ident, UnspanIdent,
    },
    utils::{Span, Spanned},
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
    pub const_defs: Spanned<Vec<Spanned<ConstDef>>>,
    pub type_defs: Spanned<Vec<Spanned<TypeDef>>>,
    pub var_decls: Spanned<Vec<Spanned<VarDecl>>>,
    pub routine_decls: Spanned<Vec<Spanned<RoutineDecl>>>,
    pub stmts: Spanned<Spanned<CompoundStmt>>,
}

#[derive(Debug, Clone)]
pub struct ConstDef {
    pub name: Ident,
    pub value: SpanExpr,
}

#[derive(Debug, Clone)]
pub struct TypeDef {
    pub name: Ident,
    pub def: Spanned<Type>,
}

#[derive(Debug, Clone)]
pub struct VarDecl {
    pub names: Spanned<Vec<Ident>>,
    pub r#type: Spanned<Type>,
}

#[derive(Debug, Clone)]
pub enum Type {
    Ordinal(Spanned<OrdinalType>),
    Structured {
        packed: Option<Span>,
        r#type: Box<Spanned<UnpackedStructuredType>>,
    },
    Pointer(UnspanIdent),
}

#[derive(Debug, Clone)]
pub enum OrdinalType {
    Enumerated(Vec<Ident>),
    Subrange { lower: SpanExpr, upper: SpanExpr },
    Identifier(UnspanIdent),
}

#[derive(Clone, Debug)]
pub enum UnpackedStructuredType {
    Array {
        indices: Spanned<Vec<Spanned<OrdinalType>>>,
        elem: Spanned<Type>,
    },
    Record(FieldList),
    Set(OrdinalType),
    File(Type),
}

#[derive(Clone, Debug)]
pub enum FieldList {
    FixedOnly(FixedPart),
    Both(FixedPart, Spanned<VariantField>),
    VariantOnly(Spanned<VariantField>),
    Empty,
}

type FixedPart = Spanned<Vec<Spanned<FixedFields>>>;

#[derive(Clone, Debug)]
pub struct FixedFields {
    names: Spanned<Vec<Ident>>,
    r#type: Spanned<Type>,
}

#[derive(Clone, Debug)]
pub struct VariantField {
    pub tag_field: Option<Ident>,
    pub tag_type: Ident,
    pub variants: Spanned<Vec<Spanned<Variant>>>,
}

#[derive(Clone, Debug)]
pub struct Variant {
    pub case_labels: Spanned<Vec<SpanExpr>>,
    pub fields: Spanned<FieldList>,
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
    pub sig: Spanned<ProcSig>,
    pub post: Spanned<PostSig>,
}

#[derive(Clone, Debug)]
pub struct ProcSig {
    pub name: Ident,
    /// params.is_empty() => procedure identification
    /// otherwise         => procedure heading
    pub params: Vec<Spanned<Spanned<Param>>>,
}

#[derive(Debug, Clone)]
pub enum FuncDecl {
    Ident(Ident, Spanned<Block>),
    Heading(Spanned<FuncSig>, Spanned<PostSig>),
}

#[derive(Debug, Clone)]
pub struct FuncSig {
    pub name: Ident,
    pub params: Spanned<Vec<Spanned<Param>>>,
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
    Value(Spanned<Vec<Ident>>, Spanned<Box<Param>>),
    Var(Spanned<Vec<Ident>>, Spanned<Vec<Ident>>),
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
        index: Spanned<IndexTypeSpec>,
        elem: Ident,
    },
    Unpacked {
        indices: Spanned<Vec<Spanned<IndexTypeSpec>>>,
        elem: Spanned<ParamType>,
    },
}

#[derive(Clone, Debug)]
pub struct IndexTypeSpec {
    pub lower: Ident,
    pub upper: Ident,
    pub r#type: Ident,
}
