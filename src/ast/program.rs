use std::str::FromStr;

use crate::{
    ast::{stmt::CompoundStmt, Ident, UnspanIdent},
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
    pub label_decls: Spanned<Vec<Spanned<u16>>>,
    pub const_defs: Spanned<Vec<Spanned<ConstDef>>>,
    pub type_defs: Spanned<Vec<Spanned<TypeDef>>>,
    pub var_decls: Spanned<Vec<Spanned<VarDecl>>>,
    pub routine_decls: Spanned<Vec<Spanned<RoutineDecl>>>,
    pub stmts: Spanned<CompoundStmt>,
}

#[derive(Debug, Clone)]
pub struct ConstDef {
    pub name: Ident,
    pub value: Spanned<ConstExpr>,
}

#[derive(Debug, Clone)]
pub enum ConstExpr {
    NumLitOrIdent {
        is_pos: Option<bool>,
        lit: ConstExprLit,
    },
    StrLit(String),
}

#[derive(Debug, Clone)]
pub enum ConstExprLit {
    UIntLit(u64),
    URealLit(f64),
    Ident(UnspanIdent),
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
    Ordinal(OrdinalType),
    Structured {
        packed: Option<Span>,
        r#type: Box<Spanned<UnpackedStructuredType>>,
    },
    Pointer(UnspanIdent),
    Ident(UnspanIdent),
}

#[derive(Debug, Clone)]
pub enum OrdinalType {
    Enumerated(Vec<Ident>),
    Subrange {
        lower: Spanned<SubrangeBound>,
        upper: Spanned<SubrangeBound>,
    },
    Ident(UnspanIdent),
}

// needs to be distinct from `ConstExpr` as this does not include real number literals
#[derive(Debug, Clone)]
pub struct SubrangeBound {
    pub is_pos: Option<bool>,
    pub lit: SubrangeBoundLiteral,
}

#[derive(Debug, Clone)]
pub enum SubrangeBoundLiteral {
    UIntLit(u64),
    Ident(UnspanIdent),
}

#[derive(Clone, Debug)]
pub enum UnpackedStructuredType {
    Array {
        indices: Spanned<Vec<Spanned<OrdinalType>>>,
        elem: Spanned<Type>,
    },
    Record(FieldList),
    Set(OrdinalType),
    File(Spanned<Type>),
}

#[derive(Clone, Debug)]
pub enum FieldList {
    FixedOnly(Vec<Spanned<FixedFields>>),
    Both(FixedPart, Spanned<VariantField>),
    VariantOnly(VariantField),
    Empty,
}

type FixedPart = Spanned<Vec<Spanned<FixedFields>>>;

#[derive(Clone, Debug)]
pub struct FixedFields {
    pub names: Spanned<Vec<Ident>>,
    pub r#type: Spanned<Type>,
}

#[derive(Clone, Debug)]
pub struct VariantField {
    pub tag_field: Option<Ident>,
    pub tag_type: Ident,
    pub variants: Spanned<Vec<Spanned<Variant>>>,
}

#[derive(Clone, Debug)]
pub struct Variant {
    pub case_labels: Spanned<Vec<Spanned<ConstExpr>>>,
    pub fields: Spanned<FieldList>,
}

#[derive(Clone, Debug)]
pub enum RoutineDecl {
    Proc(ProcDecl),
    Func(FuncDecl),
}

#[allow(clippy::large_enum_variant)] // idc not introducing indirection here
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
    /// params.is_empty() => procedure identification \/ nullary procedure
    /// otherwise         => procedure heading
    pub params: Vec<Spanned<Param>>,
}

#[derive(Debug, Clone)]
pub enum FuncDecl {
    Ident(Ident, Spanned<Block>),
    Heading(Spanned<FuncSig>, Spanned<PostSig>),
}

impl FuncDecl {
    pub fn get_name(&self) -> &Ident {
        match self {
            FuncDecl::Ident(name, _)
            | FuncDecl::Heading(
                Spanned {
                    span: _,
                    node:
                        FuncSig {
                            name,
                            params: _,
                            result: _,
                        },
                },
                _,
            ) => name,
        }
    }
}

#[derive(Debug, Clone)]
pub struct FuncSig {
    pub name: Ident,
    pub params: Spanned<Vec<Spanned<Param>>>,
    pub result: Ident,
}

impl FuncSig {
    fn arity(&self) -> usize {
        let mut count = 0;
        for param in &self.params.node {
            count += match &param.node {
                Param::Value(names, _) => names.node.len(),
                Param::Var(names, _) => names.node.len(),
                Param::Proc(_) => 1,
                Param::Func(_) => 1,
            };
        }
        count
    }
}

#[derive(Debug, Clone)]
pub enum Directive {
    Forward,
    External,
}

impl FromStr for Directive {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let lower = s.to_lowercase();
        match lower.as_str() {
            "forward" => Ok(Self::Forward),
            "external" => Ok(Self::External),
            _ => Err(lower),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Param {
    Value(Spanned<Vec<Ident>>, Spanned<ParamType>),
    Var(Spanned<Vec<Ident>>, Spanned<ParamType>),
    Proc(ProcSig),
    Func(FuncSig),
}

#[derive(Clone, Debug)]
pub enum ParamType {
    TypeIdent(UnspanIdent),
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
