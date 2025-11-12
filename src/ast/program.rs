use crate::ast::{expr::Expr, stmt::CompoundStmt};

#[derive(Debug, Clone)]
pub struct Program<'source> {
    pub name: &'source str,
    pub params: Vec<&'source str>,
    pub block: Block<'source>,
}

#[derive(Debug, Clone)]
pub struct Block<'source> {
    pub label_decls: Vec<u64>,
    pub const_defs: Vec<(&'source str, Expr<'source>)>,
    pub type_defs: Vec<(&'source str, Type<'source>)>,
    pub var_decls: Vec<(Vec<&'source str>, Type<'source>)>,
    pub routine_decls: Vec<RoutineDecl<'source>>,
    pub stmts: CompoundStmt<'source>,
}

#[derive(Debug, Clone)]
pub enum Type<'source> {
    Ordinal(OrdinalType<'source>),
    Structured {
        packed: bool,
        r#type: Box<UnpackedStructuredType<'source>>,
    },
    Pointer(&'source str),
}

#[derive(Debug, Clone)]
pub enum OrdinalType<'source> {
    Enumerated(Vec<&'source str>),
    Subrange {
        lower: Expr<'source>,
        upper: Expr<'source>,
    },
    Identifier(&'source str),
}

#[derive(Clone, Debug)]
pub enum UnpackedStructuredType<'source> {
    Array {
        indices: Vec<OrdinalType<'source>>,
        elem: Type<'source>,
    },
    Record(FieldList<'source>),
    Set(OrdinalType<'source>),
    File(Type<'source>),
}

#[derive(Clone, Debug)]
pub enum FieldList<'source> {
    FixedOnly(FixedPart<'source>),
    Both(FixedPart<'source>, VariantField<'source>),
    VariantOnly(VariantField<'source>),
    Empty,
}

type FixedPart<'source> = Vec<(Vec<&'source str>, Type<'source>)>;

#[derive(Clone, Debug)]
pub struct VariantField<'source> {
    pub tag_field: Option<&'source str>,
    pub tag_type: &'source str,
    pub variants: Vec<Variant<'source>>,
}

#[derive(Clone, Debug)]
pub struct Variant<'source> {
    pub case_labels: Vec<Expr<'source>>,
    pub fields: FieldList<'source>,
}

#[derive(Clone, Debug)]
pub enum RoutineDecl<'source> {
    Proc(ProcDecl<'source>),
    Func(FuncDecl<'source>),
}

#[derive(Debug, Clone)]
pub enum PostSig<'source> {
    Block(Block<'source>),
    Directive(Directive<'source>),
}

#[derive(Clone, Debug)]
pub struct ProcDecl<'source> {
    pub sig: ProcSig<'source>,
    pub post: PostSig<'source>,
}

#[derive(Clone, Debug)]
pub struct ProcSig<'source> {
    pub name: &'source str,
    /// params.is_empty() => procedure identification
    /// otherwise         => procedure heading
    pub params: Vec<Param<'source>>,
}

#[derive(Debug, Clone)]
pub enum FuncDecl<'source> {
    Ident(&'source str, Block<'source>),
    Heading(FuncSig<'source>, PostSig<'source>),
}

#[derive(Debug, Clone)]
pub struct FuncSig<'source> {
    pub name: &'source str,
    pub params: Vec<Param<'source>>,
    pub result: &'source str,
}

#[derive(Debug, Clone)]
pub enum Directive<'source> {
    Forward,
    External,
    Unknown(&'source str),
}

impl<'source> From<&'source str> for Directive<'source> {
    fn from(other: &'source str) -> Self {
        match other.to_lowercase().as_str() {
            "forward" => Self::Forward,
            "external" => Self::External,
            _ => Self::Unknown(other),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Param<'source> {
    Value(Vec<&'source str>, ParamType<'source>),
    Var(Vec<&'source str>, ParamType<'source>),
    Proc(ProcSig<'source>),
    Func(FuncSig<'source>),
}

#[derive(Clone, Debug)]
pub enum ParamType<'source> {
    TypeIdent(&'source str),
    ArraySchema(Box<ArraySchema<'source>>),
}

#[derive(Clone, Debug)]
pub enum ArraySchema<'source> {
    Packed {
        index: IndexTypeSpec<'source>,
        elem: &'source str,
    },
    Unpacked {
        indices: Vec<IndexTypeSpec<'source>>,
        elem: ParamType<'source>,
    },
}

#[derive(Clone, Debug)]
pub struct IndexTypeSpec<'source> {
    pub lower: &'source str,
    pub upper: &'source str,
    pub r#type: &'source str,
}
