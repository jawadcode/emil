use super::{expr::Expr, stmt::CompoundStmt, ParseResult, Parser};

#[derive(Debug, Clone)]
pub struct Program<'source> {
    pub name: &'source str,
    pub params: Vec<&'source str>,
    pub block: Block<'source>,
}

pub fn program<'source>(_parser: &mut Parser<'source>) -> ParseResult<Program<'source>> {
    todo!()
}

#[derive(Debug, Clone)]
pub struct Block<'source> {
    label_decls: Vec<u64>,
    const_defs: Vec<(&'source str, Expr<'source>)>,
    type_defs: Vec<(&'source str, Type<'source>)>,
    var_decls: Vec<(Vec<&'source str>, Type<'source>)>,
    routine_decls: Vec<RoutineDecl<'source>>,
    stmts: CompoundStmt<'source>,
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
pub struct FieldList<'source> {
    fixed: Vec<(Vec<&'source str>, Type<'source>)>,
    variant: Option<VariantType<'source>>,
}

#[derive(Clone, Debug)]
pub struct VariantType<'source> {
    tag_field: Option<&'source str>,
    tag_type: &'source str,
    variants: Vec<Variant<'source>>,
}

#[derive(Clone, Debug)]
pub struct Variant<'source> {
    case_labels: Vec<Expr<'source>>,
    fields: FieldList<'source>,
}

#[derive(Clone, Debug)]
pub enum RoutineDecl<'source> {
    Proc(ProcDecl<'source>),
    Func(FuncDecl<'source>),
}

#[derive(Clone, Debug)]
pub enum ProcDecl<'source> {
    Heading(ProcHeading<'source>, PostHeading<'source>),
    Id(&'source str, Block<'source>),
}

#[derive(Clone, Debug)]
pub struct ProcHeading<'source> {
    name: &'source str,
    params: Vec<Param<'source>>,
}

#[derive(Debug, Clone)]
pub enum FuncDecl<'source> {
    Heading(FuncHeading<'source>, PostHeading<'source>),
    Id {
        name: &'source str,
        block: Block<'source>,
    },
}

#[derive(Debug, Clone)]
pub struct FuncHeading<'source> {
    name: &'source str,
    params: Vec<Param<'source>>,
    result_type: &'source str,
}

#[derive(Debug, Clone)]
pub enum PostHeading<'source> {
    Block(Block<'source>),
    Directive(Directive<'source>),
}

#[derive(Debug, Clone)]
pub enum Directive<'source> {
    Forward,
    Extern,
    Unknown(&'source str),
}

#[derive(Clone, Debug)]
pub enum Param<'source> {
    Value(Vec<&'source str>, ParamType<'source>),
    Var(Vec<&'source str>, ParamType<'source>),
    Proc(ProcHeading<'source>),
    Func(FuncHeading<'source>),
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
        element: &'source str,
    },
    Unpacked {
        indices: Vec<IndexTypeSpec<'source>>,
        element: ParamType<'source>,
    },
}

#[derive(Clone, Debug)]
pub struct IndexTypeSpec<'source> {
    start: &'source str,
    end: &'source str,
    r#type: &'source str,
}
