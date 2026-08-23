use std::collections::HashMap;

use crate::{
    ast::{self, program::SubrangeBound, UnspanIdent},
    utils::{Span, Spanned},
};

use super::{
    builtins::{BuiltinConst, BuiltinFunc, BuiltinProc, BuiltinType, BuiltinVar},
    AnalysisError, AnalysisResult,
};
use ast::program as p;

pub struct TypingContext {
    types: Vec<TypeKind>,
    defs: Vec<DefPoint>,
    canonical_sets: HashMap<(OrdinalTypeId, bool), TypeId>,
    pub scopes: Vec<Scope>,
    // This feels jank
    integer: OrdinalTypeId,
    real: TypeId,
    boolean: OrdinalTypeId,
    char: OrdinalTypeId,
    text: TypeId,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct TypeId(usize);

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct OrdinalTypeId(usize);

impl From<OrdinalTypeId> for TypeId {
    fn from(value: OrdinalTypeId) -> Self {
        TypeId(value.0)
    }
}

pub enum Constant {
    Int(i64),
    Bool(bool),
    Char(char),
    Enum {
        r#enum: TypeId,
        member_idx: usize,
    },
    Ident {
        is_pos: bool,
        name: UnspanIdent,
        r#type: TypeId,
    },
}

pub enum TypeKind {
    Enumerated {
        members: Vec<EnumMemberId>,
    },
    Subrange {
        host_ty: OrdinalTypeId,
        lower: i64, // All possible subrange bounds can be represented as an integer,
        upper: i64, // reals are not an ordinal-type and thus are not allowed.
    },
    Integer,
    Real,
    Boolean,
    Char,
    Text,
    // Ident(UnspanIdent),
    Array {
        indices: Vec<OrdinalTypeId>,
        elem: TypeId,
    },
    Record {
        fixed: Vec<FieldId>,
        variant: Option<VariantPart>,
    },
    Set {
        packed: bool,
        elem: OrdinalTypeId,
    },
    File(TypeId),
    Pointer(TypeId),
}

pub struct VariantPart {
    tag_field: Option<UnspanIdent>,
    tag_type: TypeId,
    variants: Vec<Variant>,
}

pub struct Variant {
    case_labels: Vec<Constant>,
    fields: FieldList,
}

pub struct FieldList {
    fixed: Vec<FieldId>,
    variant: Option<VariantPart>,
}

#[derive(Default)]
pub struct Scope {
    labels: HashMap<u16, Span>,
    idents: HashMap<UnspanIdent, DefId>,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct DefId(usize);

/* 'Subtype's of `DefId` */
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct FieldId(usize);

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct EnumMemberId(usize);

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct ParamId(usize);

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct ConformArrayBoundId(usize);

/// The `defining-point` per the standard
pub enum DefPoint {
    BuiltinConst(BuiltinConst),
    BuiltinType(BuiltinType),
    BuiltinVar(BuiltinVar),
    BuiltinProc(BuiltinProc),
    BuiltinFunc(BuiltinFunc),
    UserDef { kind: DefKind, span: Span },
}

pub enum DefKind {
    ProgramParam,
    Const { r#type: TypeId, value: Constant },
    Type(TypeId),
    Var(TypeId),
    Label(u16),
    Field { record: TypeId, r#type: TypeId },
    // ordinal would ideally be a u32/u64 but we want it to line up with the
    // type of subrange bounds which are i64
    EnumMember { r#type: TypeId, ordinal: i64 },
    Proc(ProcSig),
    Func(FuncSig),
    Param(ParamKind),
    ConformArrayBound(OrdinalTypeId), /* conformant array bound */
}

pub enum ParamKind {
    Value(Vec<TypeId>, ParamType),
    Var(Vec<TypeId>, ParamType),
    Proc(ProcSig),
    Func(FuncSig),
}

pub enum ParamType {
    TypeIdent(TypeId),
    ArraySchema(ArraySchema),
}

pub enum ArraySchema {
    Packed { index: IndexTypeSpec, elem: TypeId },
    Unpacked { indices: Vec<IndexTypeSpec> },
}

pub struct IndexTypeSpec {
    lower: i64,
    upper: i64,
    host_type: OrdinalTypeId,
}

pub struct ProcSig {
    params: Vec<ParamId>,
    // so we can impl forward-declaration
    has_body: bool,
}

pub struct FuncSig {
    params: Vec<ParamId>,
    result_type: TypeId,
    // so we can impl forward-declaration
    has_body: bool,
}

impl TypingContext {
    pub fn new(idents: impl Iterator<Item = (UnspanIdent, DefPoint)>) -> Self {
        let types = Vec::from([
            TypeKind::Integer,
            TypeKind::Real,
            TypeKind::Boolean,
            TypeKind::Char,
            TypeKind::Text,
        ]);

        let (integer, real, boolean, char, text) = (
            OrdinalTypeId(0),
            TypeId(1),
            OrdinalTypeId(2),
            OrdinalTypeId(3),
            TypeId(4),
        );

        let mut defs = Vec::new();

        let idents = idents
            .map(|(name, def)| {
                defs.push(def);
                (name, DefId(defs.len() - 1))
            })
            .collect();

        Self {
            types,
            defs,
            canonical_sets: HashMap::new(),
            scopes: Vec::from([Scope {
                idents,
                ..Scope::default()
            }]),
            integer,
            real,
            boolean,
            char,
            text,
        }
    }

    // Instead of reserving a placeholder we just predict the TypeId using the length
    // of the arena, hopefully this doesn't come back to bite me :P
    fn convert_type(&mut self, value: ast::program::Type) -> Result<TypeId, AnalysisError> {
        match value {
            p::Type::Ordinal(ordinal_type) => match ordinal_type {
                p::OrdinalType::Enumerated(members) => {
                    let enum_ty = TypeId(self.types.len());
                    let members = members
                        .into_iter()
                        .enumerate()
                        .map(|(i, member)| {
                            self.insert_enum_member(member.node, member.span, enum_ty, i as i64)
                        })
                        .collect();
                    let ty = TypeKind::Enumerated { members };
                    Ok(self.fresh(ty))
                }
                p::OrdinalType::Subrange { lower, upper } => todo!(),
                p::OrdinalType::Ident(name) => todo!(),
            },
            p::Type::Structured { packed, r#type } => match r#type.node {
                p::UnpackedStructuredType::Array { indices, elem } => todo!(),
                p::UnpackedStructuredType::Record(field_list) => todo!(),
                p::UnpackedStructuredType::Set(elem) => todo!(),
                p::UnpackedStructuredType::File(r#type) => todo!(),
            },
            p::Type::Pointer(pointee) => todo!(),
            p::Type::Ident(spur) => todo!(),
        }
    }

    fn check_subrange(
        &self,
        lower: Spanned<&SubrangeBound>,
        upper: Spanned<&SubrangeBound>,
        subr_span: Span,
    ) -> Result<OrdinalTypeId, AnalysisError> {
        let lower_type = self.infer_subrange_bound(lower, subr_span)?;
        let upper_type = self.infer_subrange_bound(upper, subr_span)?;

        self.check_assign_compat(lower_type.into(), upper_type.into(), subr_span)
            .map(|_| lower_type)
    }

    const ORD_MSG: &'static str = "constant or literal of ordinal-type";

    // TODO: Implement `lower < upper` check
    fn infer_subrange_bound(
        &self,
        bound: Spanned<&SubrangeBound>,
        subr_span: Span,
    ) -> Result<OrdinalTypeId, AnalysisError> {
        match bound.node.lit {
            p::SubrangeBoundLiteral::UIntLit(_num) => Ok(self.integer),
            p::SubrangeBoundLiteral::Ident(name) => {
                let def = self.lookup(name, bound.span)?;

                match &self.defs[def.0] {
                    DefPoint::BuiltinConst(BuiltinConst::True | BuiltinConst::False) => {
                        Ok(self.boolean)
                    }
                    DefPoint::BuiltinConst(BuiltinConst::Maxint) => Ok(self.integer),
                    DefPoint::UserDef {
                        kind: DefKind::Const { r#type, value: _ },
                        span,
                    } => self.check_bound_ordinality(*r#type, *span, subr_span),
                    DefPoint::UserDef {
                        kind: DefKind::EnumMember { r#type, ordinal: _ },
                        span,
                    } => Ok(OrdinalTypeId(r#type.0)),
                    _ => {
                        return Err(AnalysisError::MismatchDef {
                            got: def,
                            at: bound.span,
                            expected: Self::ORD_MSG,
                            because: subr_span,
                        })
                    }
                }
            }
        }
    }

    fn check_bound_ordinality(
        &self,
        r#type: TypeId,
        bound_span: Span,
        subr_span: Span,
    ) -> AnalysisResult<OrdinalTypeId> {
        let id = r#type.0;
        match self.types[id] {
            // It's not possible for a subrange bound identifier to be of another subrange-type because the type of a constant can
            // never be of subrange-type without a type annotation, which is non-standard
            TypeKind::Enumerated { members: _ }
            | TypeKind::Integer
            | TypeKind::Boolean
            | TypeKind::Char => Ok(OrdinalTypeId(id)),
            _ => {
                return Err(AnalysisError::MismatchType {
                    got: r#type,
                    at: bound_span,
                    expected: Self::ORD_MSG,
                    because: subr_span,
                })
            }
        }
    }

    pub fn fresh(&mut self, kind: TypeKind) -> TypeId {
        match kind {
            TypeKind::Integer => self.integer.into(),
            TypeKind::Real => self.real,
            TypeKind::Boolean => self.boolean.into(),
            TypeKind::Char => self.char.into(),
            TypeKind::Text => self.text,
            // Deduplicate set-types based on element type and packedness as per ze standard
            kind @ TypeKind::Set { packed, elem } => {
                if let Some(r#type) = self.canonical_sets.get(&(elem, packed)) {
                    *r#type
                } else {
                    self.types.push(kind);
                    let id = TypeId(self.types.len() - 1);
                    self.canonical_sets.insert((elem, packed), id);
                    id
                }
            }
            kind => {
                self.types.push(kind);
                TypeId(self.types.len() - 1)
            }
        }
    }

    /// Insert a definition
    ///
    /// # Panics
    ///
    ///
    pub fn insert(&mut self, name: UnspanIdent, kind: DefKind, span: Span) -> DefId {
        match kind {
            DefKind::ProgramParam
            | DefKind::Const {
                r#type: _,
                value: _,
            }
            | DefKind::Type(_)
            | DefKind::Var(_)
            | DefKind::Label(_)
            | DefKind::EnumMember {
                r#type: _,
                ordinal: _,
            }
            | DefKind::Proc(_)
            | DefKind::Func(_) => DefId(self.insert_def(name, kind, span)),
            _ => unreachable!(),
        }
    }

    pub fn check_assign_compat(&self, t1: TypeId, t2: TypeId, because: Span) -> AnalysisResult<()> {
        todo!()
    }

    pub fn insert_field(
        &mut self,
        name: UnspanIdent,
        span: Span,
        record: TypeId,
        r#type: TypeId,
    ) -> FieldId {
        let id = self.insert_def(name, DefKind::Field { record, r#type }, span);
        FieldId(id)
    }

    pub fn insert_enum_member(
        &mut self,
        name: UnspanIdent,
        span: Span,
        r#type: TypeId,
        ordinal: i64,
    ) -> EnumMemberId {
        let id = self.insert_def(name, DefKind::EnumMember { r#type, ordinal }, span);
        EnumMemberId(id)
    }

    pub fn insert_param(&mut self, name: UnspanIdent, span: Span, kind: ParamKind) -> ParamId {
        let id = self.insert_def(name, DefKind::Param(kind), span);
        ParamId(id)
    }

    pub fn insert_conform_array_bound(
        &mut self,
        name: UnspanIdent,
        span: Span,
        r#type: OrdinalTypeId,
    ) -> ConformArrayBoundId {
        let id = self.insert_def(name, DefKind::ConformArrayBound(r#type), span);
        ConformArrayBoundId(id)
    }

    fn insert_def(&mut self, name: UnspanIdent, kind: DefKind, span: Span) -> usize {
        self.defs.push(DefPoint::UserDef { kind, span });
        let id = self.defs.len() - 1;
        self.curr_scope_mut().idents.insert(name, DefId(id));
        id
    }

    pub fn lookup(&self, name: UnspanIdent, span: Span) -> AnalysisResult<DefId> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.idents.get(&name))
            .ok_or(AnalysisError::Unbound { name, at: span })
            .cloned()
    }

    pub fn curr_scope(&self) -> &Scope {
        self.scopes.last().expect("Expected at least one scope")
    }

    pub fn curr_scope_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().expect("Expected at least one scope")
    }
}
