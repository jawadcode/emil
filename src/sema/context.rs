use std::{collections::HashMap, iter, ops::RangeInclusive};

use crate::{
    ast::{
        self, UnspanIdent,
        program::{ConstExpr, Param, SubrangeBound},
    },
    utils::{Span, Spanned},
};

use super::{
    AnalysisError, AnalysisResult,
    builtins::{BuiltinConst, BuiltinFunc, BuiltinProc, BuiltinType, BuiltinVar},
};
use ast::program as p;
use ast::program::{OrdinalType as AOrdTy, Type as ATy, UnpackedStructuredType as AUnpackStructTy};
use lasso::{Rodeo, Spur};

pub struct TypingContext {
    types: Vec<TypeKind>,
    defs: Vec<DefPoint>,
    strings: Rodeo,
    canonical_sets: HashMap<(OrdinalTypeId, bool), TypeId>,
    scopes: Vec<Scope>,
    // TODO: This feels jank, maybe move it into a separate struct
    pub integer: OrdinalTypeId,
    pub real: TypeId,
    pub boolean: OrdinalTypeId,
    pub char: OrdinalTypeId,
    pub text: TypeId,
    /// For error diags only!!!
    pub pointer_of_t: TypeId,
    /// For error diags only!!!
    pub set_of_t: TypeId,
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

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct StringId(Spur);

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Constant {
    Int(i64),
    Real(f64),
    Bool(bool),
    Char(char),
    Str(StringId),
    Enum { r#enum: TypeId, member_idx: usize },
    // Constants should be fully resolved on insertion so we shouldn't need this anymore
    // Ident {
    //     is_pos: bool,
    //     name: UnspanIdent,
    //     r#type: TypeId,
    // },
}

#[derive(Debug, Clone)]
pub enum TypeKind {
    Enumerated {
        members: Vec<EnumMemberId>,
    },
    Subrange {
        host_type: OrdinalTypeId,
        lower: i64, // All possible subrange bounds can be represented as an integer,
        upper: i64, // reals are not an ordinal-type and thus are not allowed.
    },
    Integer,
    Real,
    Boolean,
    Char,
    Text,
    // TypeKinds should be fully resolved on insertion so we shouldn't need this anymore
    // Ident(UnspanIdent),
    Array {
        packed: bool,
        indices: Vec<OrdinalTypeId>,
        elem: TypeId,
    },
    Record {
        packed: bool,
        fixed: Vec<FieldId>,
        variant: Option<VariantPart>,
    },
    Set {
        packed: bool,
        elem: OrdinalTypeId,
    },
    File(TypeId),
    Pointer(TypeId),

    /* FOR ERROR DIAGNOSTICS ONLY */
    // `nil` and `[]` are type-checked eagerly against an expected type
    PointerOfT,
    SetOfT,
}

#[derive(Debug, Clone)]
pub struct VariantPart {
    pub tag_field: Option<UnspanIdent>,
    pub tag_type: TypeId,
    pub variants: Vec<Variant>,
}

#[derive(Debug, Clone)]
pub struct Variant {
    pub case_labels: Vec<Constant>,
    pub fields: FieldList,
}

#[derive(Default, Debug, Clone)]
pub struct FieldList {
    pub fixed: Vec<FieldId>,
    pub variant: Option<VariantPart>,
}

#[derive(Default, Debug, Clone)]
pub struct Scope {
    labels: HashMap<u16, Span>,
    idents: HashMap<UnspanIdent, DefId>,
}

impl Scope {
    pub fn insert(&mut self, name: UnspanIdent, def: DefId) {
        self.idents.insert(name, def);
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct DefId(usize);

/* 'Subtype's of `DefId` */
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct FieldId(usize);

impl From<FieldId> for DefId {
    fn from(value: FieldId) -> Self {
        DefId(value.0)
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct EnumMemberId(usize);
impl From<EnumMemberId> for DefId {
    fn from(value: EnumMemberId) -> Self {
        DefId(value.0)
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct ParamId(usize);
impl From<ParamId> for DefId {
    fn from(value: ParamId) -> Self {
        DefId(value.0)
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct ConformArrayBoundId(usize);
impl From<ConformArrayBoundId> for DefId {
    fn from(value: ConformArrayBoundId) -> Self {
        DefId(value.0)
    }
}

/// A `defining-point` as per the standard
#[derive(Debug, Clone)]
pub enum DefPoint {
    BuiltinConst(BuiltinConst),
    BuiltinType(BuiltinType),
    BuiltinVar(BuiltinVar),
    BuiltinProc(BuiltinProc),
    BuiltinFunc(BuiltinFunc),
    UserDef { name: UnspanIdent, kind: DefKind, span: Span },
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub enum ParamKind {
    Value(ParamType),
    Var(ParamType),
    Proc(Vec<ParamSection>),
    Func { params: Vec<ParamSection>, result: TypeId },
}

#[derive(Debug, Clone)]
pub enum ParamType {
    TypeIdent(TypeId),
    ArraySchema(ArraySchema),
}

#[derive(Debug, Clone)]
pub enum ArraySchema {
    Packed { index: IndexTypeSpec, elem: TypeId },
    Unpacked { indices: Vec<IndexTypeSpec> },
}

#[derive(Debug, Clone)]
pub struct IndexTypeSpec {
    lower: i64,
    upper: i64,
    host_type: OrdinalTypeId,
}

#[derive(Debug, Clone)]
pub struct ProcSig {
    pub params: Vec<ParamSection>,
    /// For the purposes of forward-declaration
    pub has_body: bool,
}

impl ProcSig {
    pub fn params_iter(&self) -> ParamIter<'_> {
        ParamIter { params: &self.params, section_cursor: 0, param_cursor: 0 }
    }
}

#[derive(Debug, Clone)]
pub struct FuncSig {
    pub params: Vec<ParamSection>,
    pub result: TypeId,
    /// For the purposes of forward-declaration
    pub has_body: bool,
}

impl FuncSig {
    pub fn params_iter(&self) -> ParamIter<'_> {
        ParamIter { params: &self.params, section_cursor: 0, param_cursor: 0 }
    }
}

// Overkill but I don't want to waste allocations
pub struct ParamIter<'sig> {
    params: &'sig [ParamSection],
    section_cursor: usize,
    param_cursor: usize,
}

impl<'sig> ParamIter<'sig> {
    pub fn from(params: &'sig [ParamSection]) -> Self {
        ParamIter { params, section_cursor: 0, param_cursor: 0 }
    }
}

impl Iterator for ParamIter<'_> {
    type Item = ParamId;

    fn next(&mut self) -> Option<Self::Item> {
        if self.section_cursor < self.params.len() {
            match &self.params[self.section_cursor] {
                ParamSection::One(param_id) => {
                    self.section_cursor += 1;
                    Some(*param_id)
                }
                ParamSection::Many(param_ids) => {
                    if self.param_cursor == param_ids.len() {
                        self.section_cursor += 1;
                        self.param_cursor = 0;
                        self.next()
                    } else {
                        let param = param_ids[self.param_cursor];
                        self.param_cursor += 1;
                        Some(param)
                    }
                }
            }
        } else {
            None
        }
    }
}

// Little sanity check never hurts :P
#[test]
fn param_iter_test() {
    let sig = ProcSig {
        params: vec![
            ParamSection::One(ParamId(0)),
            ParamSection::Many(vec![ParamId(1), ParamId(2), ParamId(3)]),
            ParamSection::One(ParamId(4)),
        ],
        has_body: false,
    };
    let expected = [ParamId(0), ParamId(1), ParamId(2), ParamId(3), ParamId(4)];
    assert_eq!(sig.params_iter().collect::<Vec<_>>(), expected);
}

#[derive(Debug, Clone)]
pub enum ParamSection {
    One(ParamId),
    // TODO: Use a `smallvec::SmallVec`
    Many(Vec<ParamId>),
}

impl TypingContext {
    pub fn new(idents: impl Iterator<Item = (UnspanIdent, DefPoint)>) -> Self {
        let types = Vec::from([
            TypeKind::Integer,
            TypeKind::Real,
            TypeKind::Boolean,
            TypeKind::Char,
            TypeKind::Text,
            TypeKind::PointerOfT,
            TypeKind::SetOfT,
        ]);

        let (integer, real, boolean, char, text, pointer_of_t, set_of_t) = (
            OrdinalTypeId(0),
            TypeId(1),
            OrdinalTypeId(2),
            OrdinalTypeId(3),
            TypeId(4),
            TypeId(5),
            TypeId(6),
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
            strings: Rodeo::default(),
            canonical_sets: HashMap::new(),
            scopes: Vec::from([Scope { idents, ..Scope::default() }]),
            integer,
            real,
            boolean,
            char,
            text,
            pointer_of_t,
            set_of_t,
        }
    }

    // Instead of reserving a placeholder we just predict the TypeId using the length
    // of the arena, hopefully this doesn't come back to bite me :P
    pub fn convert_type(&mut self, ty: Spanned<&ATy>) -> Result<TypeId, AnalysisError> {
        match ty.node {
            ATy::Ordinal(ordinal_type) => {
                self.convert_ordinal_type(ordinal_type, ty.span).map(|ord| ord.into())
            }
            ATy::Structured { packed, r#type } => {
                let packed = packed.is_some();
                match &r#type.node {
                    AUnpackStructTy::Array { indices, elem } => self.convert_array_type(
                        packed,
                        indices.as_ref().map(AsRef::as_ref),
                        elem.as_ref(),
                        ty.span,
                    ),
                    AUnpackStructTy::Record(field_list) => {
                        self.convert_record_type(packed, field_list)
                    }
                    AUnpackStructTy::Set(elem) => self
                        .convert_ordinal_type(elem, ty.span)
                        .map(|elem| self.fresh(TypeKind::Set { packed, elem })),
                    AUnpackStructTy::File(r#type) => self
                        .convert_type(r#type.as_ref())
                        .map(|component| self.fresh(TypeKind::File(component))),
                }
            }
            ATy::Pointer(pointee) => {
                self.lookup_type(*pointee, ty.span).map(|ty| self.fresh(TypeKind::Pointer(ty)))
            }
            ATy::Ident(spur) => self.lookup_type(*spur, ty.span),
        }
    }

    fn convert_ordinal_type(
        &mut self,
        ordinal_type: &AOrdTy,
        ty_span: Span,
    ) -> AnalysisResult<OrdinalTypeId> {
        match ordinal_type {
            AOrdTy::Enumerated(members) => self.convert_enumerated_type(members),
            AOrdTy::Subrange { lower, upper } => {
                self.convert_subrange_type(lower.as_ref(), upper.as_ref(), ty_span)
            }
            AOrdTy::Ident(name) => self.convert_ordinal_type_ident(*name, ty_span),
        }
    }

    fn convert_enumerated_type(
        &mut self,
        members: &[Spanned<UnspanIdent>],
    ) -> AnalysisResult<OrdinalTypeId> {
        let enum_ty = TypeId(self.types.len());
        let members = members
            .iter()
            .enumerate()
            .map(|(i, member)| self.insert_enum_member(member.node, member.span, enum_ty, i as i64))
            .collect();
        let ty = TypeKind::Enumerated { members };

        Ok(OrdinalTypeId(self.fresh(ty).0))
    }

    fn convert_subrange_type(
        &mut self,
        lower: Spanned<&SubrangeBound>,
        upper: Spanned<&SubrangeBound>,
        ty_span: Span,
    ) -> AnalysisResult<OrdinalTypeId> {
        let (host_type, lower, upper) = self.check_subrange(lower, upper, ty_span)?;
        let ty = TypeKind::Subrange { host_type, lower, upper };

        Ok(OrdinalTypeId(self.fresh(ty).0))
    }

    fn check_subrange(
        &self,
        lower: Spanned<&SubrangeBound>,
        upper: Spanned<&SubrangeBound>,
        subr_span: Span,
    ) -> AnalysisResult<(OrdinalTypeId, i64, i64)> {
        let (lower_type, lower_ord) = self.resolve_subrange_bound(lower, subr_span)?;
        let (upper_type, upper_ord) = self.resolve_subrange_bound(upper, subr_span)?;

        if lower_ord > upper_ord {
            Err(AnalysisError::SubrangeBoundsBackwards {
                span: subr_span,
                start_ord: lower_ord,
                end_ord: upper_ord,
            })
        } else {
            // Due to the fact that Standard Pascal constant-definitions cannot have type annotations, it's not possible
            // for a constant to be of subrange-type or any kind of type alias (these would be resolved anyways) meaning
            // a simple `OrdinalTypeId` equality check is sufficient.
            if lower_type == upper_type {
                Ok((lower_type, lower_ord, upper_ord))
            } else {
                Err(AnalysisError::TypeMismatch {
                    got: upper_type.into(),
                    at: upper.span,
                    expected: lower_type.into(),
                    origin: subr_span,
                })
            }
        }
    }

    const ORD_MSG: &'static str =
        "constant or literal of type integer, boolean, char, or an enumerated-type member";

    fn resolve_subrange_bound(
        &self,
        bound: Spanned<&SubrangeBound>,
        subr_span: Span,
    ) -> Result<(OrdinalTypeId, i64), AnalysisError> {
        match bound.node.lit {
            p::SubrangeBoundLiteral::UIntLit(num) => Ok((self.integer, num as i64)),
            p::SubrangeBoundLiteral::Ident(name) => {
                let def = self.lookup(name, bound.span)?;

                match &self.defs[def.0] {
                    DefPoint::BuiltinConst(BuiltinConst::True) => Ok((self.boolean, 1)),
                    DefPoint::BuiltinConst(BuiltinConst::False) => Ok((self.boolean, 0)),
                    DefPoint::BuiltinConst(BuiltinConst::Maxint) => Ok((self.integer, i64::MAX)),
                    DefPoint::UserDef { kind: DefKind::Const { r#type, value }, span, .. } => {
                        let ty = self.check_bound_ordinality(*r#type, *span, subr_span)?;

                        let ord = match value {
                            Constant::Int(num) => *num,
                            Constant::Bool(bln) => {
                                if *bln {
                                    1
                                } else {
                                    0
                                }
                            }
                            Constant::Char(chr) => *chr as i64,
                            Constant::Enum { r#enum, member_idx } => *member_idx as i64,
                            // Assumption: Constant defs are resolved fully at definition time meaning there are no
                            // more references to other constant-identifiers left.
                            _ => {
                                return Err(AnalysisError::MismatchDef {
                                    got: def,
                                    // at: bound.span,
                                    expected: Self::ORD_MSG,
                                    origin: subr_span,
                                });
                            }
                        };

                        Ok((ty, ord))
                    }
                    DefPoint::UserDef { kind: DefKind::EnumMember { r#type, ordinal }, .. } => {
                        Ok((OrdinalTypeId(r#type.0), *ordinal))
                    }
                    _ => {
                        Err(AnalysisError::MismatchDef {
                            got: def,
                            // at: bound.span,
                            expected: Self::ORD_MSG,
                            origin: subr_span,
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
            TypeKind::Enumerated { .. }
            | TypeKind::Integer
            | TypeKind::Boolean
            | TypeKind::Char => Ok(OrdinalTypeId(id)),
            _ => Err(AnalysisError::MismatchType {
                got: r#type,
                at: bound_span,
                expected: Self::ORD_MSG,
                reason: subr_span,
            }),
        }
    }

    fn convert_ordinal_type_ident(
        &mut self,
        name: UnspanIdent,
        origin: Span,
    ) -> AnalysisResult<OrdinalTypeId> {
        let def = self.lookup(name, origin)?;
        match self.defs[def.0] {
            DefPoint::BuiltinType(BuiltinType::Integer) => Ok(self.integer),
            DefPoint::BuiltinType(BuiltinType::Boolean) => Ok(self.boolean),
            DefPoint::BuiltinType(BuiltinType::Char) => Ok(self.char),
            DefPoint::UserDef { kind: DefKind::Type(r#type), .. } => match self.types[r#type.0] {
                TypeKind::Enumerated { .. }
                | TypeKind::Subrange { .. }
                | TypeKind::Integer
                | TypeKind::Boolean
                | TypeKind::Char => Ok(OrdinalTypeId(r#type.0)),
                _ => Err(AnalysisError::MismatchDef { got: def, expected: "ordinal type", origin }),
            },
            _ => Err(AnalysisError::MismatchDef { got: def, expected: "ordinal type", origin }),
        }
    }

    fn convert_array_type(
        &mut self,
        packed: bool,
        indices: Spanned<&[Spanned<AOrdTy>]>,
        elem: Spanned<&ATy>,
        ty_span: Span,
    ) -> AnalysisResult<TypeId> {
        let indices = indices
            .node
            .iter()
            .map(|index| self.convert_ordinal_type(&index.node, ty_span))
            .collect::<AnalysisResult<_>>()?;
        let elem = self.convert_type(elem)?;
        let ty = TypeKind::Array { packed, indices, elem };

        Ok(self.fresh(ty))
    }

    fn convert_record_type(
        &mut self,
        packed: bool,
        fields: &p::FieldList,
    ) -> AnalysisResult<TypeId> {
        let FieldList { fixed, variant } = self.convert_field_list(fields)?;
        let ty = TypeKind::Record { packed, fixed, variant };

        Ok(self.fresh(ty))
    }

    fn convert_field_list(&mut self, fields: &p::FieldList) -> AnalysisResult<FieldList> {
        let (fixed_part, variant_part): (&[Spanned<p::FixedFields>], Option<&p::VariantField>) =
            match fields {
                p::FieldList::FixedOnly(fixed) => (fixed, None),
                p::FieldList::Both(fixed, variant) => (&fixed.node, Some(&variant.node)),
                p::FieldList::VariantOnly(variant_field) => (&[], Some(variant_field)),
                p::FieldList::Empty => (&[], None),
            };

        let field_type = TypeId(self.types.len());

        let mut fixed = Vec::with_capacity(fixed_part.len());
        for fields in fixed_part.iter() {
            let ty = self.convert_type(fields.node.r#type.as_ref())?;
            fixed.extend(
                fields
                    .node
                    .names
                    .node
                    .iter()
                    .map(|name| self.create_field(name.node, name.span, field_type, ty)),
            );
        }

        let variant = variant_part
            .map(|p::VariantField { tag_field, tag_type, variants }| {
                let tag_type = self.lookup_type(tag_type.node, tag_type.span)?;
                let variants = variants
                    .node
                    .iter()
                    .map(|variant| self.convert_variant(tag_type, variant.as_ref()))
                    .collect::<AnalysisResult<Vec<_>>>()?;

                Ok(VariantPart { tag_field: tag_field.map(|t| t.node), tag_type, variants })
            })
            .transpose()?;

        Ok(FieldList { fixed, variant })
    }

    fn convert_variant(
        &mut self,
        tag_type: TypeId,
        variant: Spanned<&p::Variant>,
    ) -> AnalysisResult<Variant> {
        let case_labels = variant
            .node
            .case_labels
            .node
            .iter()
            .map(|label| {
                let (r#type, r#const) = self.convert_constexpr(label.as_ref())?;
                // We can't just compare the type IDs because we need to check compatibility with string constants (i.e. that their indices are identical), whose types are not de-duplicated
                self.check_assign_compat(tag_type, r#type, label.span)?;
                Ok(r#const)
            })
            .collect::<AnalysisResult<_>>()?;

        let fields = self.convert_field_list(&variant.node.fields.node)?;

        Ok(Variant { case_labels, fields })
    }

    pub fn convert_constexpr(
        &mut self,
        constexpr: Spanned<&p::ConstExpr>,
    ) -> AnalysisResult<(TypeId, Constant)> {
        match constexpr.node {
            ConstExpr::NumLitOrIdent { is_pos, lit } => {
                let sign = if is_pos.unwrap_or(true) { 1i64 } else { -1 };
                match lit {
                    p::ConstExprLit::UIntLit(n) => {
                        Ok((self.integer.into(), Constant::Int(sign * (*n as i64))))
                    }
                    p::ConstExprLit::URealLit(n) => {
                        Ok((self.real, Constant::Real(sign as f64 * *n)))
                    }
                    p::ConstExprLit::Ident(name) => self.lookup_const(*name, constexpr.span),
                }
            }
            ConstExpr::StrLit(str) => {
                let str_type = self.infer_string(str);
                Ok((str_type, Constant::Str(self.get_or_intern_string(str))))
            }
        }
    }

    pub fn infer_string(&mut self, str: &str) -> TypeId {
        // TODO: Make `OrdinalTypeId` correct by construction with a `fresh_ordinal_ty` method or similar
        // which asserts that `kind` is actually ordinal with `unreachable!()`.
        let subrange = OrdinalTypeId(
            self.fresh(TypeKind::Subrange {
                host_type: self.integer,
                lower: 1,
                upper: str.len() as i64 - 1,
            })
            .0,
        );
        let ty = TypeKind::Array { packed: true, indices: vec![subrange], elem: self.char.into() };
        self.fresh(ty)
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

    pub fn to_ordinal(
        &self,
        r#type: TypeId,
        at: Span,
        reason: Span,
    ) -> AnalysisResult<OrdinalTypeId> {
        match &self.types[r#type.0] {
            TypeKind::Enumerated { .. }
            | TypeKind::Subrange { .. }
            | TypeKind::Integer
            | TypeKind::Boolean
            | TypeKind::Char => Ok(OrdinalTypeId(r#type.0)),
            _ => Err(AnalysisError::MismatchType {
                got: r#type,
                at,
                expected: "ordinal type",
                reason,
            }),
        }
    }

    pub fn check_compat(&self, t1: TypeId, t2: TypeId, origin: Span) -> AnalysisResult<()> {
        if t1 == t2 {
            return Ok(());
        }
        let char_ty: TypeId = self.char.into();
        match (&self.types[t1.0], &self.types[t2.0]) {
            // Subrange host types are always fully resolved so a simple eq-check is sufficient
            (
                TypeKind::Subrange { host_type: ht1, .. },
                TypeKind::Subrange { host_type: ht2, .. },
            ) if ht1 == ht2 => Ok(()),
            (
                TypeKind::Set { packed: p1, elem: elem1 },
                TypeKind::Set { packed: p2, elem: elem2 },
            ) if p1 == p2 => self.check_compat((*elem1).into(), (*elem2).into(), origin),
            (
                TypeKind::Array { packed: p1, indices: indices1, elem: elem1 },
                TypeKind::Array { packed: p2, indices: indices2, elem: elem2 },
            ) if p1 == p2
                && char_ty == *elem1
                && char_ty == *elem2
                && indices1.len() == 1
                && indices2.len() == 1 =>
            {
                // A little wasteful
                self.check_assign_compat(indices1[0].into(), indices2[0].into(), origin)
            }
            _ => Err(AnalysisError::IncompatibleTypes { got: t2, expected: t1, origin }),
        }
    }

    /// Check whether the types `t1` and `t2` are 'assignment-compatible', with the check originating from source
    /// location `origin` (for error-reporting reasons).
    pub fn check_assign_compat(&self, t1: TypeId, t2: TypeId, origin: Span) -> AnalysisResult<()> {
        if t1 == t2 && !self.contains_file_type(t1) && !self.contains_file_type(t2) {
            return Ok(());
        }

        let char_ty: TypeId = self.char.into();
        match (&self.types[t1.0], &self.types[t2.0]) {
            // Implicit conversion 🤢
            (TypeKind::Real, TypeKind::Integer) => Ok(()),
            (
                TypeKind::Subrange { host_type, lower, upper },
                TypeKind::Subrange { host_type: host_type2, lower: lower2, upper: upper2 },
            ) => {
                if host_type != host_type2 {
                    Err(AnalysisError::SubrangeHostTypeMismatch { t1, t2, origin })
                } else if lower2 < lower || upper2 > upper {
                    Err(AnalysisError::SubrangeIntervalsMismatch { t1, t2, origin })
                } else {
                    Ok(())
                }
            }
            (
                TypeKind::Set { packed: p1, elem: elem1 },
                TypeKind::Set { packed: p2, elem: elem2 },
            ) if p1 == p2 => {
                let range1 = self.get_interval(*elem1);
                let range2 = self.get_interval(*elem2);
                if range1.start() <= range2.start() && range1.last() >= range2.last() {
                    Ok(())
                } else {
                    Err(AnalysisError::IncompatibleTypes { got: t2, expected: t1, origin })
                }
            }
            // DRY: Do Repeat Yourself
            (
                TypeKind::Array { packed: p1, indices: indices1, elem: elem1 },
                TypeKind::Array { packed: p2, indices: indices2, elem: elem2 },
            ) if p1 == p2
                && char_ty == *elem1
                && char_ty == *elem2
                && indices1.len() == 1
                && indices2.len() == 1 =>
            {
                // A little wasteful
                self.check_assign_compat(indices1[0].into(), indices2[0].into(), origin)
            }
            _ => Err(AnalysisError::IncompatibleTypes { got: t2, expected: t1, origin }),
        }
    }

    fn get_interval(&self, ord_ty: OrdinalTypeId) -> RangeInclusive<i64> {
        match &self.types[ord_ty.0] {
            TypeKind::Enumerated { members } => 0..=(members.len() as i64 - 1),
            TypeKind::Subrange { lower, upper, .. } => (*lower)..=(*upper),
            TypeKind::Integer => i64::MIN..=i64::MAX,
            TypeKind::Boolean => 0..=1,
            TypeKind::Char => 0..=(u8::MAX as i64),
            _ => unreachable!("expected ordinal type"),
        }
    }

    // TODO: Memoise this function with an `TypingContext::imperm_file_component: HashMap<TypeId, bool>`
    fn contains_file_type(&self, r#type: TypeId) -> bool {
        match &self.types[r#type.0] {
            TypeKind::Integer
            | TypeKind::Real
            | TypeKind::Boolean
            | TypeKind::Char
            | TypeKind::Text
            | TypeKind::Enumerated { .. }
            | TypeKind::Subrange { .. }
            | TypeKind::Set { .. } => false,
            TypeKind::Array { elem, .. } => self.contains_file_type(*elem),
            TypeKind::Record { fixed, variant, .. } => {
                self.field_list_contains_file_type(fixed, variant.as_ref())
            }
            TypeKind::Pointer(r#type) => self.contains_file_type(*r#type),
            TypeKind::File(_) => true,
            TypeKind::PointerOfT | TypeKind::SetOfT => unreachable!(),
        }
    }

    fn field_list_contains_file_type(
        &self,
        fixed: &[FieldId],
        variant: Option<&VariantPart>,
    ) -> bool {
        fixed.iter().any(|field| {
            let DefPoint::UserDef { kind: DefKind::Field { r#type, .. }, .. } = self.defs[field.0]
            else {
                unreachable!()
            };
            self.contains_file_type(r#type)
        }) || variant.is_some_and(|VariantPart { tag_type, variants, .. }| {
            self.contains_file_type(*tag_type)
                || variants.iter().any(
                    |Variant { fields: FieldList { fixed, variant }, .. }| {
                        self.field_list_contains_file_type(fixed, variant.as_ref())
                    },
                )
        })
    }

    /// # Panics
    ///
    /// When passed a field, enum member, param, or conformant array bound it will panic as there are specialised
    /// methods for those that return a specialisation of [`DefId`].
    pub fn create_def(&mut self, name: UnspanIdent, kind: DefKind, span: Span) -> DefId {
        match kind {
            DefKind::ProgramParam
            | DefKind::Const { .. }
            | DefKind::Type(_)
            | DefKind::Var(_)
            | DefKind::Label(_)
            | DefKind::Proc(_)
            | DefKind::Func(_) => DefId(self.create(name, kind, span)),
            _ => unreachable!(),
        }
    }

    pub fn insert_def(&mut self, name: UnspanIdent, kind: DefKind, span: Span) -> DefId {
        let def = self.create_def(name, kind, span);
        self.curr_scope_mut().insert(name, def);
        def
    }

    fn create_field(
        &mut self,
        name: UnspanIdent,
        span: Span,
        record: TypeId,
        r#type: TypeId,
    ) -> FieldId {
        let id = self.create(name, DefKind::Field { record, r#type }, span);
        FieldId(id)
    }

    fn insert_enum_member(
        &mut self,
        name: UnspanIdent,
        span: Span,
        r#type: TypeId,
        ordinal: i64,
    ) -> EnumMemberId {
        let id = self.insert_def(name, DefKind::EnumMember { r#type, ordinal }, span);
        EnumMemberId(id.0)
    }

    pub fn create_param(&mut self, name: UnspanIdent, span: Span, kind: ParamKind) -> ParamId {
        let id = self.create(name, DefKind::Param(kind), span);
        ParamId(id)
    }

    // pub fn get_param(&self, param: ParamId) -> Spanned<(UnspanIdent, &ParamKind)> {
    //     match &self.defs[param.0] {
    //         DefPoint::UserDef {
    //             name,
    //             kind: DefKind::Param(kind),
    //             span,
    //         } => Spanned {
    //             span: *span,
    //             node: (*name, kind),
    //         },
    //         _ => unreachable!(),
    //     }
    // }

    pub fn create_conform_array_bound(
        &mut self,
        name: UnspanIdent,
        span: Span,
        r#type: OrdinalTypeId,
    ) -> ConformArrayBoundId {
        let id = self.create(name, DefKind::ConformArrayBound(r#type), span);
        ConformArrayBoundId(id)
    }

    fn create(&mut self, name: UnspanIdent, kind: DefKind, span: Span) -> usize {
        self.defs.push(DefPoint::UserDef { name, kind, span });
        self.defs.len() - 1
    }

    pub fn insert_label(&mut self, label: Spanned<u16>) {
        self.curr_scope_mut().labels.insert(label.node, label.span);
    }

    pub fn lookup(&self, name: UnspanIdent, at: Span) -> AnalysisResult<DefId> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.idents.get(&name))
            .ok_or(AnalysisError::Unbound { name, at })
            .cloned()
    }

    pub fn lookup_type(&self, name: UnspanIdent, span: Span) -> AnalysisResult<TypeId> {
        self.lookup(name, span).and_then(|def_id| match &self.defs[def_id.0] {
            DefPoint::BuiltinType(bt) => Ok(self.builtin_type(*bt)),
            DefPoint::UserDef { kind: DefKind::Type(r#type), .. } => Ok(*r#type),
            _ => Err(AnalysisError::MismatchDef { got: def_id, expected: "type", origin: span }),
        })
    }

    pub fn lookup_const(
        &self,
        name: UnspanIdent,
        span: Span,
    ) -> AnalysisResult<(TypeId, Constant)> {
        self.lookup(name, span).and_then(|def_id| match &self.defs[def_id.0] {
            DefPoint::BuiltinConst(bc) => Ok(self.builtin_const(*bc)),
            DefPoint::UserDef { kind: DefKind::Const { r#type, value }, .. } => {
                Ok((*r#type, *value))
            }
            _ => Err(AnalysisError::MismatchDef { got: def_id, expected: "type", origin: span }),
        })
    }

    pub fn lookup_label(&self, label: u16, at: Span) -> AnalysisResult<Span> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.labels.get(&label).cloned())
            .ok_or(AnalysisError::MissingLabel { label, at })
    }

    pub fn builtin_var(&self, bv: BuiltinVar) -> TypeId {
        match bv {
            BuiltinVar::Input => self.text,
            BuiltinVar::Output => self.text,
        }
    }

    pub fn builtin_const(&self, bc: BuiltinConst) -> (TypeId, Constant) {
        let ty = self.builtin_type(bc.get_type());
        let cnst = match bc {
            BuiltinConst::True => Constant::Bool(true),
            BuiltinConst::False => Constant::Bool(false),
            BuiltinConst::Maxint => Constant::Int(i64::MAX),
        };

        (ty, cnst)
    }

    pub fn builtin_type(&self, bt: BuiltinType) -> TypeId {
        match bt {
            BuiltinType::Integer => self.integer.into(),
            BuiltinType::Real => self.real,
            BuiltinType::Boolean => self.boolean.into(),
            BuiltinType::Char => self.char.into(),
            BuiltinType::Text => self.text,
        }
    }

    pub fn enter_scope(&mut self) {
        self.scopes.push(Scope::default())
    }

    pub fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn curr_scope(&self) -> &Scope {
        self.scopes.last().expect("Expected at least one scope")
    }

    pub fn curr_scope_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().expect("Expected at least one scope")
    }

    pub fn get_def(&self, def: DefId) -> &DefPoint {
        &self.defs[def.0]
    }

    /// Get the name of a definition.
    ///
    /// # Panics
    ///
    /// * Will panic if given a `def` which is not `UserDef`
    pub fn get_def_name(&self, def: DefId) -> UnspanIdent {
        match self.defs[def.0] {
            DefPoint::UserDef { name, .. } => name,
            _ => unreachable!(),
        }
    }

    pub fn get_def_mut(&mut self, def: DefId) -> &mut DefPoint {
        &mut self.defs[def.0]
    }

    pub fn get_type(&self, r#type: TypeId) -> &TypeKind {
        &self.types[r#type.0]
    }

    fn get_or_intern_string(&mut self, s: &str) -> StringId {
        StringId(self.strings.get_or_intern(s))
    }

    pub fn dump(&self, rodeo: &Rodeo) {
        for (depth, scope) in self.scopes.iter().enumerate().skip(1) {
            println!("\nScope #{depth}:");
            for (name, def) in &scope.idents {
                println!("    {:?}: {:?}", rodeo.resolve(name), self.defs[def.0]);
            }
        }
    }
}
