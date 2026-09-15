// Excuse the extensive use of `..`, I hate the way rustfmt formats >2 field struct patterns

use std::{collections::HashMap, ops::RangeInclusive};

use crate::{
    ast::{
        self,
        program::{ConstExpr, SubrangeBound},
        UnspanIdent,
    },
    utils::{Span, Spanned},
};

use super::{
    builtins::{BuiltinConst, BuiltinFunc, BuiltinProc, BuiltinType, BuiltinVar},
    AnalysisError, AnalysisResult,
};
use ast::program as p;
use ast::program::{OrdinalType as AOrdTy, Type as ATy, UnpackedStructuredType as AUnpackStructTy};
use lasso::{Rodeo, Spur};
use DefPoint::UserDef;

pub struct TypingContext {
    types: Vec<TypeKind>,
    defs: Vec<DefPoint>,
    strings: Rodeo,
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

#[derive(Default)]
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

/// A `defining-point` as per the standard
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
            strings: Rodeo::default(),
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
    fn convert_type(&mut self, ty: Spanned<&ATy>) -> Result<TypeId, AnalysisError> {
        match ty.node {
            ATy::Ordinal(ordinal_type) => self
                .convert_ordinal_type(ordinal_type, ty.span)
                .map(|ord| ord.into()),
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
            ATy::Pointer(pointee) => self
                .lookup_type(*pointee, ty.span)
                .map(|ty| self.fresh(TypeKind::Pointer(ty))),
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
            .into_iter()
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
        let ty = TypeKind::Subrange {
            host_type,
            lower,
            upper,
        };

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
                    DefPoint::UserDef {
                        kind: DefKind::Const { r#type, value },
                        span,
                    } => {
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
                    DefPoint::UserDef {
                        kind: DefKind::EnumMember { r#type, ordinal },
                        ..
                    } => Ok((OrdinalTypeId(r#type.0), *ordinal)),
                    _ => {
                        return Err(AnalysisError::MismatchDef {
                            got: def,
                            // at: bound.span,
                            expected: Self::ORD_MSG,
                            origin: subr_span,
                        });
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
            _ => {
                return Err(AnalysisError::MismatchType {
                    got: r#type,
                    at: bound_span,
                    expected: Self::ORD_MSG,
                    origin: subr_span,
                })
            }
        }
    }

    fn convert_ordinal_type_ident(
        &mut self,
        name: UnspanIdent,
        origin: Span,
    ) -> AnalysisResult<OrdinalTypeId> {
        let def = self.lookup(name, origin)?;
        match self.defs[def.0] {
            DefPoint::BuiltinType(BuiltinType::Integer) => Ok(self.integer.into()),
            DefPoint::BuiltinType(BuiltinType::Boolean) => Ok(self.boolean.into()),
            DefPoint::BuiltinType(BuiltinType::Char) => Ok(self.char.into()),
            UserDef {
                kind: DefKind::Type(r#type),
                ..
            } => match self.types[r#type.0] {
                TypeKind::Enumerated { .. }
                | TypeKind::Subrange { .. }
                | TypeKind::Integer
                | TypeKind::Boolean
                | TypeKind::Char => Ok(OrdinalTypeId(r#type.0)),
                _ => Err(AnalysisError::MismatchDef {
                    got: def,
                    expected: "ordinal type",
                    origin,
                }),
            },
            _ => Err(AnalysisError::MismatchDef {
                got: def,
                expected: "ordinal type",
                origin,
            }),
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
        let ty = TypeKind::Array {
            packed,
            indices,
            elem,
        };

        Ok(self.fresh(ty))
    }

    fn convert_record_type(
        &mut self,
        packed: bool,
        fields: &p::FieldList,
    ) -> AnalysisResult<TypeId> {
        let FieldList { fixed, variant } = self.convert_field_list(fields)?;
        let ty = TypeKind::Record {
            packed,
            fixed,
            variant,
        };

        Ok(self.fresh(ty))
    }

    fn convert_field_list(&mut self, fields: &p::FieldList) -> AnalysisResult<FieldList> {
        let (fixed_part, variant_part): (&[Spanned<p::FixedFields>], Option<&p::VariantField>) =
            match fields {
                p::FieldList::FixedOnly(fixed) => (&fixed, None),
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
                    .map(|name| self.insert_field(name.node, name.span, field_type, ty)),
            );
        }

        let variant = variant_part
            .map(
                |p::VariantField {
                     tag_field,
                     tag_type,
                     variants,
                 }| {
                    let tag_type = self.lookup_type(tag_type.node, tag_type.span)?;
                    let variants = variants
                        .node
                        .iter()
                        .map(|variant| self.convert_variant(tag_type, variant.as_ref()))
                        .collect::<AnalysisResult<Vec<_>>>()?;

                    Ok(VariantPart {
                        tag_field: tag_field.map(|t| t.node),
                        tag_type,
                        variants,
                    })
                },
            )
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

        Ok(Variant {
            case_labels,
            fields,
        })
    }

    fn convert_constexpr(
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
                let ty = TypeKind::Array {
                    packed: true,
                    indices: vec![subrange],
                    elem: self.char.into(),
                };

                Ok((
                    self.fresh(ty),
                    Constant::Str(self.get_or_intern_string(str)),
                ))
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

    /// # Panics
    ///
    /// When passed a field, enum member, param, or conformant array bound it will panic as there are specialised
    /// methods for those that return a specialisation of [`DefId`].
    pub fn insert(&mut self, name: UnspanIdent, kind: DefKind, span: Span) -> DefId {
        match kind {
            DefKind::ProgramParam
            | DefKind::Const { .. }
            | DefKind::Type(_)
            | DefKind::Var(_)
            | DefKind::Label(_)
            | DefKind::EnumMember { .. }
            | DefKind::Proc(_)
            | DefKind::Func(_) => DefId(self.insert_def(name, kind, span)),
            _ => unreachable!(),
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
                TypeKind::Set {
                    packed: p1,
                    elem: elem1,
                },
                TypeKind::Set {
                    packed: p2,
                    elem: elem2,
                },
            ) if p1 == p2 => self.check_compat((*elem1).into(), (*elem2).into(), origin),
            (
                TypeKind::Array {
                    packed: p1,
                    indices: indices1,
                    elem: elem1,
                },
                TypeKind::Array {
                    packed: p2,
                    indices: indices2,
                    elem: elem2,
                },
            ) if p1 == p2
                && char_ty == *elem1
                && char_ty == *elem2
                && indices1.len() == 1
                && indices2.len() == 1 =>
            {
                // A little wasteful
                self.check_assign_compat(indices1[0].into(), indices2[0].into(), origin)
            }
            _ => Err(AnalysisError::IncompatibleTypes {
                got: t2,
                expected: t1,
                origin,
            }),
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
                TypeKind::Subrange {
                    host_type,
                    lower,
                    upper,
                },
                TypeKind::Subrange {
                    host_type: host_type2,
                    lower: lower2,
                    upper: upper2,
                },
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
                TypeKind::Set {
                    packed: p1,
                    elem: elem1,
                },
                TypeKind::Set {
                    packed: p2,
                    elem: elem2,
                },
            ) if p1 == p2 => {
                let range1 = self.get_interval(*elem1);
                let range2 = self.get_interval(*elem2);
                if range1.start() <= range2.start() && range1.last() >= range2.last() {
                    Ok(())
                } else {
                    Err(AnalysisError::IncompatibleTypes {
                        got: t2,
                        expected: t1,
                        origin,
                    })
                }
            }
            // DRY: Do Repeat Yourself
            (
                TypeKind::Array {
                    packed: p1,
                    indices: indices1,
                    elem: elem1,
                },
                TypeKind::Array {
                    packed: p2,
                    indices: indices2,
                    elem: elem2,
                },
            ) if p1 == p2
                && char_ty == *elem1
                && char_ty == *elem2
                && indices1.len() == 1
                && indices2.len() == 1 =>
            {
                // A little wasteful
                self.check_assign_compat(indices1[0].into(), indices2[0].into(), origin)
            }
            _ => Err(AnalysisError::IncompatibleTypes {
                got: t2,
                expected: t1,
                origin,
            }),
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
        }
    }

    fn field_list_contains_file_type(
        &self,
        fixed: &[FieldId],
        variant: Option<&VariantPart>,
    ) -> bool {
        fixed.iter().any(|field| {
            let DefPoint::UserDef {
                kind: DefKind::Field { r#type, .. },
                ..
            } = self.defs[field.0]
            else {
                unreachable!()
            };
            self.contains_file_type(r#type)
        }) || variant.map_or(
            false,
            |VariantPart {
                 tag_type, variants, ..
             }| {
                self.contains_file_type(*tag_type)
                    || variants.iter().any(
                        |Variant {
                             fields: FieldList { fixed, variant },
                             ..
                         }| {
                            self.field_list_contains_file_type(fixed, variant.as_ref())
                        },
                    )
            },
        )
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

    pub fn lookup_type(&self, name: UnspanIdent, span: Span) -> AnalysisResult<TypeId> {
        self.lookup(name, span)
            .and_then(|def_id| match &self.defs[def_id.0] {
                DefPoint::BuiltinType(bt) => Ok(self.builtin_type(*bt)),
                DefPoint::UserDef {
                    kind: DefKind::Type(r#type),
                    ..
                } => Ok(*r#type),
                _ => Err(AnalysisError::MismatchDef {
                    got: def_id,
                    expected: "type",
                    origin: span,
                }),
            })
    }

    pub fn lookup_const(
        &self,
        name: UnspanIdent,
        span: Span,
    ) -> AnalysisResult<(TypeId, Constant)> {
        self.lookup(name, span)
            .and_then(|def_id| match &self.defs[def_id.0] {
                DefPoint::BuiltinConst(bc) => Ok((self.builtin_const_type(*bc), (*bc).into())),
                DefPoint::UserDef {
                    kind: DefKind::Const { r#type, value },
                    ..
                } => Ok((*r#type, *value)),
                _ => Err(AnalysisError::MismatchDef {
                    got: def_id,
                    expected: "type",
                    origin: span,
                }),
            })
    }

    fn builtin_const_type(&self, bc: BuiltinConst) -> TypeId {
        match bc {
            BuiltinConst::True | BuiltinConst::False => self.boolean,
            BuiltinConst::Maxint => self.integer,
        }
        .into()
    }

    fn builtin_type(&self, bt: BuiltinType) -> TypeId {
        match bt {
            BuiltinType::Integer => self.integer.into(),
            BuiltinType::Real => self.real,
            BuiltinType::Boolean => self.boolean.into(),
            BuiltinType::Char => self.char.into(),
            BuiltinType::Text => self.text,
        }
    }

    pub fn curr_scope(&self) -> &Scope {
        self.scopes.last().expect("Expected at least one scope")
    }

    pub fn curr_scope_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().expect("Expected at least one scope")
    }

    fn get_or_intern_string(&mut self, s: &str) -> StringId {
        StringId(self.strings.get_or_intern(s))
    }
}
