use ariadne::{Label, Report, ReportKind, Source};

use crate::{
    ast::UnspanIdent,
    sema::{
        Analyser,
        context::{
            Constant, DefId, DefKind, DefPoint, EnumMemberId, FieldId, OrdinalTypeId, ParamId,
            TypeId, TypeKind, Variant, VariantPart,
        },
    },
    utils::Span,
};

// TODO: Major major cleanup because wtf
#[derive(Debug)]
pub enum AnalysisError {
    Unbound {
        name: UnspanIdent,
        at: Span,
    },
    InvertedSubrangeBounds {
        span: Span,
        start_ord: i64,
        start_span: Span,
        end_ord: i64,
        end_span: Span,
    },
    SubrangeHostTypeMismatch {
        t1: TypeId,
        t2: TypeId,
        // Recover `host_type`s from `TypeId`s
        origin: Span,
    },
    SubrangeIntervalsMismatch {
        t1: TypeId,
        t2: TypeId,
        // Recover `lower`s and `upper`s from `TypeId`s
        origin: Span,
    },
    // These two are generated for built-in lang constructs which have specific sets of rules about accepted types
    MismatchDef {
        got: DefId,
        // Recover location of def (where applicable) from `DefPoint` at `got`
        expected: &'static str,
        origin: Span,
    },
    TypeMismatch {
        got: TypeId,
        at: Span,
        expected: Expected,
        reason: Span,
    },
    IncompatibleTypes {
        got: TypeId,
        expected: TypeId,
        origin: Span,
    },
    AssignIncompatTypes {
        got: TypeId,
        expected: TypeId,
        origin: Span,
    },
    DuplicateDecl {
        name: UnspanIdent,
        existing: DefId,
        duplicate: Span,
    },
    MissingField {
        record: TypeId,
        name: UnspanIdent,
        at: Span,
    },
    ExpectedIdentifier {
        kind: &'static str,
        at: Span,
    },
    // `formal` and `actual` are included because it's hard to trace backwards from their parameters
    Incongruous {
        /// Guaranteed to be `ParamKind::Proc(_)`
        formal: ParamId,
        /// Guaranteed to be `DefPoint::UserDef { kind: DefKind::Proc(_), .. }`
        actual: DefId,
        formal_param: ParamId,
        actual_param: ParamId,
        origin: Span,
    },
    MissingLabel {
        label: u16,
        at: Span,
    },
    RoutineParamsShapeMismatch {
        formal: ParamId,
        /// Guaranteed to be `DefPoint::UserDef { kind: DefKind::Proc(_) | DefKind::Func(_), .. }`
        actual: DefId,
        origin: Span,
    },
    MemberTypeMismatch {
        member_type: OrdinalTypeId,
        at: Span,
        /// Guaranteed to be `TypeKind::Set { .. }`
        set_type: TypeId,
    },
}

#[derive(Debug, Clone)]
pub enum Expected {
    One(TypeId),
    // TODO: Use a `smallvec::SmallVec`
    Many(Vec<TypeId>),
    Abstract(&'static str),
}

impl Analyser {
    pub fn print_error(&self, err: AnalysisError, file_path: &str, file_content: &str) {
        match err {
            AnalysisError::Unbound { name, at } => {
                Report::build(ReportKind::Error, (file_path, at.to_range()))
                    .with_message("Unbound identifier")
                    .with_label(Label::new((file_path, at.to_range())).with_message(format!(
                        "Unbound identifier '{}'",
                        self.rodeo.resolve(&name)
                    )))
            }
            AnalysisError::InvertedSubrangeBounds {
                span,
                start_ord,
                start_span,
                end_ord,
                end_span,
            } => Report::build(ReportKind::Error, (file_path, span.to_range()))
                .with_message("Inverted subrange bounds")
                // .with_label(
                //     Label::new((file_path, span.to_range()))
                //         .with_message("Inverted subrange bounds"),
                // )
                .with_label(
                    Label::new((file_path, start_span.to_range()))
                        .with_message(format!("Has an ordinal value of {}", start_ord)),
                )
                .with_label(
                    Label::new((file_path, end_span.to_range()))
                        .with_message(format!("Has an ordinal value of {}", end_ord)),
                ),
            AnalysisError::SubrangeHostTypeMismatch { t1, t2, origin } => {
                let &TypeKind::Subrange { host_type: h1, .. } = self.context.get_type(t1) else {
                    unreachable!()
                };
                let &TypeKind::Subrange { host_type: h2, .. } = self.context.get_type(t2) else {
                    unreachable!()
                };
                Report::build(ReportKind::Error, (file_path, origin.to_range()))
                    .with_message("Subrange host types do not match")
                    .with_label(Label::new((file_path, origin.to_range())).with_message(format!(
                        "Got host type of {}, expected {}",
                        self.type_to_string(h2.into()),
                        self.type_to_string(h1.into())
                    )))
            }
            AnalysisError::SubrangeIntervalsMismatch { t1, t2, origin } => {
                let TypeKind::Subrange { lower: l1, upper: u1, .. } = self.context.get_type(t1)
                else {
                    unreachable!()
                };
                let TypeKind::Subrange { lower: l2, upper: u2, .. } = self.context.get_type(t2)
                else {
                    unreachable!()
                };
                Report::build(ReportKind::Error, (file_path, origin.to_range()))
                    .with_message("Assignment incompatible: Subrange intervals mismatch")
                    .with_label(
                        Label::new((file_path, origin.to_range()))
                            .with_message(format!("Expected {l1}..{u1}, got {l2}..{u2}")),
                    )
            }
            AnalysisError::MismatchDef { got, expected, origin } => {
                Report::build(ReportKind::Error, (file_path, origin.to_range()))
                    .with_message("Mismatch in definition kinds")
                    .with_label(Label::new((file_path, origin.to_range())).with_message(format!(
                        "Expected {expected}, got {}",
                        self.def_to_string(got)
                    )))
            }
            AnalysisError::TypeMismatch { got, at, expected, reason } => {
                Report::build(ReportKind::Error, (file_path, reason.to_range()))
                    .with_message("Type mismatch")
                    .with_label(Label::new((file_path, at.to_range())).with_message(format!(
                        "Expected {}, got {}",
                        self.expected_to_string(expected),
                        self.type_to_string(got),
                    )))
                    .with_label(
                        Label::new((file_path, reason.to_range())).with_message("Due to this"),
                    )
            }
            AnalysisError::IncompatibleTypes { got, expected, origin } => {
                Report::build(ReportKind::Error, (file_path, origin.to_range()))
                    .with_message("Incompatible types")
                    .with_label(Label::new((file_path, origin.to_range())).with_message(format!(
                        "Expected {}, got {}",
                        self.type_to_string(expected),
                        self.type_to_string(got)
                    )))
            }
            AnalysisError::AssignIncompatTypes { got, expected, origin } => {
                Report::build(ReportKind::Error, (file_path, origin.to_range()))
                    .with_message("Assignment-incompatible types")
                    .with_label(Label::new((file_path, origin.to_range())).with_message(format!(
                        "Expected {}, got {}",
                        self.type_to_string(expected),
                        self.type_to_string(got)
                    )))
            }
            AnalysisError::DuplicateDecl { name, existing, duplicate } => {
                let report = Report::build(ReportKind::Error, (file_path, duplicate.to_range()))
                    .with_message(format!("Duplicate declaration {}", self.rodeo.resolve(&name)))
                    .with_label(
                        Label::new((file_path, duplicate.to_range()))
                            .with_message("Duplicate here"),
                    );
                if let DefPoint::UserDef { kind, span, .. } = self.context.get_def(existing) {
                    report.with_label(
                        Label::new((file_path, span.to_range()))
                            .with_message("Previous declaration here"),
                    )
                } else {
                    report
                }
            }
            AnalysisError::MissingField { record, name, at } => {
                let Some(&DefPoint::UserDef {
                    name: record_type_name, span: record_type_span, ..
                }) = self.context.get_def_from_type(record)
                else {
                    unreachable!()
                };
                Report::build(ReportKind::Error, (file_path, at.to_range()))
                    .with_message(format!(
                        "Field '{}' does not exist in record type '{}'",
                        self.rodeo.resolve(&name),
                        self.rodeo.resolve(&record_type_name)
                    ))
                    .with_label(
                        Label::new((file_path, at.to_range())).with_message("Field does not exist"),
                    )
                    .with_label(
                        Label::new((file_path, record_type_span.to_range()))
                            .with_message("Record type defined here"),
                    )
            }
            AnalysisError::ExpectedIdentifier { kind, at } => todo!(),
            AnalysisError::Incongruous { formal, actual, formal_param, actual_param, origin } => {
                todo!()
            }
            AnalysisError::MissingLabel { label, at } => todo!(),
            AnalysisError::RoutineParamsShapeMismatch { formal, actual, origin } => todo!(),
            AnalysisError::MemberTypeMismatch { member_type, at, set_type } => todo!(),
        }
        .finish()
        .print((file_path, Source::from(file_content)))
        .unwrap()
    }

    fn expected_to_string(&self, expected: Expected) -> String {
        match expected {
            Expected::One(r#type) => self.type_to_string(r#type),
            Expected::Many(types) => types
                .into_iter()
                .map(|r#type| self.type_to_string(r#type))
                .collect::<Vec<_>>()
                .join(", "),
            Expected::Abstract(abs) => abs.to_string(),
        }
    }

    fn def_to_string(&self, def: DefId) -> &'static str {
        match self.context.get_def(def) {
            DefPoint::BuiltinConst(_) => "builtin constant",
            DefPoint::BuiltinType(_) => "builtin type",
            DefPoint::BuiltinVar(_) => "builtin variable",
            DefPoint::BuiltinProc(_) => "builtin procedure",
            DefPoint::BuiltinFunc(_) => "builtin function",
            DefPoint::UserDef { kind, .. } => match kind {
                DefKind::ProgramParam => "program parameter",
                DefKind::Const { .. } => "constant",
                DefKind::Type(_) => "type",
                DefKind::Var(_) => "variable",
                DefKind::Field { .. } => "field",
                DefKind::EnumMember { .. } => "enumerated type member",
                DefKind::Proc(_) => "procedure",
                DefKind::Func(_) => "function",
                DefKind::Param(_) => "parameter",
                DefKind::ConformArrayBound(_) => "conformant array parameter bound",
            },
        }
    }

    fn type_to_string(&self, r#type: TypeId) -> String {
        let mut res = String::new();
        self.type_to_string_inner(r#type, &mut res);
        res
    }

    fn type_to_string_inner(&self, r#type: TypeId, buf: &mut String) {
        match self.context.get_type(r#type) {
            TypeKind::Enumerated { members } => {
                buf.push('(');
                let fmt_member = |mem: EnumMemberId, buf: &mut String| {
                    let DefPoint::UserDef { name, kind: DefKind::EnumMember { .. }, .. } =
                        self.context.get_def(mem.into())
                    else {
                        unreachable!()
                    };
                    buf.push_str(self.rodeo.resolve(name));
                };
                match members.as_slice() {
                    [] => (),
                    &[sole] => fmt_member(sole, buf),
                    [first, rest @ ..] => {
                        fmt_member(*first, buf);
                        for &member in rest {
                            buf.push_str(", ");
                            fmt_member(member, buf)
                        }
                    }
                }
                buf.push(')');
            }
            TypeKind::Subrange { host_type, lower, upper } => {
                if let TypeKind::Enumerated { members } = self.context.get_type((*host_type).into())
                {
                    let (
                        DefPoint::UserDef { name: lower, kind: DefKind::EnumMember { .. }, .. },
                        DefPoint::UserDef { name: upper, kind: DefKind::EnumMember { .. }, .. },
                    ) = (
                        self.context.get_def(members[*lower as usize].into()),
                        self.context.get_def(members[*upper as usize].into()),
                    )
                    else {
                        unreachable!()
                    };

                    buf.push_str(self.rodeo.resolve(lower));
                    buf.push_str("..");
                    buf.push_str(self.rodeo.resolve(upper));
                } else {
                    let lower = lower.to_string();
                    let upper = upper.to_string();
                    buf.push_str(&lower);
                    buf.push_str("..");
                    buf.push_str(&upper);
                }
                buf.push_str(" : ");
                self.type_to_string_inner((*host_type).into(), buf);
            }
            TypeKind::Integer => buf.push_str("Integer"),
            TypeKind::Real => buf.push_str("Real"),
            TypeKind::Boolean => buf.push_str("Boolean"),
            TypeKind::Char => buf.push_str("Character"),
            TypeKind::Text => buf.push_str("Text (File of Char)"),
            TypeKind::Array { packed, indices, elem } => {
                if *packed {
                    buf.push_str("packed ");
                }
                buf.push_str("array [");
                for index in indices {
                    self.type_to_string_inner((*index).into(), buf);
                    buf.push_str(", ");
                }
                buf.push_str("] of ");
                self.type_to_string_inner(*elem, buf);
            }
            TypeKind::Record { packed, fixed, variant } => {
                if *packed {
                    buf.push_str("packed ");
                }
                buf.push_str("record");
                self.field_list_to_string(fixed, variant.as_ref(), buf);
                buf.push_str(" end");
            }
            TypeKind::Set { packed, member_ty } => {
                if *packed {
                    buf.push_str("Packed ");
                }
                buf.push_str("Set of ");
                self.type_to_string_inner((*member_ty).into(), buf);
            }
            TypeKind::File(r#type) => {
                buf.push_str("File of ");
                self.type_to_string_inner(*r#type, buf);
            }
            TypeKind::Pointer(r#type) => {
                buf.push_str("Pointer of ");
                self.type_to_string_inner(*r#type, buf);
            }
            TypeKind::PointerOfT => buf.push_str("Pointer of T"),
            TypeKind::SetOfT => buf.push_str("Set of T"),
        }
    }

    fn field_list_to_string(
        &self,
        fixed: &[FieldId],
        variant: Option<&VariantPart>,
        buf: &mut String,
    ) {
        for field in fixed {
            buf.push(' ');
            let DefPoint::UserDef { name, kind: DefKind::Field { r#type, .. }, .. } =
                self.context.get_def((*field).into())
            else {
                unreachable!()
            };
            buf.push_str(self.rodeo.resolve(name));
            buf.push_str(" : ");
            self.type_to_string_inner(*r#type, buf);
            buf.push(';');
        }
        if let Some(VariantPart { tag_field, tag_type, variants }) = variant {
            buf.push_str(" case ");
            if let Some(tag_field) = tag_field {
                buf.push_str(self.rodeo.resolve(tag_field));
                buf.push_str(" : ");
            }
            self.type_to_string_inner(*tag_type, buf);
            buf.push_str(" of ");
            match &variants[..] {
                [] => (),
                [variant] => self.variant_to_string(variant, buf),
                [first, rest @ ..] => {
                    self.variant_to_string(first, buf);
                    for variant in rest {
                        buf.push_str("; ");
                        self.variant_to_string(variant, buf);
                    }
                }
            }
        }
    }

    fn variant_to_string(&self, Variant { case_labels, fields }: &Variant, buf: &mut String) {
        for case_label in case_labels {
            match case_label {
                Constant::Int(n) => {
                    let res = n.to_string();
                    buf.push_str(&res);
                }
                Constant::Real(n) => {
                    let res = n.to_string();
                    buf.push_str(&res);
                }
                Constant::Bool(b) => {
                    let res = b.to_string();
                    buf.push_str(&res);
                }
                Constant::Char(c) => {
                    let s = [*c];
                    let res = String::from_utf8_lossy(&s);
                    buf.push_str(res.as_ref());
                }
                Constant::Str(s) => buf.push_str(self.context.get_string(*s)),
                Constant::Enum { r#enum, member_idx } => {
                    let TypeKind::Enumerated { members } = self.context.get_type(*r#enum) else {
                        unreachable!()
                    };
                    let DefPoint::UserDef { name, kind: DefKind::EnumMember { .. }, .. } =
                        self.context.get_def(members[*member_idx].into())
                    else {
                        unreachable!()
                    };
                    buf.push_str(self.rodeo.resolve(name));
                }
            }
        }

        buf.push_str(" : ");
        buf.push('(');
        self.field_list_to_string(&fields.fixed, fields.variant.as_ref(), buf);
        buf.push(')');
    }
}
