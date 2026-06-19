use std::fmt::{Debug, Display, Formatter, Result, Write};

use lasso::Rodeo;

use crate::{
    ast::program::{
        ConstExpr, ConstExprLit, FieldList, FixedFields, OrdinalType, Type, UnpackedStructuredType,
        VariantField,
    },
    utils::Spanned,
};

use super::{Analyser, Scope};

impl Display for Analyser {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        if let Some((head, tail)) = self.scopes.split_first() {
            f.write_str("Program Scope (#0):\n")?;
            fmt_scope(head, f, &self.rodeo)?;
            for (i, scope) in tail.iter().enumerate() {
                f.write_str("Scope #")?;
                Display::fmt(&(i + 1), f)?;
                f.write_str(":\n")?;
                fmt_scope(scope, f, &self.rodeo)?;
            }
        }
        Ok(())
    }
}

fn fmt_scope(scope: &Scope, f: &mut Formatter<'_>, rodeo: &Rodeo) -> Result {
    f.write_str("    Labels:\n")?;
    scope
        .label_env
        .iter()
        .try_for_each(|(label, loc)| -> Result {
            f.write_str("    ")?;
            Display::fmt(label, f)?;
            f.write_str(" @ ")?;
            Display::fmt(loc, f)?;
            f.write_char('\n')
        })?;

    f.write_str("\n    Constants:\n")?;
    scope
        .const_env
        .iter()
        .try_for_each(|(r#const, r#type)| -> Result {
            f.write_str("    ")?;
            Display::fmt(rodeo.resolve(r#const), f)?;
            f.write_str(" : ")?;
            fmt_type(&r#type.node, f, rodeo)?;
            f.write_str(" @ ")?;
            Display::fmt(&r#type.span, f)?;
            f.write_char('\n')
        })?;

    f.write_str("\n    Variables:\n")?;
    scope
        .var_env
        .iter()
        .try_for_each(|(r#const, r#type)| -> Result {
            f.write_str("    ")?;
            Display::fmt(rodeo.resolve(r#const), f)?;
            f.write_str(" : ")?;
            fmt_type(&r#type.node, f, rodeo)?;
            f.write_str(" @ ")?;
            Display::fmt(&r#type.span, f)?;
            f.write_char('\n')
        })
}

fn fmt_type(r#type: &Type, f: &mut Formatter<'_>, rodeo: &Rodeo) -> Result {
    match r#type {
        Type::Ordinal(ordinal_type) => fmt_ordinal_type(ordinal_type, f, rodeo),
        Type::Structured { packed, r#type } => {
            if let Some(_) = packed {
                f.write_str("packed ")?;
            }

            match &r#type.node {
                UnpackedStructuredType::Array { indices, elem } => {
                    f.write_str("array [")?;
                    Spanned::fmt_many(
                        f,
                        &indices.node,
                        |f, ord_ty| fmt_ordinal_type(ord_ty, f, rodeo),
                        ", ",
                    )?;
                    f.write_str("] of (")?;
                    fmt_type(&elem.node, f, rodeo)?;
                    f.write_str(" @ ")?;
                    Display::fmt(&elem.span, f)?;
                    f.write_char(')')
                }
                UnpackedStructuredType::Record(field_list) => {
                    f.write_str("record ")?;
                    fmt_field_list(field_list, f, rodeo)?;
                    f.write_str(" end")
                }
                UnpackedStructuredType::Set(ordinal_type) => {
                    f.write_str("set of ")?;
                    fmt_ordinal_type(ordinal_type, f, rodeo)
                }
                UnpackedStructuredType::File(spanned) => {
                    f.write_str("file of ")?;
                    fmt_type(&spanned.node, f, rodeo)
                }
            }
        }
        Type::Pointer(spur) => {
            f.write_char('^')?;
            Display::fmt(rodeo.resolve(spur), f)
        }
    }
}

fn fmt_field_list(field_list: &FieldList, f: &mut Formatter<'_>, rodeo: &Rodeo) -> Result {
    let fmt_fixed = |f: &mut Formatter<'_>, fields: &FixedFields| {
        Spanned::fmt_many(
            f,
            &fields.names.node,
            |f, spur| f.write_str(rodeo.resolve(spur)),
            ", ",
        )?;
        f.write_str(" : ")?;
        fmt_type(&fields.r#type.node, f, rodeo)
    };

    let fmt_variant_field = |f: &mut Formatter, field: &VariantField| {
        f.write_str("case ")?;
        if let Some(tag_field) = field.tag_field {
            f.write_str(rodeo.resolve(&tag_field.node))?;
            f.write_str(" @ ")?;
            Display::fmt(&tag_field.span, f)?;
            f.write_str(" : ")?;
        }
        f.write_str(rodeo.resolve(&field.tag_type.node))?;
        f.write_str(" of ")?;
        Spanned::fmt_many(
            f,
            &field.variants.node,
            |f, variant| {
                Spanned::fmt_many(
                    f,
                    &variant.case_labels.node,
                    |f, label| fmt_const_expr(label, f, rodeo),
                    ", ",
                )?;
                f.write_str(": (")?;
                fmt_field_list(&variant.fields.node, f, rodeo)?;
                f.write_char(')')
            },
            "; ",
        )?;
        Ok(())
    };

    match field_list {
        FieldList::FixedOnly(fields) => Spanned::fmt_many(f, fields, fmt_fixed, "; "),
        FieldList::Both(fixed_part, variant_part) => {
            Spanned::fmt_many(f, &fixed_part.node, fmt_fixed, "; ")?;
            f.write_str("; ")?;
            fmt_variant_field(f, &variant_part.node)
        }
        FieldList::VariantOnly(variant_field) => fmt_variant_field(f, variant_field),
        FieldList::Empty => Ok(()),
    }
}

fn fmt_ordinal_type(ordinal_type: &OrdinalType, f: &mut Formatter<'_>, rodeo: &Rodeo) -> Result {
    match ordinal_type {
        OrdinalType::Enumerated(spanneds) => {
            f.write_char(')')?;
            Spanned::fmt_many(
                f,
                &spanneds,
                |f, spur| f.write_str(rodeo.resolve(spur)),
                ", ",
            )?;
            f.write_char(')')
        }
        OrdinalType::Subrange { lower, upper } => {
            fmt_const_expr(&lower.node, f, rodeo)?;
            f.write_str("..")?;
            fmt_const_expr(&upper.node, f, rodeo)
        }
        OrdinalType::Ident(spur) => f.write_str(rodeo.resolve(spur)),
    }
}

fn fmt_const_expr(const_expr: &ConstExpr, f: &mut Formatter<'_>, rodeo: &Rodeo) -> Result {
    match const_expr {
        ConstExpr::NumLitOrIdent { is_pos, lit } => {
            if let Some(is_pos) = is_pos {
                f.write_char(if *is_pos { '+' } else { '-' })?;
            }
            match lit {
                ConstExprLit::UIntLit(num) => Display::fmt(num, f),
                ConstExprLit::URealLit(num) => Display::fmt(num, f),
                ConstExprLit::Ident(spur) => f.write_str(rodeo.resolve(spur)),
            }
        }
        ConstExpr::StrLit(string) => Debug::fmt(&string, f),
    }
}
