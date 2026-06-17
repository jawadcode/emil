use std::collections::HashMap;

use lasso::{Rodeo, Spur};

use crate::{
    ast::program::{Block, OrdinalType, Program, Type as ASTType, UnpackedStructuredType},
    utils::{Span, Spanned},
};

pub struct Analyser {
    scopes: Vec<Scope>,
    /// Must come from [`ParserState::yeehaw`]
    ///
    /// [`ParserState::yeehaw`]: ../../emil/parser/struct.ParserState.html "yeehaw"
    ///
    rodeo: Rodeo,
}

#[derive(Default)]
pub struct Scope {
    label_env: HashMap<u64, Span>,
    type_env: HashMap<Spur, Spanned<Type>>,
    var_env: HashMap<Spur, Spanned<Type>>,
}

pub struct AnalysisError {
    got: Type,
    at: Span,
    expected: Type,
    because: Span,
}

pub type AnalysisResult<T> = Result<T, AnalysisError>;

#[derive(Debug, Clone)]
pub enum Type {
    Integer,
    Real,
    Boolean,
    Char,
    Text, // Akin to `file of char`
    Ident(Spur),
    Enum(Vec<Spanned<Spur>>),
    Array {
        index: Box<Spanned<Spur>>,
        elem: Box<Type>,
    },
    Record(HashMap<Spanned<Spur>, Type>),
    Set(Box<Type>),
    File(Box<Type>),
}

impl From<ASTType> for Type {
    fn from(value: ASTType) -> Self {
        match value {
            ASTType::Ordinal(ordinal_type) => match ordinal_type {
                OrdinalType::Enumerated(elements) => Type::Enum(elements),
                OrdinalType::Subrange { lower, upper } => todo!(),
                OrdinalType::Identifier(spur) => todo!(),
            },
            ASTType::Structured { packed, r#type } => match r#type.node {
                UnpackedStructuredType::Array { indices, elem } => todo!(),
                UnpackedStructuredType::Record(field_list) => todo!(),
                UnpackedStructuredType::Set(ordinal_type) => todo!(),
                UnpackedStructuredType::File(spanned) => todo!(),
            },
            ASTType::Pointer(spur) => todo!(),
        }
    }
}

#[derive(Clone, Copy)]
/// Builtin types, referred to as required type identifiers in the literature
enum Required {
    Integer,
    Real,
    Boolean,
    Char,
    Text,
}

impl Analyser {
    pub fn new(rodeo: Rodeo) -> Self {
        Self {
            rodeo,
            scopes: Vec::new(),
        }
    }

    pub fn analyse_program(&mut self, program: &Program) -> AnalysisResult<()> {
        self.scopes.push(Scope {
            var_env: [
                (
                    self.rodeo.get_or_intern("input"),
                    Spanned {
                        span: (0..0).into(),
                        node: Type::Text,
                    },
                ),
                (
                    self.rodeo.get_or_intern("output"),
                    Spanned {
                        span: (0..0).into(),
                        node: Type::Text,
                    },
                ),
            ]
            .into(),
            ..Default::default()
        });
        self.analyse_block(program.block.as_ref())
    }

    fn analyse_block(&mut self, block: Spanned<&Block>) -> AnalysisResult<()> {
        Ok(())
    }

    fn lookup_type(&self, name: Spur) -> Option<Spanned<&Type>> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.type_env.get(&name))
            .map(Spanned::as_ref)
    }

    fn lookup_var(&self, name: Spur) -> Option<Spanned<&Type>> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.type_env.get(&name))
            .map(Spanned::as_ref)
    }

    fn builtin_type(&mut self, name: Required) -> Spur {
        self.rodeo.get_or_intern(match name {
            Required::Integer => "integer",
            Required::Real => "real",
            Required::Boolean => "boolean",
            Required::Char => "char",
            Required::Text => "text",
        })
    }
}
