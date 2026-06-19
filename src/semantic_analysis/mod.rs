use std::{collections::HashMap, fmt::Debug};

use lasso::{Rodeo, Spur};
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

use crate::{
    ast::{
        program::{Block, OrdinalType, Program, Type},
        UnspanIdent,
    },
    utils::{Span, Spanned},
};

mod format;

pub struct Analyser {
    scopes: Vec<Scope>,
    /// Must come from [`ParserState::yeehaw`]
    ///
    /// [`ParserState::yeehaw`]: ../../emil/parser/struct.ParserState.html "yeehaw"
    ///
    rodeo: Rodeo,
}

#[derive(Debug, Default)]
pub struct Scope {
    label_env: HashMap<u64, Span>,
    /// Separate from [`Self::var_env`] because we need to check for illegal assignments (i.e. to constants)
    const_env: HashMap<Spur, Spanned<Type>>,
    type_env: HashMap<Spur, Spanned<Type>>,
    var_env: HashMap<Spur, Spanned<Type>>,
}

#[derive(Debug)]
pub struct AnalysisError {
    got: Type,
    at: Span,
    expected: Type,
    because: Span,
}

pub type AnalysisResult<T> = Result<T, AnalysisError>;

trait Builtin: ToString {}

/// Builtin types, referred to as required type-identifiers
#[derive(Clone, Copy, EnumIter)]
enum BuiltinType {
    Integer,
    Real,
    Boolean,
    Char,
    Text,
}

impl Builtin for BuiltinType {}
impl ToString for BuiltinType {
    fn to_string(&self) -> String {
        match self {
            BuiltinType::Integer => "integer",
            BuiltinType::Real => "real",
            BuiltinType::Boolean => "boolean",
            BuiltinType::Char => "char",
            BuiltinType::Text => "text",
        }
        .to_string()
    }
}

/// Builtin constants, referred to as required constant-identifiers
#[derive(Clone, Copy, EnumIter)]
enum BuiltinConst {
    True,
    False,
    Maxint,
}

impl Builtin for BuiltinConst {}
impl ToString for BuiltinConst {
    fn to_string(&self) -> String {
        match self {
            BuiltinConst::True => "true",
            BuiltinConst::False => "false",
            BuiltinConst::Maxint => "maxint",
        }
        .to_string()
    }
}

/// Builtin variables, referred to as required variable-identifiers
#[derive(Clone, Copy, EnumIter)]
enum BuiltinVar {
    Input,
    Output,
}

impl Builtin for BuiltinVar {}
impl ToString for BuiltinVar {
    fn to_string(&self) -> String {
        match self {
            BuiltinVar::Input => "input",
            BuiltinVar::Output => "output",
        }
        .to_string()
    }
}

const BUILTIN_CONSTS: [(BuiltinConst, BuiltinType); 3] = [
    (BuiltinConst::True, BuiltinType::Boolean),
    (BuiltinConst::False, BuiltinType::Boolean),
    (BuiltinConst::Maxint, BuiltinType::Integer),
];

const BUILTIN_VARS: [(BuiltinVar, BuiltinType); 2] = [
    (BuiltinVar::Input, BuiltinType::Text),
    (BuiltinVar::Output, BuiltinType::Text),
];

impl Analyser {
    pub fn new(rodeo: Rodeo) -> Self {
        Self {
            rodeo,
            scopes: Vec::new(),
        }
    }

    pub fn analyse_program(&mut self, program: &Program) -> AnalysisResult<()> {
        let const_env = BUILTIN_CONSTS
            .map(|(r#const, r#type)| {
                let r#const = self.builtin_ident(r#const);
                let r#type = self.builtin_type(r#type);
                (r#const, r#type)
            })
            .into();
        let type_env = BuiltinType::iter()
            .map(|r#type| {
                let name = self.builtin_ident(r#type);
                let r#type = self.builtin_type(r#type);
                (name, r#type)
            })
            .collect();
        let var_env = BUILTIN_VARS
            .map(|(var, r#type)| {
                let var = self.builtin_ident(var);
                let r#type = self.builtin_type(r#type);
                (var, r#type)
            })
            .into();

        self.scopes.push(Scope {
            const_env,
            type_env,
            var_env,
            ..Default::default()
        });

        self.analyse_block(program.block.as_ref())
    }

    fn analyse_block(&mut self, block: Spanned<&Block>) -> AnalysisResult<()> {
        println!("{self}");
        Ok(())
    }

    fn lookup_type(&self, name: UnspanIdent) -> Option<Spanned<&Type>> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.type_env.get(&name))
            .map(Spanned::as_ref)
    }

    fn lookup_var(&self, name: UnspanIdent) -> Option<Spanned<&Type>> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.type_env.get(&name))
            .map(Spanned::as_ref)
    }

    fn builtin_ident(&mut self, name: impl Builtin) -> UnspanIdent {
        self.rodeo.get_or_intern(name.to_string())
    }

    fn builtin_type(&mut self, name: BuiltinType) -> Spanned<Type> {
        span_bltn(Type::Ordinal(OrdinalType::Ident(self.builtin_ident(name))))
    }
}

/// Dummy span for builtins
fn span_bltn<T: Debug + Clone>(node: T) -> Spanned<T> {
    Spanned {
        span: (0..0).into(),
        node,
    }
}
