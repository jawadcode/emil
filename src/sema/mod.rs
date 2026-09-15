use std::{cell::RefCell, fmt::Debug};

use builtins::{Builtin, BuiltinConst, BuiltinFunc, BuiltinProc, BuiltinType, BuiltinVar};
use context::{DefId, OrdinalTypeId, Scope, TypeId, TypingContext};
use lasso::Rodeo;
use strum::IntoEnumIterator;

mod builtins;
mod context;

use crate::{
    ast::{program::Program, UnspanIdent},
    utils::{Span, Spanned},
};

pub struct Analyser {
    context: TypingContext,
    // TODO: Switch from using top-level `AnalysisResult<T>` to this
    errors: Vec<AnalysisError>,
    /// Must come from [`ParserState::yeehaw`]
    ///
    /// [`ParserState::yeehaw`]: ../../emil/parser/struct.ParserState.html "yeehaw"
    ///
    rodeo: Rodeo,
}

#[derive(Debug)]
pub enum AnalysisError {
    Unbound {
        name: UnspanIdent,
        at: Span,
    },
    // The only case of an invalid ordinal-type that the parser doesn't prevent
    NotOrdinal {
        name: UnspanIdent,
        at: Span,
    },
    SubrangeBoundsTypeMismatch {
        lower: TypeId,
        upper: TypeId,
    },
    SubrangeBoundsBackwards {
        span: Span,
        start_ord: i64,
        end_ord: i64,
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
    MismatchType {
        got: TypeId,
        at: Span,
        expected: &'static str,
        origin: Span,
    },
    TypeMismatch {
        got: TypeId,
        at: Span,
        expected: TypeId,
        origin: Span,
    },
    IncompatibleTypes {
        got: TypeId,
        expected: TypeId,
        origin: Span,
    },
    DuplicateDecl {
        name: UnspanIdent,
        existing: Span,
        duplicate: Span,
    },
    MissingForwardDecl {
        name: UnspanIdent,
        at: Span,
    },
    UnknownDirective(Span),
}

pub type AnalysisResult<T> = Result<T, AnalysisError>;

impl<'ast> Analyser {
    pub fn new(rodeo: Rodeo) -> Self {
        let rodeo = RefCell::new(rodeo);

        let idents = BuiltinType::iter()
            .map(|b| b.entry(&mut rodeo.borrow_mut()))
            .chain(BuiltinConst::iter().map(|b| b.entry(&mut rodeo.borrow_mut())))
            .chain(BuiltinVar::iter().map(|b| b.entry(&mut rodeo.borrow_mut())))
            .chain(BuiltinProc::iter().map(|b| b.entry(&mut rodeo.borrow_mut())))
            .chain(BuiltinFunc::iter().map(|b| b.entry(&mut rodeo.borrow_mut())));
        let context = TypingContext::new(idents);

        let rodeo = rodeo.into_inner(); // RefCell goes poof

        Self {
            context,
            errors: Vec::new(),
            rodeo,
        }
    }

    pub fn check_program(&mut self, program: &'ast Program) -> AnalysisResult<()> {
        self.context.scopes.pop(); // Program scope
        self.context.scopes.pop(); // Builtins scope
        Ok(())
    }

    fn curr_scope(&self) -> &Scope {
        self.context
            .scopes
            .last()
            .expect("Expected at least one scope")
    }

    fn curr_scope_mut(&mut self) -> &mut Scope {
        self.context
            .scopes
            .last_mut()
            .expect("Expected at least one scope")
    }
}

/// Dummy span for builtins
fn span_bltn<T: Debug + Clone>(node: T) -> Spanned<T> {
    Spanned {
        span: (0..0).into(),
        node,
    }
}
