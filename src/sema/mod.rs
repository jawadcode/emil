use std::{cell::RefCell, fmt::Debug};

use builtins::{Builtin, BuiltinConst, BuiltinFunc, BuiltinProc, BuiltinType, BuiltinVar};
use context::{
    DefId, DefKind, DefPoint, FuncSig, ParamId, ParamKind, ParamType, ProcSig, TypeId,
    TypingContext,
};
use lasso::Rodeo;
use strum::IntoEnumIterator;

mod builtins;
mod context;

use crate::{
    ast::{
        program::{self as p, Directive},
        Ident, UnspanIdent,
    },
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
        existing: DefId,
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

        Self { context, errors: Vec::new(), rodeo }
    }

    pub fn check_program(&mut self, program: &'ast p::Program) -> AnalysisResult<()> {
        self.context.enter_scope(); // Program scope
        for param in &program.params.node {
            self.context.insert_def(param.node, DefKind::ProgramParam, param.span);
        }

        self.check_block(&program.block.node, None)?;

        // self.dump_context();

        self.context.exit_scope(); // Program scope
        self.context.exit_scope(); // Builtins scope
        Ok(())
    }

    fn check_block(
        &mut self,
        block: &'ast p::Block,
        func: Option<(Ident, TypeId)>,
    ) -> AnalysisResult<()> {
        self.context.enter_scope();

        let p::Block { label_decls, const_defs, type_defs, var_decls, routine_decls, stmts } =
            block;

        for label in &label_decls.node {
            self.context.insert_label(*label);
        }

        for const_def in &const_defs.node {
            let p::ConstDef { name, value } = &const_def.node;
            let (r#type, value) = self.context.convert_constexpr(value.as_ref())?;
            let const_def = self.context.insert_def(
                name.node,
                DefKind::Const { r#type, value },
                const_def.span,
            );
            self.context.curr_scope_mut().insert(name.node, const_def);
        }

        for type_def in &type_defs.node {
            let p::TypeDef { name, def } = &type_def.node;
            let r#type = self.context.convert_type(def.as_ref())?;
            self.context.insert_def(name.node, DefKind::Type(r#type), type_def.span);
        }

        for var_decl in &var_decls.node {
            let p::VarDecl { names, r#type } = &var_decl.node;
            let r#type = self.context.convert_type(r#type.as_ref())?;
            for name in &names.node {
                self.context.insert_def(name.node, DefKind::Var(r#type), name.span);
            }
        }

        for routine_decl in &routine_decls.node {
            match &routine_decl.node {
                p::RoutineDecl::Proc(proc_decl) => {
                    self.convert_proc(proc_decl, routine_decl.span)?;
                }
                p::RoutineDecl::Func(func_decl) => {
                    self.convert_func(func_decl, routine_decl.span)?;
                }
            }
        }

        self.dump_context();
        self.context.exit_scope();

        Ok(())
    }

    fn convert_proc(&mut self, proc_decl: &p::ProcDecl, span: Span) -> AnalysisResult<()> {
        let p::ProcDecl { sig, post } = proc_decl;
        let p::ProcSig { name, params } = &sig.node;

        println!("\nProcedure: {:?}", name.map(|n| self.rodeo.resolve(&n)));

        let params = self.convert_params(params)?;
        let has_body = match &post.node {
            p::PostSig::Block(block) => {
                self.context.enter_scope();
                self.load_params(&params);
                self.check_block(block, None)?;
                self.context.exit_scope();
                true
            }
            p::PostSig::Directive(Directive::Forward) => false,
            _ => unimplemented!("Directives other than `forward` are currently unsupported"),
        };
        let proc = ProcSig { params, has_body };
        self.context.insert_def(name.node, DefKind::Proc(proc), span);

        Ok(())
    }

    fn convert_func(&mut self, func_decl: &p::FuncDecl, span: Span) -> AnalysisResult<()> {
        match func_decl {
            p::FuncDecl::Ident(name, block) => {
                println!("\nFunction: {:?}", name.map(|n| self.rodeo.resolve(&n)));
                let def = self.context.lookup(name.node, name.span)?;
                let has_body = {
                    match self.context.get_def(def) {
                        DefPoint::UserDef { kind: DefKind::Func(sig), .. } => sig.has_body,
                        _ => {
                            return Err(AnalysisError::MismatchDef {
                                got: def,
                                expected: "forwarded function",
                                origin: name.span,
                            })
                        }
                    }
                };

                if has_body {
                    return Err(AnalysisError::DuplicateDecl {
                        name: name.node,
                        existing: def,
                        duplicate: span,
                    });
                } else {
                    let DefPoint::UserDef { kind: DefKind::Func(sig), .. } =
                        self.context.get_def_mut(def)
                    else {
                        unreachable!()
                    };
                    sig.has_body = true;
                }

                self.context.enter_scope(); // Function scope
                let DefPoint::UserDef { kind: DefKind::Func(sig), .. } = self.context.get_def(def)
                else {
                    unreachable!()
                };
                let result_type = sig.result;

                let params = sig.params.clone();
                self.load_params(&params);

                self.check_block(&block.node, Some((*name, result_type)))?;
                self.context.exit_scope(); // Function scope

                Ok(())
            }
            p::FuncDecl::Heading(sig, post_sig) => {
                println!("\nFunction: {:?}", sig.node.name.map(|n| self.rodeo.resolve(&n)));
                let p::FuncSig { name, params, result } = &sig.node;

                let params = self.convert_params(&params.node)?;
                let result = self.context.lookup_type(result.node, result.span)?;

                let has_body = match &post_sig.node {
                    p::PostSig::Block(block) => {
                        self.context.enter_scope();
                        self.load_params(&params);
                        self.check_block(block, Some((*name, result)))?;
                        self.context.exit_scope();
                        true
                    }
                    p::PostSig::Directive(Directive::Forward) => false,
                    _ => unimplemented!(),
                };

                self.context.insert_def(
                    name.node,
                    DefKind::Func(FuncSig { params, result, has_body }),
                    span,
                );
                Ok(())
            }
        }
    }

    fn load_params(&mut self, params: &[ParamId]) {
        for param in params {
            let param_def = (*param).into();
            let param_name = self.context.get_def_name(param_def);
            self.context.curr_scope_mut().insert(param_name, param_def);
        }
    }

    fn convert_params(&mut self, params: &[Spanned<p::Param>]) -> AnalysisResult<Vec<ParamId>> {
        let mut new_params = Vec::with_capacity(params.len()); // underestimate
        for param in params {
            match &param.node {
                p::Param::Value(names, r#type) => {
                    let r#type = self.convert_param_type(r#type.as_ref())?;
                    for name in &names.node {
                        new_params.push(self.context.create_param(
                            name.node,
                            param.span,
                            ParamKind::Value(r#type.clone()),
                        ));
                    }
                }
                p::Param::Var(names, r#type) => {
                    let r#type = self.convert_param_type(r#type.as_ref())?;
                    for name in &names.node {
                        new_params.push(self.context.create_param(
                            name.node,
                            param.span,
                            ParamKind::Var(r#type.clone()),
                        ));
                    }
                }
                p::Param::Proc(proc_sig) => {
                    let params = self.convert_params(&proc_sig.params)?;
                    new_params.push(self.context.create_param(
                        proc_sig.name.node,
                        param.span,
                        ParamKind::Proc(params),
                    ));
                }
                p::Param::Func(func_sig) => {
                    let params = self.convert_params(&func_sig.params.node)?;
                    let result =
                        self.context.lookup_type(func_sig.result.node, func_sig.result.span)?;

                    new_params.push(self.context.create_param(
                        func_sig.name.node,
                        param.span,
                        ParamKind::Func { params, result },
                    ));
                }
            }
        }

        Ok(new_params)
    }

    fn convert_param_type(&mut self, r#type: Spanned<&p::ParamType>) -> AnalysisResult<ParamType> {
        match r#type.node {
            p::ParamType::TypeIdent(spur) => {
                self.context.lookup_type(*spur, r#type.span).map(ParamType::TypeIdent)
            }
            p::ParamType::ArraySchema(array_schema) => {
                unimplemented!("array schemata are unsupported at the moment")
            }
        }
    }

    pub fn dump_context(&self) {
        self.context.dump(&self.rodeo);
    }
}

/// Dummy span for builtins
fn span_bltn<T: Debug + Clone>(node: T) -> Spanned<T> {
    Spanned { span: (0..0).into(), node }
}
