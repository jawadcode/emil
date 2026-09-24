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
    ast::{Ident, UnspanIdent, expr as e, program as p, stmt as s},
    sema::context::{FieldId, ParamIter, ParamSection, TypeKind, VariantPart},
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
        reason: Span,
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
    MissingField {
        record: TypeId,
        name: UnspanIdent,
        at: Span,
    },
    UnexpectedNil(Span),
    UnexpectedEmptySet(Span),
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
    UndeclaredLabel(Span),
    RoutineParamsShapeMismatch {
        formal: ParamId,
        actual: DefId,
        origin: Span,
    },
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
        // self.context.enter_scope();

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
                    self.check_proc(proc_decl, routine_decl.span)?;
                }
                p::RoutineDecl::Func(func_decl) => {
                    self.check_func(func_decl, routine_decl.span)?;
                }
            }
        }

        for stmt in &stmts.node {
            self.check_stmt(stmt.as_ref(), func)?;
        }

        self.dump_context();
        // self.context.exit_scope();

        Ok(())
    }

    fn check_proc(&mut self, proc_decl: &p::ProcDecl, span: Span) -> AnalysisResult<()> {
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
            p::PostSig::Directive(p::Directive::Forward) => false,
            _ => unimplemented!("Directives other than `forward` are currently unsupported"),
        };
        let proc = ProcSig { params, has_body };
        self.context.insert_def(name.node, DefKind::Proc(proc), span);

        Ok(())
    }

    fn check_func(&mut self, func_decl: &p::FuncDecl, span: Span) -> AnalysisResult<()> {
        match func_decl {
            p::FuncDecl::Ident(name, block) => {
                println!("\nFunction: {:?}", name.map(|n| self.rodeo.resolve(&n)));
                let def = self.context.lookup(name.node, name.span)?;
                let DefPoint::UserDef { kind: DefKind::Func(sig), .. } =
                    self.context.get_def(def).clone()
                else {
                    return Err(AnalysisError::MismatchDef {
                        got: def,
                        expected: "forwarded function",
                        origin: name.span,
                    });
                };

                if sig.has_body {
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

                self.load_params(&sig.params);

                self.check_block(&block.node, Some((*name, sig.result)))?;
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
                    p::PostSig::Directive(p::Directive::Forward) => false,
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

    fn check_stmt(
        &mut self,
        stmt: Spanned<&s::MaybeLabelledStmt>,
        func: Option<(Ident, TypeId)>,
    ) -> AnalysisResult<()> {
        let s::MaybeLabelledStmt { label, stmt } = stmt.node;
        if let Some(label) = label {
            self.context.lookup_label(label.node, label.span)?;
        }
        match &stmt.node {
            s::Stmt::Empty => (),
            s::Stmt::Assign { var, value } => {
                if let (Some((func_name, result_type)), e::Var::Plain(name)) = (func, &var.node)
                    && func_name.node == *name
                {
                    let value_type =
                        self.infer_expr(value.as_ref(), Some((result_type, func_name.span)))?;
                    self.context.check_assign_compat(result_type, value_type, stmt.span)?;
                } else {
                    let var_type = self.infer_var(var.as_ref())?;
                    let value_type = self.infer_expr(value.as_ref(), Some((var_type, var.span)))?;
                    self.context.check_assign_compat(var_type, value_type, stmt.span)?;
                }
            }
            s::Stmt::ReadCall(_read_params) => todo!(),
            s::Stmt::ReadlnCall(_read_params) => todo!(),
            s::Stmt::WriteCall(_write_params) => todo!(),
            s::Stmt::WritelnCall(_write_params) => todo!(),
            s::Stmt::ProcCall { name, args } => self.check_proc_call(*name, args.as_deref())?,
            s::Stmt::Goto(label) => {
                self.context.lookup_label(*label, stmt.span)?;
            }
            s::Stmt::Compound(stmts) => {
                self.context.enter_scope();
                for stmt in stmts {
                    self.check_stmt(stmt.as_ref(), func)?;
                }
                self.context.exit_scope();
            }
            s::Stmt::If { cond, then, r#else } => {
                let cond_type = self.infer_expr(cond.as_ref(), None)?;
                if cond_type != self.context.boolean.into() {
                    return Err(AnalysisError::TypeMismatch {
                        got: cond_type,
                        at: cond.span,
                        expected: self.context.boolean.into(),
                        origin: stmt.span,
                    });
                }
                self.check_stmt(then.as_ref(), func)?;
                if let Some(r#else) = r#else {
                    self.check_stmt(r#else.as_ref(), func)?;
                }
            }
            s::Stmt::Case { index, cases } => {
                let index_type = self.infer_expr(index.as_ref(), None)?;
                for case in &cases.node {
                    let s::Case { labels, body } = &case.node;
                    for label in &labels.node {
                        let (label_type, _label_const) =
                            self.context.convert_constexpr(label.as_ref())?;
                        if index_type != label_type {
                            return Err(AnalysisError::TypeMismatch {
                                got: label_type,
                                at: label.span,
                                expected: index_type,
                                origin: labels.span,
                            });
                        }
                    }
                    self.check_stmt(body.as_ref(), func)?;
                }
            }
            s::Stmt::While { cond, body } => {
                let cond_type = self.infer_expr(cond.as_ref(), None)?;
                if cond_type != self.context.boolean.into() {
                    return Err(AnalysisError::TypeMismatch {
                        got: cond_type,
                        at: cond.span,
                        expected: self.context.boolean.into(),
                        origin: stmt.span,
                    });
                }
                self.check_stmt(body.as_ref(), func)?;
            }
            s::Stmt::Repeat { body, cond } => {
                self.context.enter_scope();
                for stmt in &body.node {
                    self.check_stmt(stmt.as_ref(), func)?;
                }
                self.context.exit_scope();
                let cond_type = self.infer_expr(cond.as_ref(), None)?;
                if cond_type != self.context.boolean.into() {
                    return Err(AnalysisError::TypeMismatch {
                        got: cond_type,
                        at: cond.span,
                        expected: self.context.boolean.into(),
                        origin: stmt.span,
                    });
                }
            }
            s::Stmt::For { control_var, from, direction, to, body } => {
                let from_type = self.infer_expr(from.as_ref(), None)?;
                let to_type = self.infer_expr(to.as_ref(), None)?;
                if from_type != to_type {
                    return Err(AnalysisError::TypeMismatch {
                        got: to_type,
                        at: to.span,
                        expected: from_type,
                        origin: stmt.span,
                    });
                }
                self.context.enter_scope();
                self.context.insert_def(
                    control_var.node,
                    DefKind::Var(from_type),
                    control_var.span,
                );
                self.check_stmt(body.as_ref(), func)?;
                self.context.exit_scope();
            }
            s::Stmt::With { vars, body } => {
                self.context.enter_scope();
                for var in &vars.node {
                    let var_type = self.infer_var(var.as_ref())?;
                    let TypeKind::Record { packed, fixed, variant } =
                        self.context.get_type(var_type)
                    else {
                        return Err(AnalysisError::MismatchType {
                            got: var_type,
                            at: var.span,
                            expected: "record variable",
                            reason: stmt.span,
                        });
                    };
                    let fields: Vec<_> =
                        Self::get_fields(fixed, variant.as_ref()).cloned().collect();
                    for field in fields {
                        let DefPoint::UserDef { name, kind, span } =
                            self.context.get_def(field.into()).clone()
                        else {
                            unreachable!()
                        };
                        self.context.insert_def(name, kind, span);
                    }
                }
                self.check_stmt(body.as_ref(), func)?;
                self.context.exit_scope();
            }
        }

        Ok(())
    }

    fn check_proc_call(
        &mut self,
        name: Ident,
        args: Spanned<&[e::SpanExpr]>,
    ) -> AnalysisResult<()> {
        let def = self.context.lookup(name.node, name.span)?;
        let DefPoint::UserDef { kind: DefKind::Proc(ProcSig { params, .. }), span, .. } =
            self.context.get_def(def).clone()
        else {
            return Err(AnalysisError::MismatchDef {
                got: def,
                expected: "procedure",
                origin: name.span,
            });
        };

        self.check_args(&params, args, span)
    }

    fn load_params(&mut self, params: &[ParamSection]) {
        for param in ParamIter::from(params) {
            let param_def = param.into();
            let param_name = self.context.get_def_name(param_def);
            self.context.curr_scope_mut().insert(param_name, param_def);
        }
    }

    fn convert_params(
        &mut self,
        params: &[Spanned<p::Param>],
    ) -> AnalysisResult<Vec<ParamSection>> {
        let mut new_params = Vec::with_capacity(params.len()); // underestimate
        for param in params {
            match &param.node {
                p::Param::Value(names, r#type) => {
                    let section =
                        self.convert_param_section(&names.node, r#type.as_ref(), ParamKind::Value)?;
                    new_params.push(section);
                }
                p::Param::Var(names, r#type) => {
                    let section =
                        self.convert_param_section(&names.node, r#type.as_ref(), ParamKind::Var)?;
                    new_params.push(section);
                }
                p::Param::Proc(proc_sig) => {
                    let params = self.convert_params(&proc_sig.params)?;
                    new_params.push(ParamSection::One(self.context.create_param(
                        proc_sig.name.node,
                        param.span,
                        ParamKind::Proc(params),
                    )));
                }
                p::Param::Func(func_sig) => {
                    let params = self.convert_params(&func_sig.params.node)?;
                    let result =
                        self.context.lookup_type(func_sig.result.node, func_sig.result.span)?;

                    new_params.push(ParamSection::One(self.context.create_param(
                        func_sig.name.node,
                        param.span,
                        ParamKind::Func { params, result },
                    )));
                }
            }
        }

        Ok(new_params)
    }

    fn convert_param_section(
        &mut self,
        names: &[Ident],
        r#type: Spanned<&p::ParamType>,
        param_kind_ctor: fn(ParamType) -> ParamKind,
    ) -> AnalysisResult<ParamSection> {
        let r#type = self.convert_param_type(r#type)?;
        match names {
            &[] => unreachable!(),
            &[sole] => Ok(ParamSection::One(self.context.create_param(
                sole.node,
                sole.span,
                param_kind_ctor(r#type),
            ))),
            &[first, ref rest @ ..] => {
                let mut many = Vec::from([self.context.create_param(
                    first.node,
                    first.span,
                    param_kind_ctor(r#type.clone()),
                )]);
                for name in rest {
                    many.push(self.context.create_param(
                        name.node,
                        name.span,
                        param_kind_ctor(r#type.clone()),
                    ));
                }
                Ok(ParamSection::Many(many))
            }
        }
    }

    fn convert_param_type(&mut self, r#type: Spanned<&p::ParamType>) -> AnalysisResult<ParamType> {
        match r#type.node {
            p::ParamType::TypeIdent(spur) => {
                self.context.lookup_type(*spur, r#type.span).map(ParamType::TypeIdent)
            }
            p::ParamType::ArraySchema(_array_schema) => {
                unimplemented!("Array schemata are currently unsupported")
            }
        }
    }

    fn infer_expr(
        &mut self,
        expr: Spanned<&e::Expr>,
        expected: Option<(TypeId, Span)>,
    ) -> AnalysisResult<TypeId> {
        match expr.node {
            e::Expr::Var(var) => self.infer_var(expr.map(|_| var)),
            e::Expr::Nil => {
                if let Some((expected, origin)) = expected {
                    if let TypeKind::Pointer(inner_type) = self.context.get_type(expected) {
                        Ok(expected)
                    } else {
                        Err(AnalysisError::TypeMismatch {
                            got: self.context.pointer_of_t,
                            at: expr.span,
                            expected,
                            origin,
                        })
                    }
                } else {
                    Err(AnalysisError::UnexpectedNil(expr.span))
                }
            }
            e::Expr::UIntLit(_) => Ok(self.context.integer.into()),
            e::Expr::URealLit(_) => Ok(self.context.real),
            e::Expr::StrLit(str) => Ok(self.context.infer_string(str)),
            e::Expr::Set(elements) => self.infer_set(elements, expected, expr.span),
            e::Expr::FuncCall { name, args } => {
                self.infer_fun_call(*name, args.as_deref(), expr.span)
            }
            e::Expr::UnaryOp { op, operand } => todo!(),
            e::Expr::BinOp { op, left, right } => todo!(),
        }
    }

    fn infer_set(
        &mut self,
        elements: &[Spanned<e::SetMember>],
        expected: Option<(TypeId, Span)>,
        expr_span: Span,
    ) -> AnalysisResult<TypeId> {
        // Packedness is inferred from context, and if that information is not available it defaults to unpacked
        let (packed, member_type) = if let Some((expected, origin)) = expected {
            if let TypeKind::Set { packed, elem } = self.context.get_type(expected) {
                (*packed, Some(elem))
            } else {
                return Err(AnalysisError::TypeMismatch {
                    got: self.context.set_of_t,
                    at: expr_span,
                    expected,
                    origin,
                });
            }
        } else {
            (false, None)
        };
        if let Some((first, rest)) = elements.split_first() {
            let e::SetMember { start, end } = &first.node;
            let member_type = match member_type {
                Some(r#type) => (*r#type).into(),
                None => self.infer_expr(start.as_ref(), None)?,
            };
            if let Some(end) = end.as_ref() {
                let end_type = self.infer_expr(end.as_ref(), None)?;
                self.context.check_assign_compat(member_type, end_type, end.span)?;
            }
            for elem in rest {
                let e::SetMember { start, end } = &elem.node;
                let start_type = self.infer_expr(start.as_ref(), None)?;
                self.context.check_assign_compat(member_type, start_type, start.span)?;
                if let Some(end) = end {
                    let end_type = self.infer_expr(end.as_ref(), None)?;
                    self.context.check_assign_compat(member_type, end_type, end.span)?;
                }
            }
            let ty = TypeKind::Set {
                packed,
                elem: self.context.to_ordinal(member_type, first.span, expr_span)?,
            };
            Ok(self.context.fresh(ty))
        } else {
            if let Some((expected, origin)) = expected {
                if let TypeKind::Set { .. } = self.context.get_type(expected) {
                    Ok(expected)
                } else {
                    Err(AnalysisError::TypeMismatch {
                        got: self.context.set_of_t,
                        at: expr_span,
                        expected,
                        origin,
                    })
                }
            } else {
                Err(AnalysisError::UnexpectedEmptySet(expr_span))
            }
        }
    }

    fn infer_fun_call(
        &mut self,
        name: Ident,
        args: Spanned<&[e::SpanExpr]>,
        span: Span,
    ) -> AnalysisResult<TypeId> {
        let def = self.context.lookup(name.node, name.span)?;
        let DefPoint::UserDef {
            kind: DefKind::Func(FuncSig { params, result, .. }),
            span: func_sig_span,
            ..
        } = self.context.get_def(def).clone()
        else {
            return Err(AnalysisError::MismatchDef {
                got: def,
                expected: "function",
                origin: name.span,
            });
        };

        self.check_args(&params, args, func_sig_span)?;

        Ok(result)
    }

    fn check_args(
        &mut self,
        params: &[ParamSection],
        args: Spanned<&[e::SpanExpr]>,
        call_span: Span,
    ) -> AnalysisResult<()> {
        let params: Vec<_> = ParamIter::from(params)
            .map(|param_id| {
                let param_def = param_id.into();
                if let DefPoint::UserDef { kind: DefKind::Param(param), span, .. } =
                    self.context.get_def(param_def)
                {
                    (param_id, param.clone(), *span)
                } else {
                    unreachable!()
                }
            })
            .collect();

        for ((param_id, param, param_span), arg) in params.into_iter().zip(args.node.iter()) {
            let def_type = match arg.node {
                e::Expr::Var(e::Var::Plain(name)) => {
                    let def = self.context.lookup(name, arg.span)?;
                    Some((def, self.context.get_def(def)))
                }
                _ => None,
            };
            match param {
                ParamKind::Value(ParamType::TypeIdent(param)) => {
                    let arg_type = self.infer_expr(arg.as_ref(), Some((param, param_span)))?;
                    self.context.check_assign_compat(param, arg_type, arg.span)?;
                }
                ParamKind::Var(ParamType::TypeIdent(param)) => {
                    let r#type = match def_type {
                        Some((_, DefPoint::BuiltinVar(bv))) => self.context.builtin_var(*bv),
                        Some((_, DefPoint::UserDef { kind: DefKind::Var(r#type), .. })) => *r#type,
                        Some((got, _)) => {
                            return Err(AnalysisError::MismatchDef {
                                got,
                                expected: "variable argument",
                                origin: arg.span,
                            });
                        }
                        None => {
                            return Err(AnalysisError::ExpectedIdentifier {
                                kind: "variable",
                                at: arg.span,
                            });
                        }
                    };
                    self.context.check_assign_compat(param, r#type, arg.span)?;
                }
                ParamKind::Proc(formal_params) => match def_type {
                    Some((
                        actual,
                        DefPoint::UserDef {
                            kind: DefKind::Proc(ProcSig { params: actual_params, .. }),
                            ..
                        },
                    )) => {
                        self.check_param_lists_congruity(
                            param_id,
                            actual,
                            &formal_params,
                            actual_params,
                            arg.span,
                        )?;
                    }
                    Some((got, _)) => {
                        return Err(AnalysisError::MismatchDef {
                            got,
                            expected: "user-defined procedure",
                            origin: arg.span,
                        });
                    }
                    None => {
                        return Err(AnalysisError::ExpectedIdentifier {
                            kind: "user-defined procedure",
                            at: arg.span,
                        });
                    }
                },
                ParamKind::Func { params: formal_params, result: formal_result } => {
                    match def_type {
                        Some((
                            actual,
                            DefPoint::UserDef {
                                kind:
                                    DefKind::Func(FuncSig {
                                        params: actual_params,
                                        result: actual_result,
                                        ..
                                    }),
                                ..
                            },
                        )) => {
                            self.check_param_lists_congruity(
                                param_id,
                                actual,
                                &formal_params,
                                actual_params,
                                arg.span,
                            )?;
                            if *actual_result != formal_result {
                                return Err(AnalysisError::TypeMismatch {
                                    got: *actual_result,
                                    at: arg.span,
                                    expected: formal_result,
                                    origin: call_span,
                                });
                            }
                        }
                        Some((got, _)) => {
                            return Err(AnalysisError::MismatchDef {
                                got,
                                expected: "user-defined function",
                                origin: arg.span,
                            });
                        }
                        None => {
                            return Err(AnalysisError::ExpectedIdentifier {
                                kind: "user-defined function",
                                at: arg.span,
                            });
                        }
                    }
                }
                ParamKind::Value(ParamType::ArraySchema(_))
                | ParamKind::Var(ParamType::ArraySchema(_)) => unimplemented!(),
            }
        }

        Ok(())
    }

    fn check_param_lists_congruity(
        &self,
        formal: ParamId,
        actual: DefId,
        formal_params: &[ParamSection],
        actual_params: &[ParamSection],
        origin: Span,
    ) -> AnalysisResult<()> {
        let formal_flat_count = ParamIter::from(formal_params).count();
        let actual_flat_count = ParamIter::from(actual_params).count();

        if formal_params.len() != actual_params.len() || formal_flat_count != actual_flat_count {
            return Err(AnalysisError::RoutineParamsShapeMismatch { formal, actual, origin });
        }

        for (formal_param, actual_param) in
            formal_params.iter().cloned().zip(actual_params.iter().cloned())
        {
            match (formal_param, actual_param) {
                (ParamSection::One(p1), ParamSection::One(p2)) => {
                    self.check_params_congruity(formal, actual, p1, p2, origin)?;
                }
                (ParamSection::Many(ps1), ParamSection::Many(ps2)) => {
                    if ps1.len() == ps2.len() {
                        for (p1, p2) in ps1.iter().cloned().zip(ps2.iter().cloned()) {
                            self.check_params_congruity(formal, actual, p1, p2, origin)?;
                        }
                    } else {
                        return Err(AnalysisError::RoutineParamsShapeMismatch {
                            formal,
                            actual,
                            origin,
                        });
                    }
                }
                _ => {
                    return Err(AnalysisError::RoutineParamsShapeMismatch {
                        formal,
                        actual,
                        origin,
                    });
                }
            }
        }
        Ok(())
    }

    fn check_params_congruity(
        &self,
        formal: ParamId,
        actual: DefId,
        formal_param: ParamId,
        actual_param: ParamId,
        origin: Span,
    ) -> AnalysisResult<()> {
        let DefPoint::UserDef { kind: DefKind::Param(formal_kind), .. } =
            self.context.get_def((formal_param).into())
        else {
            unreachable!()
        };
        let DefPoint::UserDef { kind: DefKind::Param(actual_kind), span: actual_span, .. } =
            self.context.get_def((actual_param).into())
        else {
            unreachable!()
        };

        match (formal_kind, actual_kind) {
            (
                ParamKind::Value(ParamType::TypeIdent(t1)),
                ParamKind::Value(ParamType::TypeIdent(t2)),
            )
            | (
                ParamKind::Var(ParamType::TypeIdent(t1)),
                ParamKind::Var(ParamType::TypeIdent(t2)),
            ) if t1 == t2 => Ok(()),
            (ParamKind::Proc(ps1), ParamKind::Proc(ps2)) => self.check_param_lists_congruity(
                formal_param,
                actual_param.into(),
                ps1,
                ps2,
                origin,
            ),
            (
                ParamKind::Func { params: ps1, result: r1 },
                ParamKind::Func { params: ps2, result: r2 },
            ) => {
                self.check_param_lists_congruity(
                    formal_param,
                    actual_param.into(),
                    ps1,
                    ps2,
                    origin,
                )?;
                if r1 == r2 {
                    Ok(())
                } else {
                    Err(AnalysisError::TypeMismatch {
                        got: *r2,
                        at: *actual_span,
                        expected: *r1,
                        origin,
                    })
                }
            }
            _ => Err(AnalysisError::Incongruous {
                formal,
                actual,
                formal_param,
                actual_param,
                origin,
            }),
        }
    }

    fn infer_var(&mut self, var: Spanned<&e::Var>) -> AnalysisResult<TypeId> {
        match var.node {
            e::Var::Plain(spur) => {
                let def = self.context.lookup(*spur, var.span)?;
                match self.context.get_def(def) {
                    DefPoint::BuiltinVar(bv) => match bv {
                        BuiltinVar::Input => Ok(self.context.text),
                        BuiltinVar::Output => Ok(self.context.text),
                    },
                    DefPoint::UserDef { kind: DefKind::Var(var_type), .. } => Ok(*var_type),
                    _ => Err(AnalysisError::MismatchDef {
                        got: def,
                        expected: "variable",
                        origin: var.span,
                    }),
                }
            }
            e::Var::Ref(inner_var) => {
                let inner_var_type = self.infer_var(inner_var.as_ref().as_ref())?;
                let TypeKind::Pointer(r#type) = self.context.get_type(inner_var_type) else {
                    return Err(AnalysisError::MismatchType {
                        got: inner_var_type,
                        at: inner_var.span,
                        expected: "pointer type",
                        reason: var.span,
                    });
                };

                Ok(*r#type)
            }
            e::Var::Indexed(array_var, subscripts) => {
                let array_var_type = self.infer_var(array_var.as_ref().as_ref())?;
                let TypeKind::Array { indices, elem, .. } =
                    self.context.get_type(array_var_type).clone()
                else {
                    return Err(AnalysisError::MismatchType {
                        got: array_var_type,
                        at: array_var.span,
                        expected: "array type",
                        reason: subscripts.span,
                    });
                };

                for (index_type, subscript) in indices.iter().zip(subscripts.node.iter()) {
                    let subscript_type = self.infer_expr(subscript.as_ref(), None)?;
                    self.context.check_assign_compat(
                        (*index_type).into(),
                        subscript_type,
                        subscript.span,
                    )?;
                }

                Ok(elem)
            }
            e::Var::FieldAccess { record, dot_span, field } => {
                let record_type = self.infer_var(record.as_ref().as_ref())?;
                let TypeKind::Record { fixed, variant, .. } = self.context.get_type(record_type)
                else {
                    return Err(AnalysisError::MismatchType {
                        got: record_type,
                        at: record.span,
                        expected: "record type",
                        reason: var.span,
                    });
                };

                let field = Self::get_fields(fixed, variant.as_ref())
                    .find(|f| self.context.get_def_name((**f).into()) == field.node)
                    .ok_or(AnalysisError::MissingField {
                        record: record_type,
                        name: field.node,
                        at: *dot_span + field.span,
                    })?;

                let DefPoint::UserDef { kind: DefKind::Field { r#type, .. }, .. } =
                    self.context.get_def((*field).into())
                else {
                    unreachable!()
                };

                Ok(*r#type)
            }
        }
    }

    fn get_fields<'ctx>(
        fixed: &'ctx [FieldId],
        variant: Option<&'ctx VariantPart>,
    ) -> impl Iterator<Item = &'ctx FieldId> {
        fixed.iter().chain(variant.into_iter().flat_map(|variant| {
            variant.variants.iter().flat_map(|variant| variant.fields.fixed.iter())
        }))
    }

    pub fn dump_context(&self) {
        self.context.dump(&self.rodeo);
    }
}

/// Dummy span for builtins
fn span_bltn<T: Debug + Clone>(node: T) -> Spanned<T> {
    Spanned { span: (0..0).into(), node }
}
