use std::{cell::RefCell, iter};

use builtins::{Builtin, BuiltinConst, BuiltinFunc, BuiltinProc, BuiltinType, BuiltinVar};
use context::{DefId, DefKind, DefPoint, ParamId, ParamKind, ParamType, TypeId, TypingContext};
use lasso::Rodeo;
use strum::IntoEnumIterator;

pub mod builtins;
pub mod context;
mod errors;

use crate::{
    ast::{
        Ident, expr as e,
        program::{self as p},
        stmt as s,
    },
    sema::{
        context::{self as ctx, Constant, FieldId, ParamIter, ParamSection, TypeKind, VariantPart},
        errors::{AnalysisError, Expected},
    },
    tast::{
        Typed,
        expr::{Arg, BinOp, Expr, Place, SetMember, TypedExpr, UnaryOp},
        program::{
            Block, ConstDef, FuncSig, ProcSig, Program,
            RoutineDecl::{self, FuncFwd},
            TypeDef, VarDecl,
        },
        stmt::{Case, MaybeLabelledStmt, Stmt},
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

    pub fn check_program(&mut self, program: &'ast p::Program) -> AnalysisResult<Program> {
        self.context.enter_scope(); // Program scope

        let params = program
            .params
            .node
            .iter()
            .map(|param| self.context.insert_program_param(param.node, param.span))
            .collect::<Result<Vec<_>, _>>()?;

        let block = self.check_block(&program.block.node, None)?;

        self.context.exit_scope(); // Program scope
        self.context.exit_scope(); // Builtins scope

        Ok(Program { name: program.name.node, params, block })
    }

    fn check_block(
        &mut self,
        block: &'ast p::Block,
        func: Option<(Ident, TypeId)>,
    ) -> AnalysisResult<Block> {
        let p::Block { label_decls, const_defs, type_defs, var_decls, routine_decls, stmts } =
            block;

        let label_decls = label_decls
            .node
            .iter()
            .map(|label| {
                self.context.insert_label(*label)?;
                Ok(label.node)
            })
            .collect::<Result<_, _>>()?;

        let const_defs = const_defs
            .node
            .iter()
            .map(|cd| {
                let p::ConstDef { name, value } = &cd.node;
                let (r#type, value) = self.context.convert_constexpr(value.as_ref())?;
                let def = self.context.insert_const_def(name.node, name.span, value, r#type)?;
                Ok(ConstDef { name: name.node, def, value, r#type })
            })
            .collect::<Result<Vec<_>, _>>()?;

        let type_defs = type_defs
            .node
            .iter()
            .map(|type_def| {
                let p::TypeDef { name, def } = &type_def.node;
                let r#type = self.context.convert_type(def.as_ref())?;
                self.context.insert_def(name.node, DefKind::Type(r#type), type_def.span)?;
                Ok(TypeDef { name: name.node, def: r#type })
            })
            .collect::<Result<Vec<_>, _>>()?;

        // TODO: Switch this over to itertools::process_results maybe? this seems really slow
        let var_decls = var_decls
            .node
            .iter()
            .map(|var_decl| {
                let p::VarDecl { names, r#type } = &var_decl.node;
                let r#type = self.context.convert_type(r#type.as_ref())?;
                names
                    .node
                    .iter()
                    .map(|name| {
                        let def =
                            self.context.insert_def(name.node, DefKind::Var(r#type), name.span)?;
                        Ok(VarDecl { name: name.node, def, r#type })
                    })
                    .collect::<Result<Vec<_>, _>>()
            })
            .try_fold(Vec::new(), |mut acc, decls| {
                decls.map(|decls| {
                    acc.extend_from_slice(&decls);
                    acc
                })
            })?;

        let routine_decls = routine_decls
            .node
            .iter()
            .map(|routine_decl| match &routine_decl.node {
                p::RoutineDecl::Proc(proc_decl) => self.check_proc(proc_decl, routine_decl.span),
                p::RoutineDecl::Func(func_decl) => self.check_func(func_decl, routine_decl.span),
            })
            .collect::<Result<Vec<_>, _>>()?;

        let stmts = stmts
            .node
            .iter()
            .map(|stmt| self.check_stmt(stmt.as_ref(), func))
            .collect::<Result<Vec<_>, _>>()?;

        // self.context.exit_scope();

        Ok(Block { label_decls, const_defs, type_defs, var_decls, routine_decls, stmts })
    }

    fn check_proc(&mut self, proc_decl: &p::ProcDecl, span: Span) -> AnalysisResult<RoutineDecl> {
        let p::ProcDecl { sig, post } = proc_decl;
        let p::ProcSig { name, params } = &sig.node;

        println!("\nProcedure: {:?}", name.map(|n| self.rodeo.resolve(&n)));
        let has_body = match post.node {
            p::PostSig::Block(_) => true,
            p::PostSig::Directive(p::Directive::Forward) => false,
            _ => unimplemented!(),
        };
        let def = self.context.insert_def(
            name.node,
            DefKind::Proc(ctx::ProcSig { params: Vec::new(), has_body }),
            span,
        )?;

        let params = self.convert_params(params, def)?;

        // Need to update the def for recursive calls to work
        self.context.update_def(def, |def| match def {
            // Could've done `std::mem::swap` but then we'd have to grab and clone `params`
            DefPoint::UserDef {
                kind: DefKind::Proc(ctx::ProcSig { params: placeholder, .. }),
                ..
            } => placeholder.extend_from_slice(&params),
            _ => unreachable!(),
        });

        let body = match &post.node {
            p::PostSig::Block(block) => {
                self.context.enter_scope();
                self.load_params(&params);
                let block = self.check_block(block, None)?;
                self.context.exit_scope();
                Some(block)
            }
            p::PostSig::Directive(p::Directive::Forward) => None,
            _ => unimplemented!(),
        };
        let params = ParamIter::from(&params).collect::<Vec<_>>();
        let sig = ProcSig { name: name.node, params, def };

        Ok(match body {
            Some(body) => RoutineDecl::Proc { sig, body },
            None => RoutineDecl::ProcFwd(sig),
        })
    }

    fn check_func(&mut self, func_decl: &p::FuncDecl, span: Span) -> AnalysisResult<RoutineDecl> {
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

                Ok(FuncFwd(FuncSig {
                    name: name.node,
                    def,
                    params: sig.params_iter().collect::<Vec<_>>(),
                    result: sig.result,
                }))
            }
            p::FuncDecl::Heading(sig, post_sig) => {
                println!("\nFunction: {:?}", sig.node.name.map(|n| self.rodeo.resolve(&n)));

                let p::FuncSig { name, params, result } = &sig.node;

                let has_body = match post_sig.node {
                    p::PostSig::Block(_) => true,
                    p::PostSig::Directive(p::Directive::Forward) => false,
                    _ => unimplemented!(),
                };
                let result = self.context.lookup_type(result.node, result.span)?;
                let def = self.context.insert_def(
                    name.node,
                    DefKind::Func(ctx::FuncSig { params: Vec::new(), result, has_body }),
                    span,
                )?;

                let params = self.convert_params(&params.node, def)?;
                self.context.update_def(def, |def| match def {
                    DefPoint::UserDef {
                        kind: DefKind::Func(ctx::FuncSig { params: placeholder, .. }),
                        ..
                    } => placeholder.extend_from_slice(&params),
                    _ => unreachable!(),
                });

                let body = match &post_sig.node {
                    p::PostSig::Block(block) => {
                        self.context.enter_scope();
                        self.load_params(&params);
                        let body = self.check_block(block, Some((*name, result)))?;
                        self.context.exit_scope();
                        Some(body)
                    }
                    p::PostSig::Directive(p::Directive::Forward) => None,
                    _ => unimplemented!(),
                };

                let sig = FuncSig {
                    name: name.node,
                    def,
                    params: ParamIter::from(&params).collect::<Vec<_>>(),
                    result,
                };
                Ok(match body {
                    Some(body) => RoutineDecl::Func { sig, body },
                    None => RoutineDecl::FuncFwd(sig),
                })
            }
        }
    }

    fn check_stmt(
        &mut self,
        stmt: Spanned<&s::MaybeLabelledStmt>,
        func: Option<(Ident, TypeId)>,
    ) -> AnalysisResult<MaybeLabelledStmt> {
        let s::MaybeLabelledStmt { label, stmt } = stmt.node;
        if let Some(label) = label {
            self.context.lookup_label(label.node, label.span)?;
        }
        let label = label.map(Spanned::get_node);
        match &stmt.node {
            s::Stmt::Empty => Ok(MaybeLabelledStmt { label, stmt: Stmt::Empty }),
            s::Stmt::Assign { var, value } => {
                if let (Some((func_name, result_type)), e::Var::Plain(name)) = (func, &var.node)
                    && func_name.node == *name
                {
                    let value =
                        self.infer_expr(value.as_ref(), Some((result_type, func_name.span)))?;
                    self.context.check_assign_compat(result_type, value.r#type, stmt.span)?;
                    Ok(MaybeLabelledStmt { label, stmt: Stmt::SetReturn(value) })
                } else {
                    let typed_var = self.infer_assign_var(var.as_ref())?;
                    let value =
                        self.infer_expr(value.as_ref(), Some((typed_var.r#type, var.span)))?;
                    self.context.check_assign_compat(typed_var.r#type, value.r#type, stmt.span)?;
                    Ok(MaybeLabelledStmt { label, stmt: Stmt::Assign { var: typed_var, value } })
                }
            }
            s::Stmt::ReadCall(_read_params) => todo!(),
            s::Stmt::ReadlnCall(_read_params) => todo!(),
            s::Stmt::WriteCall(_write_params) => todo!(),
            s::Stmt::WritelnCall(_write_params) => todo!(),
            s::Stmt::ProcCall { name, args } => self
                .check_proc_call(*name, args.as_deref())
                .map(|stmt| MaybeLabelledStmt { label, stmt }),
            s::Stmt::Goto(goto_label) => {
                self.context.lookup_label(*goto_label, stmt.span)?;
                Ok(MaybeLabelledStmt { label, stmt: Stmt::Goto(*goto_label) })
            }
            s::Stmt::Compound(stmts) => {
                self.context.enter_scope();
                let stmts = stmts
                    .iter()
                    .map(|stmt| self.check_stmt(stmt.as_ref(), func))
                    .collect::<Result<Vec<_>, _>>()?;
                // for stmt in stmts {
                //     self.check_stmt(stmt.as_ref(), func)?;
                // }
                self.context.exit_scope();
                Ok(MaybeLabelledStmt { label, stmt: Stmt::Compound(stmts) })
            }
            s::Stmt::If { cond: old_cond, then, r#else } => {
                let cond = self.infer_expr(old_cond.as_ref(), None)?;
                if cond.r#type != self.context.boolean.into() {
                    return Err(AnalysisError::TypeMismatch {
                        got: cond.r#type,
                        at: old_cond.span,
                        expected: Expected::One(self.context.boolean.into()),
                        reason: stmt.span,
                    });
                }
                let then = self.check_stmt(then.as_ref(), func).map(Box::new)?;
                let r#else = r#else
                    .as_ref()
                    .map(|r#else| self.check_stmt(r#else.as_ref(), func).map(Box::new))
                    .transpose()?;

                Ok(MaybeLabelledStmt { label, stmt: Stmt::If { cond, then, r#else } })
            }
            s::Stmt::Case { index, cases } => {
                let index = self.infer_expr(index.as_ref(), None)?;

                let cases = cases
                    .node
                    .iter()
                    .map(|case| {
                        let s::Case { labels, body } = &case.node;
                        let labels = labels
                            .node
                            .iter()
                            .map(|old_label| {
                                let (label_type, label_const) =
                                    self.context.convert_constexpr(old_label.as_ref())?;
                                if index.r#type != label_type {
                                    return Err(AnalysisError::TypeMismatch {
                                        got: label_type,
                                        at: old_label.span,
                                        expected: Expected::One(index.r#type),
                                        reason: labels.span,
                                    });
                                }

                                Ok((label_type, label_const))
                            })
                            .collect::<Result<Vec<_>, _>>()?;

                        let body = self.check_stmt(body.as_ref(), func)?;

                        Ok(Case { labels, body })
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                Ok(MaybeLabelledStmt { label, stmt: Stmt::Case { index, cases } })
            }
            s::Stmt::While { cond: old_cond, body } => {
                let cond = self.infer_expr(old_cond.as_ref(), None)?;
                if cond.r#type != self.context.boolean.into() {
                    return Err(AnalysisError::TypeMismatch {
                        got: cond.r#type,
                        at: old_cond.span,
                        expected: Expected::One(self.context.boolean.into()),
                        reason: stmt.span,
                    });
                }

                let body = self.check_stmt(body.as_ref(), func).map(Box::new)?;

                Ok(MaybeLabelledStmt { label, stmt: Stmt::While { cond, body } })
            }
            s::Stmt::Repeat { body, cond: old_cond } => {
                self.context.enter_scope();
                let body = body
                    .node
                    .iter()
                    .map(|stmt| self.check_stmt(stmt.as_ref(), func))
                    .collect::<Result<Vec<_>, _>>()?;
                self.context.exit_scope();

                let cond = self.infer_expr(old_cond.as_ref(), None)?;
                if cond.r#type != self.context.boolean.into() {
                    Err(AnalysisError::TypeMismatch {
                        got: cond.r#type,
                        at: old_cond.span,
                        expected: Expected::One(self.context.boolean.into()),
                        reason: stmt.span,
                    })
                } else {
                    Ok(MaybeLabelledStmt { label, stmt: Stmt::Repeat { body, cond } })
                }
            }
            s::Stmt::For { control_var, from: old_from, direction, to: old_to, body } => {
                let from = self.infer_expr(old_from.as_ref(), None)?;
                let direction = direction.node.into();
                let to = self.infer_expr(old_to.as_ref(), None)?;
                if from.r#type != to.r#type {
                    return Err(AnalysisError::TypeMismatch {
                        got: to.r#type,
                        at: old_to.span,
                        expected: Expected::One(from.r#type),
                        reason: stmt.span,
                    });
                }
                let control_var_type =
                    self.context.to_ordinal(from.r#type, old_from.span, stmt.span)?;
                // self.context.enter_scope();
                // Apparently the control-variable is not restricted to the body of the for loop 🤢
                let control_var_def = self.context.insert_def(
                    control_var.node,
                    DefKind::Var(from.r#type),
                    control_var.span,
                )?;
                let body = self.check_stmt(body.as_ref(), func).map(Box::new)?;
                // self.context.exit_scope();
                Ok(MaybeLabelledStmt {
                    label,
                    stmt: Stmt::For {
                        control_var: (control_var_type, control_var.node),
                        control_var_def,
                        from,
                        direction,
                        to,
                        body,
                    },
                })
            }
            s::Stmt::With { vars, body } => {
                self.context.enter_scope();
                let vars = vars
                    .node
                    .iter()
                    .map(|var| {
                        let record_var = self.infer_expr_var(var.as_ref())?;
                        let record_var_id =
                            self.context.create_with_record_var(record_var.r#type, var.span);

                        let TypeKind::Record { fixed, variant, .. } =
                            self.context.get_type(record_var.r#type)
                        else {
                            return Err(AnalysisError::TypeMismatch {
                                got: record_var.r#type,
                                at: var.span,
                                expected: Expected::Abstract("record variable"),
                                reason: stmt.span,
                            });
                        };

                        let fields: Vec<_> =
                            Self::get_fields(fixed, variant.as_ref()).cloned().collect();
                        for field in fields {
                            self.context.insert_with_field_alias(record_var_id, field, var.span)?;
                        }

                        Ok((record_var, record_var_id))
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                let body = self.check_stmt(body.as_ref(), func).map(Box::new)?;
                self.context.exit_scope();
                Ok(MaybeLabelledStmt { label, stmt: Stmt::With { vars, body } })
            }
        }
    }

    fn check_proc_call(
        &mut self,
        name: Ident,
        args: Spanned<&[e::SpanExpr]>,
    ) -> AnalysisResult<Stmt> {
        let def = self.context.lookup(name.node, name.span)?;
        let DefPoint::UserDef { kind: DefKind::Proc(ctx::ProcSig { params, .. }), span, .. } =
            self.context.get_def(def).clone()
        else {
            return Err(AnalysisError::MismatchDef {
                got: def,
                expected: "procedure",
                origin: name.span,
            });
        };

        self.check_args(&params, args, span)?;

        todo!()
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
        routine: DefId,
    ) -> AnalysisResult<Vec<ParamSection>> {
        let mut new_params = Vec::with_capacity(params.len()); // underestimate
        for param in params {
            match &param.node {
                p::Param::Value(names, r#type) => {
                    let section = self.convert_param_section(
                        &names.node,
                        r#type.as_ref(),
                        ParamKind::Value,
                        routine,
                    )?;
                    new_params.push(section);
                }
                p::Param::Var(names, r#type) => {
                    let section = self.convert_param_section(
                        &names.node,
                        r#type.as_ref(),
                        ParamKind::Var,
                        routine,
                    )?;
                    new_params.push(section);
                }
                p::Param::Proc(proc_sig) => {
                    let params = self.convert_params(&proc_sig.params, routine)?;
                    new_params.push(ParamSection::One(self.context.create_param(
                        proc_sig.name.node,
                        param.span,
                        ParamKind::Proc(params),
                        routine,
                    )));
                }
                p::Param::Func(func_sig) => {
                    let params = self.convert_params(&func_sig.params.node, routine)?;
                    let result =
                        self.context.lookup_type(func_sig.result.node, func_sig.result.span)?;

                    new_params.push(ParamSection::One(self.context.create_param(
                        func_sig.name.node,
                        param.span,
                        ParamKind::Func { params, result },
                        routine,
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
        routine: DefId,
    ) -> AnalysisResult<ParamSection> {
        let r#type = self.convert_param_type(r#type)?;
        match *names {
            [] => unreachable!(),
            [sole] => Ok(ParamSection::One(self.context.create_param(
                sole.node,
                sole.span,
                param_kind_ctor(r#type),
                routine,
            ))),
            [first, ref rest @ ..] => {
                let mut many = Vec::from([self.context.create_param(
                    first.node,
                    first.span,
                    param_kind_ctor(r#type.clone()),
                    routine,
                )]);
                for name in rest {
                    many.push(self.context.create_param(
                        name.node,
                        name.span,
                        param_kind_ctor(r#type.clone()),
                        routine,
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

    /// # Parameters
    ///
    /// * `expected` - This information is exclusively for inferring the concrete type of `nil` and set literals, in
    ///   cases where neither of those kinds of expressions are expected, `None` should be passed.
    fn infer_expr(
        &mut self,
        expr: Spanned<&e::Expr>,
        expected: Option<(TypeId, Span)>,
    ) -> AnalysisResult<TypedExpr> {
        match expr.node {
            e::Expr::Var(var) => self.infer_expr_var(expr.map(|_| var)),
            e::Expr::Nil => {
                if let Some((expected, reason)) = expected {
                    if let TypeKind::Pointer(_) = self.context.get_type(expected) {
                        Ok(Typed { node: Expr::Nil, r#type: expected })
                    } else {
                        Err(AnalysisError::TypeMismatch {
                            got: self.context.pointer_of_t,
                            at: expr.span,
                            expected: Expected::One(expected),
                            reason,
                        })
                    }
                } else {
                    Ok(Typed { node: Expr::Nil, r#type: self.context.pointer_of_t })
                }
            }
            &e::Expr::UIntLit(n) => {
                Ok(Typed { node: Expr::IntLit(n as i64), r#type: self.context.integer.into() })
            }
            &e::Expr::URealLit(n) => {
                Ok(Typed { node: Expr::RealLit(n), r#type: self.context.real })
            }
            &e::Expr::CharLit(c) => {
                Ok(Typed { node: Expr::CharLit(c), r#type: self.context.char.into() })
            }
            e::Expr::StrLit(str) => Ok(self.infer_string(str)),
            e::Expr::Set(members) => self.infer_set(members, expr.span, expected),
            e::Expr::FuncCall { name, args } => self.infer_fun_call(*name, args.as_deref()),
            e::Expr::UnaryOp { op, operand } => self.infer_unary_op(*op, operand.as_ref().as_ref()),
            e::Expr::BinOp { op, left, right } => {
                self.infer_binop(*op, left.as_ref().as_ref(), right.as_ref().as_ref(), expected)
            }
        }
    }

    fn infer_unary_op(
        &mut self,
        op: Spanned<e::UnaryOp>,
        operand: Spanned<&e::Expr>,
    ) -> AnalysisResult<TypedExpr> {
        let new_operand = self.infer_expr(operand, None)?;
        let operand_type = new_operand.r#type;
        match op.node {
            e::UnaryOp::Not if operand_type != self.context.boolean.into() => {
                Err(AnalysisError::TypeMismatch {
                    got: operand_type,
                    at: operand.span,
                    expected: Expected::One(self.context.boolean.into()),
                    reason: op.span,
                })
            }
            e::UnaryOp::Identity
                if [self.context.integer.into(), self.context.real].contains(&operand_type) =>
            {
                Err(AnalysisError::TypeMismatch {
                    got: operand_type,
                    at: operand.span,
                    expected: Expected::Abstract("Integer or Real"),
                    reason: op.span,
                })
            }
            e::UnaryOp::Negation
                if [self.context.integer.into(), self.context.real].contains(&operand_type) =>
            {
                Err(AnalysisError::TypeMismatch {
                    got: operand_type,
                    at: operand.span,
                    expected: Expected::Abstract("Integer or Real"),
                    reason: op.span,
                })
            }
            _ => {
                let op = match op.node {
                    e::UnaryOp::Not => UnaryOp::BNot,
                    e::UnaryOp::Identity => return Ok(new_operand),
                    e::UnaryOp::Negation => match self.context.get_type(operand_type) {
                        TypeKind::Integer => UnaryOp::INeg,
                        TypeKind::Real => UnaryOp::RNeg,
                        _ => unreachable!(),
                    },
                };
                Ok(Typed {
                    node: Expr::UnaryOp { op, operand: Box::new(new_operand.clone()) },
                    r#type: operand_type,
                })
            }
        }
    }

    fn infer_string(&mut self, s: &str) -> TypedExpr {
        if s.len() == 1 {
            Typed {
                node: Expr::CharLit(s.bytes().nth(0).unwrap()),
                r#type: self.context.char.into(),
            }
        } else {
            let subrange = self.context.make_subrange(self.context.integer, 1, s.len() as i64 - 1);
            let ty = TypeKind::Array {
                packed: true,
                indices: vec![subrange],
                elem: self.context.char.into(),
            };
            Typed {
                node: Expr::StrLit(self.context.get_or_intern_string(s)),
                r#type: self.context.fresh(ty),
            }
        }
    }

    // This function feels like a war crime 😭
    fn infer_binop(
        &mut self,
        op: Spanned<e::BinOp>,
        left: Spanned<&e::Expr>,
        right: Spanned<&e::Expr>,
        expected: Option<(TypeId, Span)>,
    ) -> AnalysisResult<TypedExpr> {
        // If the operator is a valid set operator which also outputs a set, the expected type is significant
        let expected = if let e::BinOp::Add | e::BinOp::Sub | e::BinOp::Mul = op.node {
            expected
        } else {
            None
        };

        let new_left = self.infer_expr(left, expected).map(Box::new)?;
        let left_type = new_left.r#type;
        let left = left.map(|_| self.context.widen_type(left_type));

        let new_right = self.infer_expr(right, expected).map(Box::new)?;
        let right_type = new_right.r#type;
        let right = right.map(|_| self.context.widen_type(right_type));

        let (int, real, boole) =
            (self.context.integer.into(), self.context.real, self.context.boolean.into());
        let num = [int, real];

        let left_type_kind = self.context.get_type(left_type).clone();
        let right_type_kind = self.context.get_type(right_type).clone();

        let out_ty = match op.node {
            e::BinOp::Add | e::BinOp::Sub | e::BinOp::Mul => {
                if num.contains(&left_type) {
                    if num.contains(&right_type) {
                        match (&left_type_kind, &right_type_kind) {
                            (TypeKind::Integer, TypeKind::Integer) => Ok(int),
                            _ => Ok(real),
                        }
                    } else {
                        Err(AnalysisError::TypeMismatch {
                            got: right_type,
                            at: right.span,
                            expected: Expected::Abstract("Integer, Real or Set of T"),
                            reason: op.span,
                        })
                    }
                } else if let Some(target) = self.infer_set_type_operands::<false>(
                    left,
                    right,
                    &left_type_kind,
                    &right_type_kind,
                    op.span,
                )? {
                    Ok(target)
                } else {
                    Err(AnalysisError::TypeMismatch {
                        got: left_type,
                        at: left.span,
                        expected: Expected::Abstract("Integer, Real or Set of T"),
                        reason: op.span,
                    })
                }
            }
            e::BinOp::Quot => self.infer_binop_simple(&num, left, right, real, op.span),
            e::BinOp::Div | e::BinOp::Mod => {
                self.infer_binop_simple(&[int], left, right, int, op.span)
            }
            e::BinOp::And | e::BinOp::Or => {
                self.infer_binop_simple(&[boole], left, right, boole, op.span)
            }
            e::BinOp::Eq | e::BinOp::NEq => {
                if self.infer_binop_cmp_common(
                    left,
                    right,
                    &left_type_kind,
                    &right_type_kind,
                    op.span,
                )? {
                    Ok(boole)
                } else if let TypeKind::Pointer(_) = left_type_kind {
                    if let TypeKind::Pointer(_) = right_type_kind
                        && left_type == right_type
                    {
                        Ok(boole)
                    } else if let TypeKind::PointerOfT = right_type_kind {
                        Ok(boole)
                    } else {
                        Err(AnalysisError::TypeMismatch {
                            got: right_type,
                            at: right.span,
                            expected: Expected::One(left_type),
                            reason: op.span,
                        })
                    }
                } else if let (TypeKind::PointerOfT, TypeKind::Pointer(_)) =
                    (&left_type_kind, &right_type_kind)
                {
                    Ok(boole)
                } else if let Some(target) = self.infer_set_type_operands::<true>(
                    left,
                    right,
                    &left_type_kind,
                    &right_type_kind,
                    op.span,
                )? {
                    Ok(target)
                } else {
                    Err(AnalysisError::TypeMismatch {
                        got: left_type,
                        at: left.span,
                        expected: Expected::Abstract(
                            "simple-type, Pointer of T, string-type or Set of T",
                        ),
                        reason: op.span,
                    })
                }
            }
            e::BinOp::LT | e::BinOp::GT => {
                if self.infer_binop_cmp_common(
                    left,
                    right,
                    &left_type_kind,
                    &right_type_kind,
                    op.span,
                )? {
                    Ok(boole)
                } else {
                    Err(AnalysisError::TypeMismatch {
                        got: left_type,
                        at: left.span,
                        expected: Expected::Abstract("simple-type or string-type"),
                        reason: op.span,
                    })
                }
            }
            e::BinOp::LEq | e::BinOp::GEq => {
                if self.infer_binop_cmp_common(
                    left,
                    right,
                    &left_type_kind,
                    &right_type_kind,
                    op.span,
                )? {
                    Ok(boole)
                } else if let Some(target) = self.infer_set_type_operands::<true>(
                    left,
                    right,
                    &left_type_kind,
                    &right_type_kind,
                    op.span,
                )? {
                    Ok(target)
                } else {
                    Err(AnalysisError::TypeMismatch {
                        got: left_type,
                        at: left.span,
                        expected: Expected::Abstract("simple-type, string-type or Set of T"),
                        reason: op.span,
                    })
                }
            }
            e::BinOp::In => {
                let left_type = self.context.to_ordinal(left_type, left.span, op.span)?;
                if let TypeKind::Set { member_ty: elem, .. } = right_type_kind {
                    if left_type == elem {
                        Ok(boole)
                    } else {
                        Err(AnalysisError::MemberTypeMismatch {
                            member_type: left_type,
                            at: left.span,
                            set_type: right_type,
                        })
                    }
                } else {
                    Err(AnalysisError::TypeMismatch {
                        got: right_type.into(),
                        at: right.span,
                        expected: Expected::One(self.context.set_of_t),
                        reason: op.span,
                    })
                }
            }
        }?;

        let bin_op = match (left_type_kind, op.node, right_type_kind) {
            (TypeKind::Integer, e::BinOp::Add | e::BinOp::Sub | e::BinOp::Mul, TypeKind::Real) => {
                Expr::BinOp {
                    op: self.convert_bop(op.node, out_ty),
                    left: self.int_to_real(new_left),
                    right: new_right,
                }
            }
            (TypeKind::Real, e::BinOp::Add | e::BinOp::Sub | e::BinOp::Mul, TypeKind::Integer) => {
                Expr::BinOp {
                    op: self.convert_bop(op.node, out_ty),
                    left: new_left,
                    right: self.int_to_real(new_right),
                }
            }
            // Intentionally permissive
            (
                TypeKind::Integer | TypeKind::Real,
                e::BinOp::Add | e::BinOp::Sub | e::BinOp::Mul,
                _,
            ) => Expr::BinOp {
                op: self.convert_bop(op.node, out_ty),
                left: new_left,
                right: new_right,
            },
            (
                TypeKind::Set { .. },
                e::BinOp::Add | e::BinOp::Sub | e::BinOp::Mul,
                TypeKind::Set { .. },
            ) => Expr::BinOp {
                op: self.convert_bop(op.node, out_ty),
                left: new_left,
                right: new_right,
            },
            (
                TypeKind::Integer,
                e::BinOp::Eq
                | e::BinOp::NEq
                | e::BinOp::LT
                | e::BinOp::GT
                | e::BinOp::LEq
                | e::BinOp::GEq,
                TypeKind::Real,
            ) => Expr::BinOp {
                op: self.convert_bop(op.node, real),
                left: self.int_to_real(new_left),
                right: new_right,
            },
            (
                TypeKind::Real,
                e::BinOp::Eq
                | e::BinOp::NEq
                | e::BinOp::LT
                | e::BinOp::GT
                | e::BinOp::LEq
                | e::BinOp::GEq,
                TypeKind::Integer,
            ) => Expr::BinOp {
                op: self.convert_bop(op.node, real),
                left: new_left,
                right: self.int_to_real(new_right),
            },
            // Intentionally permissive
            (
                TypeKind::Integer
                | TypeKind::Real
                | TypeKind::Boolean
                | TypeKind::Char
                | TypeKind::Enumerated { .. }
                | TypeKind::Subrange { .. }
                | TypeKind::Pointer(_)
                | TypeKind::Array { .. }
                | TypeKind::Set { .. },
                e::BinOp::Eq
                | e::BinOp::NEq
                | e::BinOp::LT
                | e::BinOp::GT
                | e::BinOp::LEq
                | e::BinOp::GEq,
                _,
            ) => todo!(),
            (_, e::BinOp::In, TypeKind::Set { .. }) => {
                Expr::BinOp { op: BinOp::SetIn, left: new_left, right: new_right }
            }
            _ => unreachable!(),
        };
        Ok(Typed { node: bin_op, r#type: out_ty })
    }

    fn int_to_real(&self, int: Box<TypedExpr>) -> Box<TypedExpr> {
        Box::new(Typed { node: Expr::IntToReal(int), r#type: self.context.real })
    }

    fn convert_bop(&self, bop: e::BinOp, hint: TypeId) -> BinOp {
        match (bop, self.context.get_type(hint)) {
            (e::BinOp::Add, TypeKind::Integer) => BinOp::IAdd,
            (e::BinOp::Add, TypeKind::Real) => BinOp::RAdd,
            (e::BinOp::Add, TypeKind::Set { .. }) => BinOp::Union,

            (e::BinOp::Sub, TypeKind::Integer) => BinOp::ISub,
            (e::BinOp::Sub, TypeKind::Real) => BinOp::RSub,
            (e::BinOp::Sub, TypeKind::Set { .. }) => BinOp::Diff,

            (e::BinOp::Mul, TypeKind::Integer) => BinOp::IMul,
            (e::BinOp::Mul, TypeKind::Real) => BinOp::RMul,
            (e::BinOp::Mul, TypeKind::Set { .. }) => BinOp::Inter,

            (e::BinOp::Quot, _) => BinOp::RQuot,
            (e::BinOp::Div, _) => BinOp::IDiv,
            (e::BinOp::Mod, _) => BinOp::IMod,

            (e::BinOp::And, _) => BinOp::BAnd,
            (e::BinOp::Or, _) => BinOp::BOr,

            (e::BinOp::Eq, TypeKind::Integer) => BinOp::IEq,
            (e::BinOp::NEq, TypeKind::Integer) => BinOp::INEq,
            (e::BinOp::LT, TypeKind::Integer) => BinOp::ILT,
            (e::BinOp::GT, TypeKind::Integer) => BinOp::IGT,
            (e::BinOp::LEq, TypeKind::Integer) => BinOp::ILEq,
            (e::BinOp::GEq, TypeKind::Integer) => BinOp::IGEq,

            (e::BinOp::Eq, TypeKind::Real) => BinOp::REq,
            (e::BinOp::NEq, TypeKind::Real) => BinOp::RNEq,
            (e::BinOp::LT, TypeKind::Real) => BinOp::RLT,
            (e::BinOp::GT, TypeKind::Real) => BinOp::RGT,
            (e::BinOp::LEq, TypeKind::Real) => BinOp::RLEq,
            (e::BinOp::GEq, TypeKind::Real) => BinOp::RGEq,

            (e::BinOp::Eq, TypeKind::Boolean) => BinOp::BEq,
            (e::BinOp::NEq, TypeKind::Boolean) => BinOp::BNEq,
            (e::BinOp::LT, TypeKind::Boolean) => BinOp::BLT,
            (e::BinOp::GT, TypeKind::Boolean) => BinOp::BGT,
            (e::BinOp::LEq, TypeKind::Boolean) => BinOp::BLEq,
            (e::BinOp::GEq, TypeKind::Boolean) => BinOp::BGEq,

            (e::BinOp::Eq, TypeKind::Char) => BinOp::CEq,
            (e::BinOp::NEq, TypeKind::Char) => BinOp::CNEq,
            (e::BinOp::LT, TypeKind::Char) => BinOp::CLT,
            (e::BinOp::GT, TypeKind::Char) => BinOp::CGT,
            (e::BinOp::LEq, TypeKind::Char) => BinOp::CLEq,
            (e::BinOp::GEq, TypeKind::Char) => BinOp::CGEq,

            (e::BinOp::Eq, TypeKind::Pointer(_)) => BinOp::PtrEq,
            (e::BinOp::NEq, TypeKind::Pointer(_)) => BinOp::PtrNEq,

            (e::BinOp::Eq, TypeKind::Enumerated { .. }) => BinOp::EEq,
            (e::BinOp::NEq, TypeKind::Enumerated { .. }) => BinOp::ENEq,
            (e::BinOp::LT, TypeKind::Enumerated { .. }) => BinOp::ELT,
            (e::BinOp::GT, TypeKind::Enumerated { .. }) => BinOp::EGT,
            (e::BinOp::LEq, TypeKind::Enumerated { .. }) => BinOp::ELEq,
            (e::BinOp::GEq, TypeKind::Enumerated { .. }) => BinOp::EGEq,

            (e::BinOp::Eq, TypeKind::Subrange { .. }) => BinOp::SubEq,
            (e::BinOp::NEq, TypeKind::Subrange { .. }) => BinOp::SubNEq,
            (e::BinOp::LT, TypeKind::Subrange { .. }) => BinOp::SubLT,
            (e::BinOp::GT, TypeKind::Subrange { .. }) => BinOp::SubGT,
            (e::BinOp::LEq, TypeKind::Subrange { .. }) => BinOp::SubLEq,
            (e::BinOp::GEq, TypeKind::Subrange { .. }) => BinOp::SubGEq,

            // Assuming previous code did its job and only allowed string-types through
            (e::BinOp::Eq, TypeKind::Array { .. }) => BinOp::StrEq,
            (e::BinOp::NEq, TypeKind::Array { .. }) => BinOp::StrNEq,
            (e::BinOp::LT, TypeKind::Array { .. }) => BinOp::StrLT,
            (e::BinOp::GT, TypeKind::Array { .. }) => BinOp::StrGT,
            (e::BinOp::LEq, TypeKind::Array { .. }) => BinOp::StrLEq,
            (e::BinOp::GEq, TypeKind::Array { .. }) => BinOp::StrGEq,

            (e::BinOp::Eq, TypeKind::Set { .. }) => BinOp::SetEq,
            (e::BinOp::NEq, TypeKind::Set { .. }) => BinOp::SetNEq,
            (e::BinOp::LEq, TypeKind::Set { .. }) => BinOp::SetLEq,
            (e::BinOp::GEq, TypeKind::Set { .. }) => BinOp::SetGEq,
            (e::BinOp::In, _) => BinOp::SetIn,

            _ => unreachable!(),
        }
    }

    /// Infer/Check simple-type or string-type operands for comparison operations.
    fn infer_binop_cmp_common(
        &mut self,
        left: Spanned<TypeId>,
        right: Spanned<TypeId>,
        left_type_kind: &TypeKind,
        right_type_kind: &TypeKind,
        op_span: Span,
    ) -> AnalysisResult<bool> {
        let reqd_simple = [
            self.context.integer.into(),
            self.context.real,
            self.context.boolean.into(),
            self.context.char.into(),
        ];

        if reqd_simple.contains(&left.node) {
            if reqd_simple.contains(&right.node) {
                match (left_type_kind, right_type_kind) {
                    (TypeKind::Integer, TypeKind::Real) | (TypeKind::Real, TypeKind::Integer) => {
                        Ok(true)
                    }
                    _ => {
                        self.context.check_compat(left.node, right.node, op_span)?;
                        Ok(true)
                    }
                }
            } else {
                Err(AnalysisError::TypeMismatch {
                    got: right.node,
                    at: right.span,
                    expected: Expected::Abstract("compatible simple-type"),
                    reason: op_span,
                })
            }
        } else if let TypeKind::Subrange { .. }
        | TypeKind::Enumerated { .. }
        | TypeKind::Array { .. } = left_type_kind
        {
            self.context.check_compat(left.node, right.node, op_span)?;
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn infer_binop_simple<const N: usize>(
        &mut self,
        types: &[TypeId; N],
        left: Spanned<TypeId>,
        right: Spanned<TypeId>,
        result_type: TypeId,
        op_span: Span,
    ) -> AnalysisResult<TypeId> {
        let expected =
            if const { N == 1 } { Expected::One(types[0]) } else { Expected::Many(types.to_vec()) };

        if types.contains(&left.node) {
            if types.contains(&right.node) {
                Ok(result_type)
            } else {
                Err(AnalysisError::TypeMismatch {
                    got: right.node,
                    at: right.span,
                    expected,
                    reason: op_span,
                })
            }
        } else {
            Err(AnalysisError::TypeMismatch {
                got: left.node,
                at: left.span,
                expected,
                reason: op_span,
            })
        }
    }

    /// # Constant Generics
    ///
    /// * `CMP` - Whether the surrounding binary operation is a comparison, so that an appropriate target type can be
    ///   returned.
    fn infer_set_type_operands<const CMP: bool>(
        &mut self,
        left: Spanned<TypeId>,
        right: Spanned<TypeId>,
        left_type_kind: &TypeKind,
        right_type_kind: &TypeKind,
        op_span: Span,
    ) -> AnalysisResult<Option<TypeId>> {
        if let &TypeKind::Set { .. } = left_type_kind {
            let result = if const { CMP } { self.context.boolean.into() } else { left.node };
            if let &TypeKind::Set { .. } = right_type_kind
                && left.node == right.node
            {
                Ok(Some(result))
            } else if right.node == self.context.set_of_t {
                Ok(Some(result))
            } else {
                Err(AnalysisError::TypeMismatch {
                    got: right.node,
                    at: right.span,
                    expected: Expected::One(left.node),
                    reason: op_span,
                })
            }
        } else if let (TypeKind::SetOfT, TypeKind::Set { .. }) = (left_type_kind, right_type_kind) {
            Ok(Some(right.node))
        } else {
            Ok(None)
        }
    }

    fn infer_set(
        &mut self,
        elements: &[Spanned<e::SetMember>],
        expr_span: Span,
        expected: Option<(TypeId, Span)>,
    ) -> AnalysisResult<TypedExpr> {
        if let Some((first, rest)) = elements.split_first() {
            // Packedness is inferred from context, and if that information is not available it defaults to unpacked
            let (packed, member_type) =
                if let Some((TypeKind::Set { packed, member_ty: elem }, reason)) =
                    expected.map(|(ty, reason)| (self.context.get_type(ty), reason))
                {
                    (*packed, Some((*elem, reason)))
                } else {
                    (false, None)
                };
            let e::SetMember { start, end } = &first.node;
            let new_start = match member_type {
                Some((member_type, reason)) => {
                    let expected = member_type.into();
                    let new_start = self.infer_expr(start.as_ref(), None)?;
                    self.context.check_assign_compat(expected, new_start.r#type, start.span)?;
                    new_start
                }
                None => self.infer_expr(start.as_ref(), None)?,
            };
            let member_type = new_start.r#type;

            let first_member = match end.as_ref() {
                Some(end) => {
                    let new_end = self.infer_expr(end.as_ref(), None)?;
                    self.context.check_assign_compat(member_type, new_end.r#type, end.span)?;
                    SetMember::Range { start: new_start, end: new_end }
                }
                None => SetMember::One(new_start),
            };

            let members = iter::once(Ok(first_member))
                .chain(rest.iter().map(|elem| {
                    let e::SetMember { start, end } = &elem.node;
                    let new_start = self.infer_expr(start.as_ref(), None)?;
                    self.context.check_assign_compat(member_type, new_start.r#type, start.span)?;

                    if let Some(end) = end {
                        let new_end = self.infer_expr(end.as_ref(), None)?;
                        self.context.check_assign_compat(member_type, new_end.r#type, end.span)?;
                        Ok(SetMember::Range { start: new_start, end: new_end })
                    } else {
                        Ok(SetMember::One(new_start))
                    }
                }))
                .collect::<Result<_, _>>()?;

            let ty = TypeKind::Set {
                packed,
                member_ty: self.context.to_ordinal(member_type, first.span, expr_span)?,
            };
            Ok(Typed { node: Expr::Set(members), r#type: self.context.fresh(ty) })
        }
        // Empty set literal
        else {
            if let Some((expected, reason)) = expected {
                if let TypeKind::Set { .. } = self.context.get_type(expected) {
                    Ok(Typed { node: Expr::Set(Vec::new()), r#type: expected })
                } else {
                    Err(AnalysisError::TypeMismatch {
                        got: self.context.set_of_t,
                        at: expr_span,
                        expected: Expected::One(expected),
                        reason,
                    })
                }
            } else {
                Ok(Typed { node: Expr::Set(Vec::new()), r#type: self.context.set_of_t })
            }
        }
    }

    fn infer_fun_call(
        &mut self,
        name: Ident,
        args: Spanned<&[e::SpanExpr]>,
    ) -> AnalysisResult<TypedExpr> {
        let def = self.context.lookup(name.node, name.span)?;
        let DefPoint::UserDef {
            kind:
                DefKind::Func(ctx::FuncSig { params, result, .. })
                | DefKind::Param { kind: ParamKind::Func { params, result }, .. },
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

        let args = self.check_args(&params, args, func_sig_span)?;
        Ok(Typed {
            node: Expr::FuncCall { name: name.node, def: self.context.as_func(def), args },
            r#type: result,
        })
    }

    fn check_args(
        &mut self,
        params: &[ParamSection],
        args: Spanned<&[e::SpanExpr]>,
        call_span: Span,
    ) -> AnalysisResult<Vec<(ParamId, Arg)>> {
        let params = ParamIter::from(params)
            .map(|param_id| {
                let param_def = param_id.into();
                if let DefPoint::UserDef {
                    kind: DefKind::Param { kind: param, .. }, span, ..
                } = self.context.get_def(param_def)
                {
                    (param_id, param.clone(), *span)
                } else {
                    unreachable!()
                }
            })
            .collect::<Vec<_>>();

        params
            .into_iter()
            .zip(args.node.iter())
            .map(|((param_id, param, param_span), arg)| {
                let def_type = match arg.node {
                    e::Expr::Var(e::Var::Plain(name)) => {
                        let def = self.context.lookup(name, arg.span)?;
                        Some((def, self.context.get_def(def)))
                    }
                    _ => None,
                };
                match param {
                    ParamKind::Value(ParamType::TypeIdent(param)) => {
                        let new_arg = self.infer_expr(arg.as_ref(), Some((param, param_span)))?;
                        self.context.check_assign_compat(param, new_arg.r#type, arg.span)?;
                        Ok((param_id, Arg::Value(new_arg)))
                    }
                    ParamKind::Var(ParamType::TypeIdent(param)) => {
                        let new_arg = match &arg.node {
                            e::Expr::Var(var) => {
                                self.infer_assign_var(Spanned { span: arg.span, node: var })
                            }
                            _ => Err(AnalysisError::ExpectedVar { span: arg.span }),
                        }?;
                        // variable-parameters require complete type equality with their corresponding formal-variable-parameters
                        if new_arg.r#type != param {
                            return Err(AnalysisError::TypeMismatch {
                                got: new_arg.r#type,
                                at: arg.span,
                                expected: Expected::One(param),
                                reason: param_span,
                            });
                        }
                        Ok((param_id, Arg::Var(new_arg)))
                    }
                    ParamKind::Proc(formal_params) => match def_type {
                        Some((
                            actual,
                            DefPoint::UserDef {
                                name,
                                kind: DefKind::Proc(ctx::ProcSig { params: actual_params, .. }),
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
                            Ok((param_id, Arg::Proc(*name, self.context.as_proc(actual))))
                        }
                        Some((got, _)) => Err(AnalysisError::MismatchDef {
                            got,
                            expected: "user-defined procedure",
                            origin: arg.span,
                        }),
                        None => Err(AnalysisError::ExpectedIdentifier {
                            kind: "user-defined procedure",
                            at: arg.span,
                        }),
                    },
                    ParamKind::Func { params: formal_params, result: formal_result } => {
                        match def_type {
                            Some((
                                actual,
                                DefPoint::UserDef {
                                    name,
                                    kind:
                                        DefKind::Func(ctx::FuncSig {
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
                                        expected: Expected::One(formal_result),
                                        reason: call_span,
                                    });
                                }
                                Ok((param_id, Arg::Func(*name, self.context.as_func(actual))))
                            }
                            Some((got, _)) => Err(AnalysisError::MismatchDef {
                                got,
                                expected: "user-defined function",
                                origin: arg.span,
                            }),
                            None => Err(AnalysisError::ExpectedIdentifier {
                                kind: "user-defined function",
                                at: arg.span,
                            }),
                        }
                    }
                    ParamKind::Value(ParamType::ArraySchema(_))
                    | ParamKind::Var(ParamType::ArraySchema(_)) => unimplemented!(),
                }
            })
            .collect()
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
                (ParamSection::Many(ps1), ParamSection::Many(ps2)) if ps1.len() == ps2.len() => {
                    for (p1, p2) in ps1.iter().cloned().zip(ps2.iter().cloned()) {
                        self.check_params_congruity(formal, actual, p1, p2, origin)?;
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
        let DefPoint::UserDef { kind: DefKind::Param { kind: formal_kind, .. }, .. } =
            self.context.get_def((formal_param).into())
        else {
            unreachable!()
        };
        let DefPoint::UserDef {
            kind: DefKind::Param { kind: actual_kind, .. },
            span: actual_span,
            ..
        } = self.context.get_def((actual_param).into())
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
                        expected: Expected::One(*r1),
                        reason: origin,
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

    fn infer_assign_var(&mut self, var: Spanned<&e::Var>) -> AnalysisResult<Typed<Place>> {
        match var.node {
            e::Var::Plain(spur) => {
                let def = self.context.lookup(*spur, var.span)?;
                match self.context.get_def(def) {
                    DefPoint::BuiltinVar(bv) => match bv {
                        BuiltinVar::Input => {
                            Ok(Typed { node: Place::Builtin(*bv), r#type: self.context.text })
                        }
                        BuiltinVar::Output => {
                            Ok(Typed { node: Place::Builtin(*bv), r#type: self.context.text })
                        }
                    },
                    &DefPoint::UserDef { name, ref kind, .. } => match kind {
                        DefKind::ProgramParam => Ok(Typed {
                            node: Place::Plain { name, def },
                            r#type: self.context.text,
                        }),
                        &DefKind::Var(r#type) => {
                            Ok(Typed { node: Place::Plain { name, def }, r#type })
                        }
                        &DefKind::WithFieldAlias { record_var, field, r#type } => Ok(Typed {
                            node: Place::WithFieldAlias { name, record_var, field },
                            r#type,
                        }),
                        &DefKind::Param {
                            kind: ParamKind::Value(ParamType::TypeIdent(r#type)),
                            ..
                        } => Ok(Typed {
                            node: Place::ValueParam { name, param: self.context.as_param(def) },
                            r#type,
                        }),
                        &DefKind::Param {
                            kind: ParamKind::Var(ParamType::TypeIdent(r#type)),
                            ..
                        } => Ok(Typed {
                            node: Place::VarParam { name, param: self.context.as_param(def) },
                            r#type,
                        }),
                        _ => todo!("throw error about definition being un-assignable"),
                    },
                    _ => todo!("throw error about definition being un-assignable"),
                }
            }
            e::Var::Ref(inner_var) => todo!(),
            e::Var::Indexed(array_var, indices) => todo!(),
            e::Var::FieldAccess { record, dot_span, field } => todo!(),
        }
    }

    fn infer_expr_var(&mut self, var: Spanned<&e::Var>) -> AnalysisResult<TypedExpr> {
        match var.node {
            &e::Var::Plain(name) => {
                let def = self.context.lookup(name, var.span)?;
                match self.context.get_def(def) {
                    DefPoint::BuiltinVar(bv) => match bv {
                        BuiltinVar::Input => Ok(Typed {
                            node: Expr::Place(Place::Builtin(*bv)),
                            r#type: self.context.text,
                        }),
                        BuiltinVar::Output => Ok(Typed {
                            node: Expr::Place(Place::Builtin(*bv)),
                            r#type: self.context.text,
                        }),
                    },
                    DefPoint::UserDef { kind, .. } => match kind {
                        &DefKind::EnumMember { r#type, ordinal } => Ok(Typed {
                            node: Expr::Enum(ordinal, self.context.as_enum_member(def)),
                            r#type,
                        }),
                        &DefKind::Const { r#type, value } => {
                            Ok(Typed { node: self.resolve_const(value, def), r#type })
                        }
                        &DefKind::Var(r#type) => {
                            Ok(Typed { node: Expr::Place(Place::Plain { name, def }), r#type })
                        }
                        &DefKind::Param {
                            kind: ParamKind::Value(ParamType::TypeIdent(r#type)),
                            ..
                        } => Ok(Typed {
                            node: Expr::Place(Place::ValueParam {
                                name,
                                param: self.context.as_param(def),
                            }),
                            r#type,
                        }),
                        &DefKind::Param {
                            kind: ParamKind::Var(ParamType::TypeIdent(r#type)),
                            ..
                        } => Ok(Typed {
                            node: Expr::Place(Place::VarParam {
                                name,
                                param: self.context.as_param(def),
                            }),
                            r#type,
                        }),
                        &DefKind::WithFieldAlias { record_var, field, r#type } => Ok(Typed {
                            node: Expr::Place(Place::WithFieldAlias { name, record_var, field }),
                            r#type,
                        }),
                        _ => Err(AnalysisError::MismatchDef {
                            got: def,
                            expected: "variable",
                            origin: var.span,
                        }),
                    },

                    _ => Err(AnalysisError::MismatchDef {
                        got: def,
                        expected: "variable",
                        origin: var.span,
                    }),
                }
            }
            e::Var::Ref(inner_var) => {
                let Typed { node: new_inner_var, r#type } =
                    self.infer_assign_var(inner_var.as_ref().as_ref())?;
                let &TypeKind::Pointer(domain_type) = self.context.get_type(r#type) else {
                    return Err(AnalysisError::TypeMismatch {
                        got: r#type,
                        at: inner_var.span,
                        expected: Expected::Abstract("pointer type"),
                        reason: var.span,
                    });
                };

                Ok(Typed {
                    node: Expr::Place(Place::Deref {
                        referent: Box::new(new_inner_var),
                        domain_type,
                    }),
                    r#type,
                })
            }
            e::Var::Indexed(array_var, subscripts) => {
                let Typed { node: new_array_var, r#type } =
                    self.infer_assign_var(array_var.as_ref().as_ref())?.map(Box::new);
                let TypeKind::Array { indices, elem, .. } = self.context.get_type(r#type).clone()
                else {
                    return Err(AnalysisError::TypeMismatch {
                        got: r#type,
                        at: array_var.span,
                        expected: Expected::Abstract("array type"),
                        reason: subscripts.span,
                    });
                };

                let indices = indices
                    .iter()
                    .zip(subscripts.node.iter())
                    .map(|(index_type, subscript)| {
                        let new_subscript = self.infer_expr(subscript.as_ref(), None)?;
                        self.context.check_assign_compat(
                            (*index_type).into(),
                            new_subscript.r#type,
                            subscript.span,
                        )?;
                        Ok(new_subscript)
                    })
                    .collect::<Result<_, _>>()?;

                Ok(Typed {
                    node: Expr::Place(Place::Indexed { array_var: new_array_var, indices }),
                    r#type,
                })
            }
            e::Var::FieldAccess { record, dot_span, field } => {
                let Typed { node: new_record, r#type } =
                    self.infer_assign_var(record.as_ref().as_ref())?.map(Box::new);
                let TypeKind::Record { fixed, variant, .. } = self.context.get_type(r#type) else {
                    return Err(AnalysisError::TypeMismatch {
                        got: r#type,
                        at: record.span,
                        expected: Expected::Abstract("record type"),
                        reason: var.span,
                    });
                };

                let &field_id = Self::get_fields(fixed, variant.as_ref())
                    .find(|&&f| self.context.get_def_name(f.into()) == field.node)
                    .ok_or(AnalysisError::MissingField {
                        record: r#type,
                        name: field.node,
                        at: *dot_span + field.span,
                    })?;

                let &DefPoint::UserDef { kind: DefKind::Field { r#type: field_type, .. }, .. } =
                    self.context.get_def(field_id.into())
                else {
                    unreachable!()
                };

                Ok(Typed {
                    node: Expr::Place(Place::Field {
                        record_var: new_record,
                        field_name: field.node,
                        record: r#type,
                        field: field_id,
                    }),
                    r#type: field_type,
                })
            }
        }
    }

    fn resolve_const(&self, r#const: Constant, def: DefId) -> Expr {
        match r#const {
            Constant::Int(n) => Expr::IntLit(n),
            Constant::Real(n) => Expr::RealLit(n),
            Constant::Bool(b) => {
                if b {
                    Expr::True
                } else {
                    Expr::False
                }
            }
            Constant::Char(c) => Expr::CharLit(c),
            Constant::Str(s) => Expr::StrLit(s),
            Constant::Enum { member_idx, .. } => {
                Expr::Enum(member_idx, self.context.as_enum_member(def))
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
