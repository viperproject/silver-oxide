use core::ops::{Deref, DerefMut};

use crate::{
    parse::{self, AssignRhs, AstWalkable, AstWalker, ConstKind, ExpKind, StarOrNames, StmtBlock, UnOp}, vmir::{middle::*, ty::Ty, DefId, Symbol}, HashMap, TiVec
};

use super::{cfg::Cfg, ArgRef, BasicBlockKind, LoopHead, TranslationCtxt};

struct BodyTranslator<'a, 'tcx> {
    inner: TranslationCtxt<'a, 'tcx>,
    locals: TiVec<Local, Ty<'tcx>>,
}

impl<'a, 'tcx> Deref for BodyTranslator<'a, 'tcx> {
    type Target = TranslationCtxt<'a, 'tcx>;
    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<'a, 'tcx> DerefMut for BodyTranslator<'a, 'tcx> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl<'tcx> TranslationCtxt<'_, 'tcx> {
    pub(crate) fn translate_body(self, body: &parse::StmtBlock, name: Symbol<'tcx>, params: impl Iterator<Item = (Local, Ty<'tcx>)>) -> Body<'tcx> {
        let cfg = Cfg::new(self.tcx, name, self.goto_labels.iter().copied(), body);
        let mut locals = TiVec::new();
        for (local, ty) in params {
            let l = locals.push_and_get_key(ty);
            assert_eq!(local, l);
        }
        BodyTranslator {
            inner: self,
            locals,
        }.translate_body(body, cfg)
    }
}

impl<'tcx> BodyTranslator<'_, 'tcx> {
    pub(crate) fn translate_body(mut self, body: &parse::StmtBlock, cfg: Cfg<'_, 'tcx>) -> Body<'tcx> {
        self.add_body(body);

        let mut result = Body::default();
        let mut branches = HashMap::<BasicBlock, Operand<'tcx>>::default();
        let mut loop_frames = HashMap::<LoopHead, (Local, ResourceExp<'tcx>)>::default();

        let mut curr = &mut Block::default();
        for (bb, data) in cfg.preorder() {
            if let Ok(pcs) = data.pcs.pcs() {
                if curr.stmts.is_empty() {
                    let prev = result.blocks.pop();
                    assert!(prev.is_none_or(|b| b.stmts.is_empty()));
                }
                let conds = BlockConds::new(pcs.map(|pcs| {
                    pcs.map(|(pcs, last)| {
                        pcs.iter()
                            .copied()
                            .chain([last])
                            .map(|pc| (branches[&pc.0], !pc.1))
                    })
                }));
                result.blocks.push(Block {
                    conds,
                    ..Default::default()
                });
                curr = result.blocks.last_mut().unwrap();
            }
            for frame in data.loop_exit() {
                let frame = loop_frames[frame].0;
                let kind = StatementKind::MergeHeap(self.curr_heap(), frame);
                curr.stmts.push(Statement { kind });
            }
            if let Some(lh) = data.as_loop_head() {
                let (_, inv) = data.kind.loop_head();
                let heap = self.curr_heap_nd();
                let inv = self.translate_resource(&inv.0.as_ref().expect("no loop invariant at label, todo"), heap);
                let kind =
                    StatementKind::Ghost(None, InhaleExhale::Exhale, self.curr_heap(), inv.clone());
                curr.stmts.push(Statement { kind });

                let tmp = self.new_local_lh(lh);
                let heap_nd = OperandKind::Local(self.curr_heap());
                let heap_nd = Operand {
                    ty: self.tcx.types.heap_,
                    kind: heap_nd,
                };
                let kind = StatementKind::Eval(Ok(tmp), Exp::new_use(heap_nd));
                curr.stmts.push(Statement { kind });
                loop_frames.insert(lh, (tmp, inv.clone()));

                let (_, tmp2) = curr.new_temporary(self.tcx.types.heap_);
                let kind = StatementKind::Eval(Ok(self.curr_heap()), Exp::new_use(tmp2));
                curr.stmts.push(Statement { kind });
                for &modified in &cfg.loop_data(lh).modifies {
                    let (local, ty) = self.params[&ArgRef::Ident(modified)];
                    let (_, tmp) = curr.new_temporary(ty);
                    let kind = StatementKind::Eval(Ok(local), Exp::new_use(tmp));
                    curr.stmts.push(Statement { kind });
                }

                let kind = StatementKind::Ghost(None, InhaleExhale::Inhale, self.curr_heap(), inv);
                curr.stmts.push(Statement { kind });
            }
            match data.kind {
                BasicBlockKind::Return => (),
                BasicBlockKind::Block(stmts, ..) => {
                    for stmt in stmts {
                        self.translate_stmt(stmt, curr);
                    }
                }
                BasicBlockKind::Branch(stmt, _) => {
                    let cond = match stmt {
                        parse::Statement::While(cond, ..) => cond,
                        parse::Statement::If(cond, ..) => cond,
                        _ => unreachable!(),
                    };
                    let ty = self.tcx.types.bool_;
                    let cond = self.translate_exp_local(cond, Some(ty), curr);
                    branches.insert(bb, cond);
                }
            }
            if let Some(lh) = data.back_edge_to() {
                let inv = loop_frames[&lh].1.clone();
                let kind = StatementKind::Ghost(None, InhaleExhale::Exhale, self.curr_heap(), inv);
                curr.stmts.push(Statement { kind });

                let false_ = OperandKind::Const(self.tcx.interner.mk_const(ConstKind::bool(false)));
                let false_ = ResourceExp::pure(Operand {
                    ty: self.tcx.types.bool_,
                    kind: false_,
                });
                let kind =
                    StatementKind::Ghost(None, InhaleExhale::Inhale, self.curr_heap(), false_);
                curr.stmts.push(Statement { kind });
            }
        }

        result.locals = self.locals;
        result
    }

    fn translate_stmt(&mut self, stmt: &parse::Statement, block: &mut Block<'tcx>) {
        use parse::Statement::*;
        let kind = match stmt {
            Goto(..) | While(..) | If(..) | Block(..) => unreachable!(),
            Label(..) | Var(..) => return,
            Assume(exp) => {
                let exp = self.translate_exp_inner(exp, Some(self.tcx.types.bool_), self.curr_heap_nd());
                StatementKind::Ghost(
                    None,
                    InhaleExhale::Inhale,
                    self.curr_heap(),
                    ResourceExp::pure_exp(exp),
                )
            }
            Assert(exp) => {
                let exp = self.translate_exp_inner(exp, Some(self.tcx.types.bool_), self.curr_heap_nd());
                StatementKind::Ghost(
                    None,
                    InhaleExhale::Exhale,
                    self.curr_heap(),
                    ResourceExp::pure_exp(exp),
                )
            }
            Inhale(exp) => {
                let heap = self.curr_heap_nd();
                let exp = self.translate_resource(exp, heap);
                StatementKind::Ghost(None, InhaleExhale::Inhale, self.curr_heap(), exp)
            }
            Exhale(exp) => {
                let heap = self.curr_heap_nd();
                let exp = self.translate_resource(exp, heap);
                StatementKind::Ghost(None, InhaleExhale::Exhale, self.curr_heap(), exp)
            }
            Refute(..) => todo!(),
            Fold(acc) | Unfold(acc) => {
                let ExpKind::FuncApp(pred, args) = &*acc.acc.loc else {
                    unreachable!();
                };
                let pred = self.tcx.resolve(pred).unwrap();
                let (m, tys) = self.tcx.get_fold_unfold(pred.unwrap());
                assert_eq!(tys.len(), args.len());

                let args = args.iter().zip(tys).map(|(arg, ty)| {
                    self.translate_exp_local(arg, Some(*ty), block)
                }).collect();
                let ty = self.tcx.types.real_;
                let perm = self.translate_exp_local(&acc.perm, Some(ty), block);
                let kind = match stmt {
                    Fold(_) => CallKind::Call,
                    Unfold(_) => CallKind::UnCall,
                    _ => unreachable!(),
                };
                StatementKind::GhostCall(kind, m, self.curr_heap(), perm, args)
            }
            Havoc(..) => todo!(),
            QuasiHavoc(..) => todo!(),
            QuasiHavocAll(..) => todo!(),
            // TODO:
            Package(..) => return,
            Apply(..) => todo!(),
            Assign(targets, rhs) => {
                self.translate_assign_rhs(targets, rhs, block);
                return;
            }
        };
        block.stmts.push(Statement { kind });
    }

    fn translate_assign_target(
        &mut self,
        tgt: &parse::Exp,
        block: &mut Block<'tcx>,
    ) -> (Ty<'tcx>, Result<Local, Operand<'tcx>>) {
        match &**tgt {
            ExpKind::Ident(idn) => {
                let (local, ty) = self.params[&ArgRef::Ident(self.tcx.interner.mk_symbol(idn))];
                (ty, Ok(local))
            }
            ExpKind::UnOp(UnOp::Deref, rcv) => {
                // TODO: constrain to any address?
                let rcv = self.translate_exp_local(rcv, None, block);
                (rcv.ty.deref(), Err(rcv))
            }
            _ => unreachable!(),
        }
    }

    fn translate_assign_rhs(
        &mut self,
        targets: &[parse::Exp],
        rhs: &AssignRhs,
        block: &mut Block<'tcx>,
    ) {
        match rhs {
            AssignRhs::Exp(exp) => {
                let (ty, tgt) = self.translate_assign_target(&targets[0], block);
                let value = self.translate_exp_inner(exp, Some(ty), self.curr_heap_nd());
                match tgt {
                    Ok(local) => {
                        block.stmts.push(Statement {
                            kind: StatementKind::Eval(Ok(local), value),
                        });
                    }
                    Err(rcv) => {
                        if let Some(nd) = value.as_operand() {
                            block.stmts.push(Statement {
                                kind: StatementKind::Assign(Ok(self.curr_heap()), rcv, nd),
                            });
                        } else {
                            let (tmp, nd) = block.new_temporary(ty);
                            block.stmts.push(Statement {
                                kind: StatementKind::Eval(Err(tmp), value),
                            });
                            block.stmts.push(Statement {
                                kind: StatementKind::Assign(Ok(self.curr_heap()), rcv, nd),
                            });
                        }
                    }
                }
            }
            AssignRhs::New(new) => {
                let (_, nd) = block.new_temporary(self.tcx.types.ref_);
                self.translate_inhale_new(nd, new, block);
                let (ty, tgt) = self.translate_assign_target(&targets[0], block);
                assert_eq!(ty, self.tcx.types.ref_);
                match tgt {
                    Ok(local) => {
                        // let kind = StatementKind::Havoc(local);
                        // block.stmts.push(Statement { kind });
                        let kind = StatementKind::Eval(Ok(local), Exp::new_use(nd));
                        block.stmts.push(Statement { kind });
                    }
                    Err(rcv) => {
                        let kind = StatementKind::Assign(Ok(self.curr_heap()), rcv, nd);
                        block.stmts.push(Statement { kind });
                    }
                };
            }
            AssignRhs::Call(ident, args) => {
                let callee = self.tcx.resolve(ident).unwrap();
                let (callee, params, rets) = self.tcx.get_method(callee.unwrap()).unwrap();

                let mut post_call = Vec::new();
                let targets = targets
                    .iter()
                    .zip(rets)
                    .map(|(tgt, ety)| {
                        let (ty, tgt) = self.translate_assign_target(tgt, block);
                        assert_eq!(ty, *ety);
                        tgt.map(Ok).unwrap_or_else(|nd| {
                            let (tmp, tmp_nd) = block.new_temporary(*ety);
                            post_call.push(Statement {
                                kind: StatementKind::Assign(Ok(self.curr_heap()), nd, tmp_nd),
                            });
                            Err(tmp)
                        })
                    })
                    .collect();

                let heap = Operand {
                    ty: self.tcx.types.heap_,
                    kind: OperandKind::Local(self.params[&ArgRef::Heap(None)].0),
                };
                let args = args
                    .iter()
                    .zip(params)
                    .map(|(arg, ty)| self.translate_exp_local(arg, Some(*ty), block))
                    .chain([heap])
                    .collect();
                block.stmts.push(Statement {
                    kind: StatementKind::Call(targets, callee, args),
                });
                block.stmts.extend(post_call);
            }
        }
    }

    fn translate_inhale_new(
        &mut self,
        tmp: Operand<'tcx>,
        new: &StarOrNames,
        block: &mut Block<'tcx>,
    ) {
        let fields: Vec<_> = match new {
            StarOrNames::Star => {
                self.tcx.get_all_fields().collect()
            }
            StarOrNames::Names(idents) => idents
                .iter()
                .map(|ident| self.tcx.get_address(self.tcx.resolve(ident).unwrap().unwrap()).unwrap().0)
                .collect(),
        };
        let res = self.translate_resource_for_new(tmp, fields);
        block.stmts.push(Statement {
            kind: StatementKind::Ghost(None, InhaleExhale::Inhale, self.curr_heap(), res),
        });
    }

    fn translate_exp_local(
        &mut self,
        exp: &parse::Exp,
        ty: Option<Ty<'tcx>>,
        block: &mut Block<'tcx>,
    ) -> Operand<'tcx> {
        let exp = self.translate_exp_inner(exp, ty, self.curr_heap_nd());
        let ty = exp.result_ty();
        exp.as_operand().unwrap_or_else(|| {
            let (tmp, nd) = block.new_temporary(ty);
            block.stmts.push(Statement {
                kind: StatementKind::Eval(Err(tmp), exp),
            });
            nd
        })
    }

    fn curr_heap(&self) -> Local {
        self.params[&ArgRef::Heap(None)].0
    }

    fn curr_heap_nd(&self) -> Option<OperandKind<'tcx>> {
        Some(OperandKind::Local(self.curr_heap()))
    }

    pub(super) fn add_body(&mut self, body: &StmtBlock) {
        self.new_local(
            ArgRef::Heap(None),
            self.tcx.types.heap_,
        );
        self.walk_block(body);
        for label in self.inner.used_labels.drain(..) {
            assert!(
                self.inner.defined_labels.contains(&label),
                "label `{label}` not defined"
            );
            Self::new_local_inner(&mut self.locals, &mut self.inner.params, ArgRef::Label(label), self.inner.tcx.types.heap_);
        }
        assert!(
            self.goto_labels.is_subset(&self.defined_labels),
            "goto label not defined"
        );
    }

    pub(super) fn new_local_lh(&mut self, lh: LoopHead) -> Local {
        self.new_local(ArgRef::LoopFrame(lh), self.tcx.types.heap_)
    }

    fn new_local(&mut self, ar: ArgRef<'tcx>, ty: Ty<'tcx>) -> Local {
        Self::new_local_inner(&mut self.locals, &mut self.inner.params, ar, ty)
    }

    fn new_local_inner(locals: &mut TiVec<Local, Ty<'tcx>>, params: &mut HashMap<ArgRef<'tcx>, (Local, Ty<'tcx>)>, ar: ArgRef<'tcx>, ty: Ty<'tcx>) -> Local {
        let id = locals.push_and_get_key(ty);
        let old = params.insert(ar, (id, ty));
        assert!(old.is_none(), "duplicate local in body `{ar:?}`");
        id
    }
}

impl<'a> AstWalker<'a> for BodyTranslator<'_, '_> {
    fn walk_statement(&mut self, ast: &'a parse::Statement) {
        match ast {
            parse::Statement::Var(new, _) => {
                for new in new {
                    let ty = self.tycx.translate(&new.ty);
                    let idn = self.tcx.interner.mk_symbol(&new.idn.0);
                    self.new_local(ArgRef::Ident(idn), ty);
                    // assert!(old.is_none(), "duplicate var in body `{}`", new.idn.0 .0);
                }
            }
            parse::Statement::Label(decl, _) => {
                let name = self.tcx.interner.mk_symbol(&decl.0);
                self.defined_labels
                    .insert(name);
            }
            parse::Statement::Goto(label) => {
                let name = self.tcx.interner.mk_symbol(label);
                self.goto_labels.insert(name);
            }
            _ => (),
        }
        ast.walk_children(self);
    }

    fn walk_exp_kind(&mut self, ast: &'a ExpKind) {
        if let ExpKind::Old(Some(label), ..) = ast {
            let name = self.tcx.interner.mk_symbol(label);
            self.used_labels.insert(name);
        }
        ast.walk_children(self);
    }
}

impl<'tcx> Block<'tcx> {
    fn new_temporary(&mut self, ty: Ty<'tcx>) -> (Temporary, Operand<'tcx>) {
        let tmp = self.temporaries.push_and_get_key(ty);
        let nd = Operand {
            ty,
            kind: OperandKind::Temporary(tmp),
        };
        (tmp, nd)
    }
}

impl<'tcx> BlockConds<'tcx> {
    pub(crate) fn new<I: Iterator<Item = (Operand<'tcx>, bool)>>(
        conds: Option<impl Iterator<Item = I>>,
    ) -> Self {
        let Some(conds) = conds else {
            return BlockConds(vec![Vec::new()]);
        };
        let mut cs = Vec::new();
        'o: for conds in conds {
            let mut c = Vec::new();
            for (cond, neg) in conds {
                if let Some(c) = cond.as_const() {
                    if c.as_bool().unwrap() == neg {
                        continue 'o;
                    }
                } else {
                    c.push((cond, neg));
                }
            }
            if c.is_empty() {
                return BlockConds(vec![Vec::new()]);
            }
            cs.push(c);
        }
        Self(cs)
    }
}
