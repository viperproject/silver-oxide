use crate::{parse::{self, AssignRhs, ConstKind, ExpKind, StarOrNames, UnOp}, program::{body::{Block, BlockConds, Body, FoldUnfold, InhaleExhale, Operand, Statement, StatementKind}, exp::{Exp, ExpOperand}, resource::ResourceExp, ArgRef, BasicBlock, DefId, Local, MemberKind, Ty, TyKind}, HashMap};

use super::{cfg::Cfg, BasicBlockKind, LoopHead, TranslationCtxt};

impl<'tcx> TranslationCtxt<'_, 'tcx> {
    pub(crate) fn translate_body(mut self, body: &parse::StmtBlock) -> Body<'tcx> {
        self.add_body(body);
        let cfg = Cfg::new(&self.tcx, self.goto_labels.iter().copied(), body);

        let name = self.tcx.item_name(self.id).unwrap();
        cfg.dump_dot(&format!("log/cfg/{name}.dot"));

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
                let mut block = Block::default();
                block.conds = BlockConds::new(pcs.map(|pcs| {
                    pcs.map(|(pcs, last)| {
                        pcs.iter().copied().chain([last]).map(|pc| {
                            (branches[&pc.0], !pc.1)
                        })
                    })
                }));
                result.blocks.push(block);
                curr = result.blocks.last_mut().unwrap();
            }
            for frame in data.loop_exit() {
                let frame = loop_frames[frame].0;
                let kind = StatementKind::MergeHeap(self.curr_heap(), frame);
                curr.stmts.push(Statement { kind });
            }
            if let Some(lh) = data.as_loop_head() {
                let (_, inv) = data.kind.loop_head();
                let inv = self.translate_resource(&inv.0, self.curr_heap_nd());
                let kind = StatementKind::Ghost(None, InhaleExhale::Exhale, self.curr_heap(), inv.clone());
                curr.stmts.push(Statement { kind });

                let tmp = self.locals.push_and_get_key(self.tcx.types.heap_);
                let kind = StatementKind::Eval(tmp, Exp::new_use(ExpOperand::Local(self.curr_heap()), self.tcx.types.heap_));
                curr.stmts.push(Statement { kind });
                loop_frames.insert(lh, (tmp, inv.clone()));

                let kind = StatementKind::Havoc(self.curr_heap());
                curr.stmts.push(Statement { kind });
                for &modified in &cfg.loop_data(lh).modifies {
                    let local = self.params[&ArgRef::Ident(modified)];
                    let kind = StatementKind::Havoc(local);
                    curr.stmts.push(Statement { kind });
                }

                let kind = StatementKind::Ghost(None, InhaleExhale::Inhale, self.curr_heap(), inv);
                curr.stmts.push(Statement { kind });
            }
            match data.kind {
                BasicBlockKind::Return => (),
                BasicBlockKind::Block(stmts, ..) => for stmt in stmts {
                    self.translate_stmt(stmt, curr);
                }
                BasicBlockKind::Branch(stmt, _) => {
                    let cond = match stmt {
                        parse::Statement::While(cond, ..) => cond,
                        parse::Statement::If(cond, ..) => cond,
                        _ => unreachable!(),
                    };
                    let (cond, _) = self.translate_exp_local(cond, self.tcx.types.bool_, curr);
                    branches.insert(bb, cond);
                }
            }
            if let Some(lh) = data.back_edge_to() {
                let inv = loop_frames[&lh].1.clone();
                let kind = StatementKind::Ghost(None, InhaleExhale::Exhale, self.curr_heap(), inv);
                curr.stmts.push(Statement { kind });

                let false_ = ExpOperand::Const(self.tcx.interner.mk_const(ConstKind::bool(false)));
                let false_ = ResourceExp::pure(Exp::new_use(false_, self.tcx.types.bool_));
                let kind = StatementKind::Ghost(None, InhaleExhale::Inhale, self.curr_heap(), false_);
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
                let exp = self.translate_exp_inner(exp, self.tcx.types.bool_, self.curr_heap_nd());
                StatementKind::Ghost(None, InhaleExhale::Inhale, self.curr_heap(), ResourceExp::pure(exp))
            }
            Assert(exp) => {
                let exp = self.translate_exp_inner(exp, self.tcx.types.bool_, self.curr_heap_nd());
                StatementKind::Ghost(None, InhaleExhale::Exhale, self.curr_heap(), ResourceExp::pure(exp))
            }
            Inhale(exp) => {
                let exp = self.translate_resource(exp, self.curr_heap_nd());
                StatementKind::Ghost(None, InhaleExhale::Inhale, self.curr_heap(), exp)
            }
            Exhale(exp) => {
                let exp = self.translate_resource(exp, self.curr_heap_nd());
                StatementKind::Ghost(None, InhaleExhale::Exhale, self.curr_heap(), exp)
            }
            Refute(..) => todo!(),
            Fold(acc) | Unfold(acc) => {
                let ty = self.any_resource_id();
                let (resource, _) = self.translate_exp_local(&acc.acc.loc, ty, block);
                let (perm, _) = self.translate_exp_local(&acc.perm, self.tcx.types.real_, block);
                let fu = match stmt {
                    Fold(_) => FoldUnfold::Fold,
                    Unfold(_) => FoldUnfold::Unfold,
                    _ => unreachable!(),
                };
                StatementKind::Predicate(fu, self.curr_heap(), resource, perm)
            }
            Havoc(..) => todo!(),
            QuasiHavoc(..) => todo!(),
            QuasiHavocAll(..) => todo!(),
            Package(..) => todo!(),
            Apply(..) => todo!(),
            Assign(targets, rhs) => {
                self.translate_assign_rhs(targets, rhs, block);
                return;
            }
        };
        block.stmts.push(Statement { kind });
    }

    fn translate_assign_target(&mut self, tgt: &parse::Exp, block: &mut Block<'tcx>) -> (Ty<'tcx>, Result<Local, Operand<'tcx>>) {
        match &**tgt {
            ExpKind::Ident(idn) => {
                let local = self.params[&ArgRef::Ident(self.tcx.interner.mk_symbol(idn))];
                (self.locals[local], Ok(local))
            }
            ExpKind::UnOp(UnOp::Deref, rcv) => {
                let (rcv, ty) = self.translate_exp_local(rcv, self.any_resource_id(), block);
                let TyKind::ResourceId(inner) = ty.kind() else {
                    unreachable!()
                };
                (*inner, Err(rcv))
            }
            _ => unreachable!(),
        }
    }

    fn translate_assign_rhs(&mut self, targets: &Vec<parse::Exp>, rhs: &AssignRhs, block: &mut Block<'tcx>) {
        match rhs {
            AssignRhs::Exp(exp) => {
                let (ty, tgt) = self.translate_assign_target(&targets[0], block);
                let value = self.translate_exp_inner(exp, ty, self.curr_heap_nd());
                match tgt {
                    Ok(local) => {
                        block.stmts.push(Statement { kind: StatementKind::Eval(local, value) });
                    }
                    Err(rcv) => if let Some(nd) = value.as_operand() {
                        block.stmts.push(Statement { kind: StatementKind::Assign(self.curr_heap(), rcv, nd) });
                    } else {
                        let tmp = self.locals.push_and_get_key(ty);
                        block.stmts.push(Statement { kind: StatementKind::Eval(tmp, value) });
                        block.stmts.push(Statement { kind: StatementKind::Assign(self.curr_heap(), rcv, Operand::Local(tmp)) });
                    },
                }
            }
            AssignRhs::New(new) => {
                let (ty, tgt) = self.translate_assign_target(&targets[0], block);
                assert_eq!(ty, self.tcx.types.ref_);
                match tgt {
                    Ok(local) => {
                        let kind = StatementKind::Havoc(local);
                        block.stmts.push(Statement { kind });
                        self.translate_inhale_new(local, new, block);
                    }
                    Err(rcv) => {
                        let tmp = self.locals.push_and_get_key(self.tcx.types.ref_);
                        self.translate_inhale_new(tmp, new, block);
                        let kind = StatementKind::Assign(self.curr_heap(), rcv, Operand::Local(tmp));
                        block.stmts.push(Statement { kind });
                    }
                };
            }
            AssignRhs::Call(ident, args) => {
                let callee = self.tcx.get_callee(ident);
                let sig = self.tcx.fn_sig(callee).unwrap();

                let mut post_call = Vec::new();
                let targets = targets.iter().zip(sig.caller_returns().1).map(|(tgt, ety)| {
                    let (ty, tgt) = self.translate_assign_target(tgt, block);
                    assert_eq!(ty, *ety);
                    tgt.unwrap_or_else(|nd| {
                        let tmp = self.locals.push_and_get_key(*ety);
                        post_call.push(Statement { kind: StatementKind::Assign(self.curr_heap(), nd, Operand::Local(tmp)) });
                        tmp
                    })
                }).collect();

                let heap = Operand::Local(self.params[&ArgRef::Heap(None)]);
                let args = args.iter().zip(sig.caller_args().1).map(|(arg, ty)| {
                    self.translate_exp_local(arg, *ty, block).0
                }).chain([heap]).collect();
                block.stmts.push(Statement { kind: StatementKind::Call(targets, callee, args) });
                block.stmts.extend(post_call);
            }
        }
    }

    fn translate_inhale_new(&mut self, tmp: Local, new: &StarOrNames, block: &mut Block<'tcx>) {
        let fields: Vec<_> = match new {
            StarOrNames::Star => {
                let all = self.tcx.globals.data.iter_enumerated();
                let all = all.filter(|(_, data)| matches!(data.kind, MemberKind::Field));
                all.map(|(id, _)| DefId::from(id)).collect()
            }
            StarOrNames::Names(idents) =>
                idents.iter().map(|ident| self.tcx.get_callee(ident)).collect(),
        };
        let res = self.translate_resource_for_new(ExpOperand::Local(tmp), fields);
        block.stmts.push(Statement { kind: StatementKind::Ghost(None, InhaleExhale::Inhale, self.curr_heap(), res) });
    }

    fn translate_exp_local(&mut self, exp: &parse::Exp, ty: Ty<'tcx>, block: &mut Block<'tcx>) -> (Operand<'tcx>, Ty<'tcx>) {
        let exp = self.translate_exp_inner(exp, ty, self.curr_heap_nd());
        let ty = exp.result_ty();
        (exp.as_operand().unwrap_or_else(|| {
            let local = self.locals.push_and_get_key(ty);
            block.stmts.push(Statement { kind: StatementKind::Eval(local, exp) });
            Operand::Local(local)
        }), ty)
    }

    fn curr_heap(&self) -> Local {
        self.params[&ArgRef::Heap(None)]
    }

    fn curr_heap_nd(&self) -> Option<ExpOperand<'tcx>> {
        Some(ExpOperand::Local(self.curr_heap()))
    }
}

impl<'tcx> BlockConds<'tcx> {
    pub(crate) fn new<I: Iterator<Item = (Operand<'tcx>, bool)>>(conds: Option<impl Iterator<Item = I>>) -> Self {
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
