use std::u32;

use crate::translate::member::tychk::ExpLineIfx;
use crate::translate::member::tychk::ExpOperandIfx;
use crate::translate::member::tychk::ExpectedTys;
use crate::translate::member::tychk::TypeChecker;
use crate::translate::member::TranslationCtxt;
use crate::vmir::middle::*;
use crate::vmir::ty::*;
use crate::vmir::Symbol;
use crate::{HashMap, HashSet, TiVec};

use crate::{
    parse::{AccExp, BinOp, ConstHeapKind, ConstKind, ExpKind, HeapUpdateOp, UnOp},
    translate::*,
};

impl<'b, 'tcx> TranslationCtxt<'b, 'tcx> {
    pub(crate) fn translate_exp(
        &self,
        exp: &crate::parse::Exp,
        ty: Ty<'tcx>,
        have_heap: bool,
    ) -> Exp<'tcx> {
        let heap = have_heap.then(|| {
            OperandKind::Const(
                self.tcx
                    .interner
                    .mk_const(ConstKind::Heap(ConstHeapKind::Old)),
            )
        });
        self.translate_exp_inner(exp, Some(ty), heap)
    }

    pub(super) fn translate_exp_inner(
        &self,
        exp: &crate::parse::Exp,
        ty: Option<Ty<'tcx>>,
        heap: Option<OperandKind<'tcx>>,
    ) -> Exp<'tcx> {
        let et = self.prepare_translator(heap);
        et.translate_final(exp, ty)
    }

    pub(super) fn prepare_translator(
        &self,
        heap: Option<OperandKind<'tcx>>,
    ) -> ExpTranslator<'_, 'b, 'tcx> {
        let heap = heap.map(|h| {
            Operand {
                ty: self.tcx.types.heap_,
                kind: h,
            }
            .into()
        });
        ExpTranslator {
            tcx: self,
            e: Default::default(),

            curr_cond: Default::default(),
            evaluated: HashMap::default(),
            used: HashSet::default(),
            conditionless: vec![TiVec::default()],

            let_bound: HashMap::default(),
            curr_qvars: QuantLocal::ZERO,
            curr_nest: 0,
            heap,

            param_infcx: Default::default(),
        }
    }
}

pub(super) struct ExpTranslator<'a, 'b, 'tcx> {
    pub(super) tcx: &'a TranslationCtxt<'b, 'tcx>,
    e: Vec<ExpLineIfx<'tcx>>,
    pub(super) curr_cond: ExpConds<'tcx>,

    let_bound: HashMap<Symbol<'tcx>, ExpOperandIfx<'tcx>>,
    pub(super) curr_qvars: QuantLocal,
    pub(super) curr_nest: u16,
    heap: Option<ExpOperand<'tcx>>,

    param_infcx: TypeChecker<'tcx>,
}

impl<'tcx> ExpTranslator<'_, '_, 'tcx> {
    // pub(super) fn any_address(&mut self) -> Ty<'tcx> {
    //     self.param_infcx.any_address(self.tcx)
    // }

    pub(super) fn translate_cond(&mut self, neg: bool, exp: &crate::parse::Exp) {
        if self.curr_cond.is_false() {
            return;
        }

        let cond = self.translate_chain(exp, self.tcx.tcx.types.bool_);
        if let Some(c) = cond.as_const() {
            if c.as_bool().unwrap() == neg {
                self.curr_cond.set_false();
            }
            return;
        }
        self.curr_cond.push_cond(cond, neg);
    }

    pub(super) fn translate_chain(
        &mut self,
        exp: &crate::parse::Exp,
        ty: Ty<'tcx>,
    ) -> ExpOperand<'tcx> {
        let nd = self.translate_resolve(exp, [ty]);
        // self.param_infcx.assert_tys_match(ty, nd.ty);
        assert_eq!(nd.ty, ty, "TODO: chain result type mismatch");
        self.assume_used(nd);
        nd
    }

    pub(super) fn add_line(&mut self, ty: Ty<'tcx>, kind: ExpLineKind<'tcx>) -> ExpOperand<'tcx> {
        let line = self.mk_line(ty, kind);
        let nd = self.new_line(line);
        self.assume_used(nd);
        nd
    }

    pub(super) fn curr_line(&self) -> Option<ExpLocal> {
        self.e.last_key()
    }

    pub(super) fn finish_chain(&mut self) -> (ExpLocal, ExpConds<'tcx>) {
        (self.curr_line().unwrap(), self.curr_cond.take())
    }

    pub(super) fn translate_final(mut self, exp: &crate::parse::Exp, ty: Option<Ty<'tcx>>) -> Exp<'tcx> {
        assert!(self.curr_cond.is_true());
        let e = self.translate_full(exp, ty);
        let rt = e.result_ty();
        if let Some(ty) = ty {
            assert_eq!(ty, rt, "TODO: result type mismatch in final translation");
            // self.param_infcx.assert_tys_match(ty, rt);
        }
        e
    }

    pub(super) fn translate_final_true(mut self) -> Exp<'tcx> {
        self.finish_expression(self.tcx.tcx.const_operand(ConstKind::bool(true)).into())
    }

    fn assume_used(&mut self, nd: ExpOperand<'tcx>) {
        if let Some((n, l)) = nd.as_exp_local() {
            assert_eq!(n, self.curr_nest);
            self.used.insert(l);
        }
    }

    fn translate_full(
        &mut self,
        exp: &crate::parse::Exp,
        ty: Option<Ty<'tcx>>,
    ) -> Exp<'tcx> {
        let r = self.translate_resolve(exp, ty);
        self.finish_expression(r)
    }

    fn finish_expression(&mut self, result: ExpOperand<'tcx>) -> Exp<'tcx> {
        assert!(self.curr_cond.is_true());
        let lines = core::mem::take(&mut self.e);
        let used = core::mem::take(&mut self.used);
        self.optimise_exp(Exp { lines, result }, used)
    }

    fn translate_resolve(
        &mut self,
        exp: &crate::parse::Exp,
        tys: impl Into<ExpectedTys<'tcx>>,
    ) -> ExpOperand<'tcx> {
        let result = self.translate(exp, tys);
        self.param_infcx.resolve_ty(&self.tcx, result)
    }

    fn translate(
        &mut self,
        exp: &crate::parse::Exp,
        tys: impl Into<ExpectedTys<'tcx>>,
    ) -> ExpOperandIfx<'tcx> {
        let tys = tys.into();
        // println!("EQOIHI: {tys:?}\n({exp:?})\n");
        let line = match &**exp {
            ExpKind::Field(..) | ExpKind::Acc(..) | ExpKind::MagicWand(..) => unreachable!(),
            ExpKind::Const(c) => {
                let c = self.tcx.tcx.interner.mk_const_ref(c);
                let ty = self.tcx.tcx.const_ty(c);
                self.param_infcx.check_ty(&tys, ty);
                return Operand {
                    ty,
                    kind: OperandKind::Const(c),
                }
                .into();
            }
            ExpKind::Result => return self.mk_use(ArgRef::Result, tys),
            ExpKind::Old(ident, e) => {
                let heap = self.heap.expect("old without heap");
                let ty = self.tcx.tcx.types.heap_;
                let new = match ident {
                    None => {
                        let new = self
                            .tcx
                            .tcx
                            .interner
                            .mk_const(ConstKind::Heap(ConstHeapKind::Old));
                        Operand {
                            ty,
                            kind: OperandKind::Const(new),
                        }
                    }
                    Some(label) => {
                        let label = self.tcx.tcx.interner.mk_symbol(label);
                        Operand {
                            ty,
                            kind: OperandKind::Local(self.tcx.params[&ArgRef::Label(label)].0),
                        }
                    }
                };
                let new = ExpOperand::from(new);
                if heap == new {
                    eprintln!("warning: unnecessary old");
                }
                self.heap = Some(new);
                let e = self.translate_resolve(e, tys);
                self.heap = Some(heap);
                return e;
            }
            ExpKind::Ascribe(e, ty) => {
                let ty = self.tcx.tycx.translate(ty);
                self.param_infcx.check_ty(&tys, ty);
                return self.translate_resolve(e, [ty]);
            }
            ExpKind::HeapUpdate(op, acc, e) => {
                let heap = self.mk_heap_update(*op, acc);
                let heap = self.heap.replace(heap);
                let r = self.translate_resolve(e, tys);
                self.heap = heap;
                return r;
            }
            ExpKind::Quantifier(kind, qvars, triggers, body) => {
                let curr_cond = self.curr_cond.take();
                let curr_qvars = self.curr_qvars;
                self.curr_qvars = QuantLocal::from(usize::from(curr_qvars) + qvars.len());

                let (idns, tys): (Vec<_>, Vec<_>) = qvars
                    .iter()
                    .map(|qv| {
                        let idn = self.tcx.tcx.interner.mk_symbol(&qv.idn.0);
                        let ty = self.tcx.tycx.translate(&qv.ty);
                        (idn, ty)
                    })
                    .unzip();
                let tys = self.tcx.tcx.interner.mk_ty_list(tys);
                for (i, (idn, ty)) in idns.iter().zip(tys.iter()).enumerate() {
                    let kind =
                        ExpOperandKind::QuantLocal(QuantLocal::from(usize::from(curr_qvars) + i));
                    let old = self.let_bound.insert(*idn, ExpOperand { ty: *ty, kind });
                    assert!(
                        old.is_none(),
                        "duplicate variable name bound in quantifier {idn:?}"
                    );
                }
                let triggers = triggers
                    .iter()
                    .map(|t| {
                        t.exp
                            .iter()
                            .map(|st| self.translate_nest(st, ExpectedTys::default()))
                            .collect()
                    })
                    .collect();

                let body = self.translate_nest(body, [self.tcx.tcx.types.bool_]);

                for idn in idns.iter().rev() {
                    self.let_bound.swap_remove(idn);
                }
                self.curr_qvars = curr_qvars;
                self.curr_cond = curr_cond;

                self.mk_line(
                    self.tcx.tcx.types.bool_,
                    ExpLineKind::Quantifier(*kind, tys, triggers, body),
                )
            }
            ExpKind::LetIn(decl, val, e) => {
                let decl = self.tcx.tcx.interner.mk_symbol(&decl.0);
                let v = self.translate_resolve(val, ExpectedTys::default());

                let old = self.let_bound.insert(decl, v);
                assert!(old.is_none(), "duplicate let bound");

                let r = self.translate_resolve(e, tys);
                self.let_bound.swap_remove(&decl);
                return r;
            }
            ExpKind::ForPerm(..) => todo!(),
            ExpKind::FuncApp(ident, args) => {
                let callee = self.tcx.tcx.resolve(ident).unwrap();
                let (function, ..) = self.tcx.tcx.get_any_function(callee).unwrap();
                let f = &self.tcx.tcx.members.functions[function];
                let params = f.params();
                let ret = f.ret();

                // let heap_dependent = self.tcx.tcx.is_heap_dependent(callee);
                // let heap = heap_dependent.then(|| self.heap.unwrap());
                // let sig = self.tcx.tcx.fn_sig(callee).unwrap();

                // let caller_args = sig.caller_args().1;
                assert_eq!(args.len(), params.len());
                let mut args: Vec<_> = args
                    .iter()
                    .zip(params)
                    .map(|(arg, ty)| self.translate_resolve(arg, [*ty]))
                    .collect();
                if let Some((pre, snap)) = f.heap_pre() {
                    let line = self.mk_line(snap, ExpLineKind::Snapshot(pre, args.clone()));
                    let heap_arg = self.new_line(line);
                    args.push(heap_arg);
                }

                let kind = ExpLineKind::Call(function, args);
                self.mk_line(ret, kind)
            }
            ExpKind::Ident(ident) => {
                let i = self.tcx.tcx.interner.mk_symbol(ident);
                let lb = self.let_bound.get(&i).copied();
                if let Some(nd) = lb {
                    self.param_infcx.check_ty(&tys, nd.ty);
                }
                return lb.unwrap_or_else(|| self.mk_use(ArgRef::Ident(i), tys));
            }
            ExpKind::BinOp(op, lhs, rhs) => {
                use crate::parse::BinOp::*;
                match *op {
                    Or | Implies | And => unreachable!(),
                    op => {
                        let pbo = self.possible_bin_op(op);
                        let mut lhs = self.translate(lhs, pbo.lhs);
                        let mut rhs = self.translate(rhs, pbo.rhs);
                        self.param_infcx.resolve_ty(&self.tcx, &mut lhs.ty);
                        self.param_infcx.resolve_ty(&self.tcx, &mut rhs.ty);
                        if pbo.must_eq {
                            self.equate_tys(&mut lhs, &mut rhs, pbo.upgrade_real_pre_op && tys.want_real());
                        }
                        self.apply_bin_op(op, lhs, rhs)
                    }
                }
            }
            ExpKind::Ternary(c, t, e) => match self.mk_ternary(c, t, e, &tys) {
                Ok(line) => line,
                Err(operand) => return operand,
            },
            ExpKind::Index(..) => todo!(),
            ExpKind::UnOp(op, e) => {
                let tys = self.possible_un_op(*op);
                let e = self.translate_resolve(e, tys);
                self.apply_un_op(*op, e)
            }
            ExpKind::AdtConstructor(cons, args) => {
                let cons = self.tcx.tcx.resolve(cons).unwrap();
                let (adt, vid) = self.tcx.tcx.get_constructor(cons.expect("todo std")).unwrap();
                let data = self.tcx.tcx.interner.get_adt_def(adt).data();
                let fields = &data.variants[vid].fields;
                assert_eq!(fields.len(), args.len(), "wrong number of fields for adt {adt:?}");
                let ty_list = self.param_infcx.mk_param_list(self.tcx, &data.params);
                let params = fields.iter().map(|f| f.ty(self.tcx.tcx, ty_list));
                let args = args
                    .iter()
                    .zip(params)
                    .map(|(arg, ty)| self.translate_resolve(arg, [ty]))
                    .collect();
                let kind = ExpLineKind::Adt(adt, AdtOp::Construct(vid, args));

                let ty = self.tcx.tcx.interner.mk_ty_from_kind(TyKind::Domain(DomainKind::Adt(adt), ty_list));
                self.mk_line(ty, kind)
            }
            ExpKind::AdtDestructor(e, fd) => {
                let (cons, idx) = self.tcx.tcx.resolve_adt_destr(fd).unwrap();
                let fid = FieldIdx::from(idx);
                let (adt, vid) = self.tcx.tcx.get_constructor(cons).unwrap();
                let data = self.tcx.tcx.interner.get_adt_def(adt).data();

                let ty_list = self.param_infcx.mk_param_list(self.tcx, &data.params);
                let ty = self.tcx.tcx.interner.mk_ty_from_kind(TyKind::Domain(DomainKind::Adt(adt), ty_list));
                let e = self.translate_resolve(e, [ty]);

                let data = self.tcx.tcx.interner.get_adt_def(adt).data();
                let field = data.variants[vid].fields[fid].ty(self.tcx.tcx, ty_list);

                let kind = ExpLineKind::Adt(adt, AdtOp::Destructor(e, vid, fid));
                self.mk_line(field, kind)
            }
            ExpKind::AdtDiscriminator(e, cons) => {
                let cons = self.tcx.tcx.resolve(cons).unwrap();
                let (adt, vid) = self.tcx.tcx.get_constructor(cons.expect("todo std")).unwrap();

                // TODO: `ExpectedTys::Exact` here with infervars for param list
                let e = self.translate_resolve(e, ExpectedTys::Adt(adt));

                let kind = ExpLineKind::Adt(adt, AdtOp::Discriminator(e, vid));
                self.mk_line(self.tcx.tcx.types.bool_, kind)
            }
        };
        let mut result = self.new_line(line);
        if tys.want_real() && matches!(result.ty.kind(), TyKind::Int) {
            result = self.int_to_real(result);
        }
        self.param_infcx.check_ty(&tys, result.ty);
        result
    }

    fn mk_line(&self, ty: Ty<'tcx>, kind: ExpLineKind<'tcx>) -> ExpLine<'tcx> {
        let cond = self.get_condition(&kind);
        ExpLine { ty, cond, kind }
    }

    fn mk_ternary(
        &mut self,
        c: &crate::parse::Exp,
        t: &crate::parse::Exp,
        e: &crate::parse::Exp,
        tys: &ExpectedTys<'tcx>,
    ) -> Result<ExpLine<'tcx>, ExpOperand<'tcx>> {
        let cond = self.translate_resolve(c, [self.tcx.tcx.types.bool_]);
        if let Some(c) = cond.as_const() {
            return Err(if c.as_bool().unwrap() {
                self.translate_resolve(t, tys.clone())
            } else {
                self.translate_resolve(e, tys.clone())
            });
        }
        let mut token = self.curr_cond.push_cond(cond, false);
        let mut t = self.translate_resolve(t, tys.clone());
        self.curr_cond.negate_cond(&mut token);
        let mut e = self.translate_resolve(e, tys.clone());
        self.curr_cond.pop_cond(token);

        self.equate_tys(&mut t, &mut e, false);
        Ok(self.mk_line(t.ty, ExpLineKind::Ternary([cond, t, e])))
    }

    fn mk_heap_update(&mut self, op: HeapUpdateOp, acc: &AccExp) -> ExpOperand<'tcx> {
        assert_eq!(
            op,
            HeapUpdateOp::Unfold,
            "other heap ops not yet implemented"
        );
        let heap = self.heap.expect("heap update without heap");
        let ExpKind::FuncApp(pred, args) = &*acc.acc.loc else {
            unreachable!();
        };
        let pred = self.tcx.tcx.resolve(pred).unwrap();
        let (pred, tys) = self.tcx.tcx.get_fold_unfold(pred.unwrap());
        assert_eq!(args.len(), tys.len());
        let args = args.iter().zip(tys).map(|(arg, ty)| {
            self.translate_resolve(arg, [*ty])
        }).collect();
        let perm = self.translate_resolve(&acc.perm, [self.tcx.tcx.types.real_]);
        let kind = ExpLineKind::Calling(CallKind::UnCall, pred, [heap, perm], args);
        let line = self.mk_line(self.tcx.tcx.types.heap_, kind);
        self.new_line(line)
    }

    fn translate_nest(
        &mut self,
        exp: &crate::parse::Exp,
        tys: impl Into<ExpectedTys<'tcx>>,
    ) -> Exp<'tcx> {
        let old_e = core::mem::take(&mut self.e);
        let old_used = core::mem::take(&mut self.used);
        self.curr_nest += 1;
        self.conditionless.push(TiVec::default());

        let e = self.translate_full(exp, tys);

        self.conditionless.pop();
        self.curr_nest -= 1;
        self.used = old_used;
        self.e = old_e;

        self.evaluated.retain(|_, nd| {
            let (nest, _) = nd.as_exp_local().unwrap();
            assert!(nest <= self.curr_nest + 1);
            nest <= self.curr_nest
        });
        e
    }

    fn mk_use(&mut self, r: ArgRef<'tcx>, tys: ExpectedTys<'tcx>) -> ExpOperand<'tcx> {
        let (local, ty) = self.tcx.get_param(r);
        self.param_infcx.check_ty(&tys, ty);
        Operand {
            ty,
            kind: OperandKind::Local(local),
        }
        .into()
    }

    fn possible_bin_op(&mut self, op: BinOp) -> PossibleBinOp<'tcx> {
        let types = &self.tcx.tcx.types;
        use BinOp::*;
        match op {
            Implies | Or | And | Gt | Ge => unreachable!(),
            Iff => PossibleBinOp { must_eq: false, upgrade_real_pre_op: false, lhs: [types.bool_].into(), rhs: [types.bool_].into() },
            Eq | Neq => {
                let any = self.param_infcx.any_ty(self.tcx);
                PossibleBinOp { must_eq: true, upgrade_real_pre_op: false, lhs: [any].into(), rhs: [any].into() }
            },
            Lt | Le | Plus | Minus | Mult | Div | Mod => PossibleBinOp {
                must_eq: true,
                upgrade_real_pre_op: matches!(op, Div | Mod),
                lhs: [types.int_, types.real_].into(),
                rhs: [types.int_, types.real_].into(),
            },
            IntDiv => PossibleBinOp {
                must_eq: false,
                upgrade_real_pre_op: false,
                lhs: [types.int_].into(),
                rhs: [types.int_].into(),
            },
            _ => todo!(),
        }
    }

    fn apply_bin_op(
        &mut self,
        op: BinOp,
        lhs: ExpOperand<'tcx>,
        rhs: ExpOperand<'tcx>,
    ) -> ExpLine<'tcx> {
        use BinOp::*;
        let types = &self.tcx.tcx.types;
        let ty = match op {
            Implies | Or | And => unreachable!(),
            Iff | Eq | Neq | Lt | Le | Gt | Ge | In => types.bool_,
            Plus | Minus | Mult | Div => lhs.ty,
            Mod | IntDiv => types.int_,
            _ => todo!(),
        };
        self.mk_line(ty, ExpLineKind::BinOp(op, [lhs, rhs]))
    }

    fn possible_un_op(&mut self, op: UnOp) -> ExpectedTys<'tcx> {
        let types = &self.tcx.tcx.types;
        use UnOp::*;
        match op {
            Not => [types.bool_].into(),
            Neg => [types.int_, types.real_].into(),
            IntToReal => [types.int_].into(),
            Deref | Perm => [self.any_address()].into(),
            Abs => todo!(),
        }
    }

    fn apply_un_op(&mut self, op: UnOp, e: ExpOperand<'tcx>) -> ExpLine<'tcx> {
        use UnOp::*;
        let ty = match op {
            Not | Neg => e.ty,
            IntToReal => self.tcx.tcx.types.real_,
            Abs => todo!(),
            Deref => {
                let heap = self.heap.expect("heap deref without heap");
                let kind = ExpLineKind::Heap(HeapOp::Deref, [heap, e]);
                let ty = e.ty.deref();
                return self.mk_line(ty, kind);
            }
            Perm => {
                let heap = self.heap.expect("heap perm without heap");
                let kind = ExpLineKind::Heap(HeapOp::Perm, [heap, e]);
                return self.mk_line(self.tcx.tcx.types.real_, kind);
            }
        };
        self.mk_line(ty, ExpLineKind::UnOp(op, e))
    }

    fn equate_tys(&mut self, lhs: &mut ExpOperand<'tcx>, rhs: &mut ExpOperand<'tcx>, upgrade_int: bool) {
        match (lhs.ty.kind(), rhs.ty.kind()) {
            (TyKind::Int, TyKind::Real) => {
                *lhs = self.int_to_real(*lhs);
            }
            (TyKind::Real, TyKind::Int) => {
                *rhs = self.int_to_real(*rhs);
            }
            (TyKind::Int, TyKind::Int) if upgrade_int => {
                *lhs = self.int_to_real(*lhs);
                *rhs = self.int_to_real(*rhs);
            }
            _ => {}
        }
        assert_eq!(lhs.ty, rhs.ty);
    }

    pub(super) fn new_line(&mut self, line: ExpLineIfx<'tcx>) -> ExpOperandIfx<'tcx> {
        self.optimise_line(line).unwrap_or_else(|mut line| {
            let cc: Option<usize> = line.cond_mut().cond_count();
            // TODO: optimise this! (check for an existing line with fewer conds)
            let mut cond = line.cond_mut().take().into_iter().unwrap();
            loop {
                let Some(nd) = self.evaluated.get(&line).copied() else {
                    let Some(c) = cond.next() else {
                        break;
                    };
                    line.cond_mut().push_cond(c.cond, c.neg);
                    continue;
                };
                assert_eq!(nd.ty, line.ty);
                assert!(nd.as_exp_local().unwrap().0 <= self.curr_nest);
                return nd;
            }
            assert_eq!(cc, line.cond_mut().cond_count());
            self.conditionless[self.curr_nest as usize].push(line.cond_mut().is_true());
            let l = self.e.push_and_get_key(line.clone());
            let nd = ExpOperand {
                ty: line.ty,
                kind: ExpOperandKind::ExpLocal(self.curr_nest, l),
            };
            self.evaluated.insert(line, nd);
            nd
        })
    }

    pub(super) fn negate(&mut self, c: ExpOperand<'tcx>) -> ExpOperand<'tcx> {
        assert_eq!(*c.ty.kind(), TyKind::Bool);
        let line = self.mk_line(self.tcx.tcx.types.bool_, ExpLineKind::UnOp(UnOp::Not, c));
        self.new_line(line)
    }

    fn int_to_real(&mut self, c: ExpOperand<'tcx>) -> ExpOperand<'tcx> {
        assert_eq!(*c.ty.kind(), TyKind::Int);
        let line = self.mk_line(self.tcx.tcx.types.real_, ExpLineKind::UnOp(UnOp::IntToReal, c));
        self.new_line(line)
    }

    // pub(super) fn inline_exp(&mut self, mut exp: Exp<'tcx>) -> ExpOperand<'tcx> {
    //     if let Some(o) = exp.as_operand() {
    //         return o;
    //     }
    //     exp.walk_operands(&mut |op| self.inline_operand(op));
    //     let op = exp.lines.last().unwrap().kind.as_use();
    //     if op.is_some() {
    //         exp.lines.pop();
    //     }
    //     for line in exp.lines.into_iter() {
    //         let curr_line = self.e.lines.push_and_get_key(line.clone());
    //         let old = self.evaluated.insert(line.kind.clone(), (self.curr_nest, curr_line, line.ty));
    //         assert!(old.is_none(), "duplicate line");
    //         self.added_at_level.push(line.kind);
    //     }
    //     op.unwrap_or_else(|| {
    //         let k = self.e.lines.last_key().unwrap();
    //         ExpOperand::ExpLocal(self.curr_nest, k)
    //     })
    // }

    // fn inline_operand(&self, operand: &mut ExpOperand<'tcx>) {
    //     let ExpOperand::ExpLocal(n, l) = operand else {
    //         return;
    //     };
    //     if *n <= self.curr_nest {
    //         return;
    //     }
    //     *n -= 1;
    //     if *n == self.curr_nest {
    //         *l = ExpLocal::from(self.e.lines.len() + usize::from(*l))
    //     }
    // }
}

struct PossibleBinOp<'tcx> {
    must_eq: bool,
    /// If we `want_real`, then for most ops it's fine to stay with `int * int`
    /// and only do `int_to_real` later up. But for e.g. `/` we want to upgrade
    /// to `real / real` right away if `want_real` is true.
    upgrade_real_pre_op: bool,
    lhs: ExpectedTys<'tcx>,
    rhs: ExpectedTys<'tcx>,
}
