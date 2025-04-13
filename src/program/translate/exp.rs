use crate::{HashMap, HashSet, TiVec};

use crate::{parse::{AccExp, BinOp, ConstHeapKind, ConstKind, ExpKind, HeapUpdateOp, UnOp}, program::{exp::*, *}};

use super::TranslationCtxt;

impl<'b, 'tcx> TranslationCtxt<'b, 'tcx> {
    pub(crate) fn translate_exp(&self, exp: &crate::parse::Exp, ty: Ty<'tcx>, have_heap: bool) -> Exp<'tcx> {
        let heap = have_heap.then(|| {
            ExpOperand::Const(self.tcx.interner.mk_const(ConstKind::Heap(ConstHeapKind::Old)))
        });
        self.translate_exp_inner(exp, ty, heap)
    }

    pub(super) fn translate_exp_inner(&self, exp: &crate::parse::Exp, ty: Ty<'tcx>, heap: Option<ExpOperand<'tcx>>) -> Exp<'tcx> {
        let mut et = self.prepare_translator(heap);
        let e = et.translate_full(exp, [ty]);
        let rt = e.result_ty();
        assert!(ExpectedTys::tys_match(ty, rt), "type error: expected {ty:?}, found {rt:?}");
        e
    }

    pub(super) fn prepare_translator(&self, heap: Option<ExpOperand<'tcx>>) -> ExpTranslator<'_, 'b, 'tcx> {
        ExpTranslator {
            tcx: self,
            e: Default::default(),

            curr_cond: Some(Vec::new()),
            evaluated: HashMap::default(),
            conditionless: vec![TiVec::default()],

            let_bound: HashMap::default(),
            curr_nest: 0,
            heap,
        }
    }
}

type Operand<'tcx> = (ExpOperand<'tcx>, Ty<'tcx>);

pub(super) struct ExpTranslator<'a, 'b, 'tcx> {
    pub(super) tcx: &'a TranslationCtxt<'b, 'tcx>,
    e: Exp<'tcx>,
    evaluated: HashMap<ExpLine<'tcx>, (ExpLocal, Ty<'tcx>, u16)>,

    pub(super) curr_cond: Option<Vec<ExpCond<'tcx>>>,
    pub(super) conditionless: Vec<TiVec<ExpLocal, bool>>,

    let_bound: HashMap<Symbol<'tcx>, Operand<'tcx>>,
    curr_nest: u16,
    heap: Option<ExpOperand<'tcx>>,
}

impl<'tcx> ExpTranslator<'_, '_, 'tcx> {
    pub(super) fn translate_cond(&mut self, neg: bool, exp: &crate::parse::Exp) {
        if self.curr_cond.is_none() {
            return;
        }

        let cond = self.translate_chain(exp, self.tcx.tcx.types.bool_);
        if let Some(c) = cond.as_const() {
            if c.as_bool().unwrap() == neg {
                self.curr_cond = None;
            } else {
                return;
            }
        }
        self.curr_cond.as_mut().unwrap().push(ExpCond { cond, neg: false });
    }

    pub(super) fn translate_chain(&mut self, exp: &crate::parse::Exp, ty: Ty<'tcx>) -> ExpOperand<'tcx> {
        let (nd, rt) = self.translate(exp, [ty]);
        assert!(ExpectedTys::tys_match(ty, rt), "type error: expected {ty:?}, found {rt:?}");
        nd
    }

    pub(super) fn finish_chain(self) -> (Exp<'tcx>, Option<Box<[ExpCond<'tcx>]>>) {
        (self.e, self.curr_cond.map(|cc| cc.into_boxed_slice()))
    }

    fn translate_full(&mut self, exp: &crate::parse::Exp, tys: impl Into<ExpectedTys<'tcx>>) -> Exp<'tcx> {
        let e = core::mem::take(&mut self.e);
        let (r, ty) = self.translate(exp, tys);
        match r {
            ExpOperand::ExpLocal(n, l) => {
                assert_eq!(n, self.curr_nest);
                assert_eq!(l, self.e.lines.last_key().unwrap());
            }
            _ => {
                let line = self.mk_line(ExpLineKind::Use(r), ty);
                self.e.lines.push(line);
            }
        }
        core::mem::replace(&mut self.e, e)
    }

    fn translate(&mut self, exp: &crate::parse::Exp, tys: impl Into<ExpectedTys<'tcx>>) -> Operand<'tcx> {
        let tys = tys.into();
        let line = match &**exp {
            ExpKind::Field(..) | ExpKind::Acc(..) => unreachable!(),
            ExpKind::Const(c) => {
                let c = self.tcx.tcx.interner.mk_const_ref(c);
                let ty = self.tcx.tcx.const_ty(c);
                tys.check_ty(ty);
                return (ExpOperand::Const(c), ty);
            }
            ExpKind::Result => return self.mk_use(ArgRef::Result, tys),
            ExpKind::Old(ident, e) => {
                let heap = self.heap.expect("old without heap");
                let new = match ident {
                    None => {
                        let new = self.tcx.tcx.interner.mk_const(ConstKind::Heap(ConstHeapKind::Old));
                        ExpOperand::Const(new)
                    }
                    Some(label) => {
                        let label = self.tcx.tcx.interner.mk_symbol(label);
                        ExpOperand::Local(self.tcx.params[&ArgRef::Label(label)])
                    }
                };
                assert_ne!(heap, new, "unnecessary old");
                self.heap = Some(new);
                let e = self.translate(e, tys);
                self.heap = Some(heap);
                return e;
            }
            ExpKind::Ascribe(e, ty) => {
                let ty = self.tcx.tcx.translate_type(ty);
                tys.check_ty(ty);
                return self.translate(e, [ty]);
            }
            ExpKind::HeapUpdate(op, acc, e) => {
                let heap = self.mk_heap_update(*op, acc);
                let heap = self.heap.replace(heap);
                let r = self.translate(e, tys);
                self.heap = heap;
                return r;
            }
            ExpKind::Quantifier(kind, qvars, triggers, body) => {
                let curr_cond = self.curr_cond.as_mut().map(|cc| core::mem::take(cc));
                let (idns, tys): (Vec<_>, Vec<_>) = qvars.iter().map(|qv| {
                    let idn = self.tcx.tcx.interner.mk_symbol(&qv.idn.0);
                    let ty = self.tcx.tcx.translate_type(&qv.ty);
                    (idn, ty)
                }).unzip();
                let tys = self.tcx.tcx.interner.mk_ty_list(tys);
                for (i, (idn, ty)) in idns.iter().zip(tys.iter()).enumerate() {
                    let op = ExpOperand::QuantLocal(self.curr_nest, QuantLocal::from(i));
                    let old = self.let_bound.insert(*idn, (op, *ty));
                    assert!(old.is_none(), "duplicate variable name bound in quantifier {idn:?}");
                }
                let triggers = triggers.iter()
                    .map(|t| t.exp.iter().map(|st| self.translate_nest(st, None)).collect())
                    .collect();

                let body = self.translate_nest(body, [self.tcx.tcx.types.bool_]);

                for idn in idns.iter().rev() {
                    self.let_bound.swap_remove(idn);
                }
                self.curr_cond = curr_cond;

                self.mk_line(ExpLineKind::Quantifier(*kind, tys, triggers, body), self.tcx.tcx.types.bool_)
            }
            ExpKind::LetIn(decl, val, e) => {
                let decl = self.tcx.tcx.interner.mk_symbol(&decl.0);
                let v = self.translate(val, None);

                let old = self.let_bound.insert(decl, v);
                assert!(old.is_none(), "duplicate let bound");

                let r = self.translate(e, tys);
                self.let_bound.swap_remove(&decl);
                return r;
            }
            ExpKind::ForPerm(..) => todo!(),
            ExpKind::FuncApp(ident, args) => {
                let callee = self.tcx.tcx.get_callee(ident);
                let heap_dependent = self.tcx.tcx.is_heap_dependent(callee);
                let heap = heap_dependent.then(|| self.heap.unwrap());
                let sig = self.tcx.tcx.fn_sig(callee).unwrap();

                let caller_args = sig.caller_args().1;
                assert_eq!(args.len(), caller_args.len());
                let args = args.iter()
                    .zip(caller_args)
                    .map(|(arg, ty)| self.translate(arg, [*ty]).0)
                    .chain(heap);

                let kind = ExpLineKind::Call(callee, args.collect());
                let ty = sig.returns().1[0];
                self.mk_line(kind, ty)
            }
            ExpKind::Ident(ident) => {
                let i = self.tcx.tcx.interner.mk_symbol(ident);
                let lb = self.let_bound.get(&i).copied();
                if let Some((_, ty)) = lb {
                    tys.check_ty(ty);
                }
                return lb.unwrap_or_else(|| self.mk_use(ArgRef::Ident(i), tys));
            }
            ExpKind::BinOp(op, lhs, rhs) => {
                use crate::parse::BinOp::*;
                match *op {
                    Or | Implies | And => unreachable!(),
                    op => {
                        let (eq, lhs_tys, rhs_tys) = self.possible_bin_op(op);
                        let mut lhs = self.translate(lhs, lhs_tys);
                        let mut rhs = self.translate(rhs, rhs_tys);
                        if eq {
                            self.equate_tys(&mut lhs, &mut rhs);
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
                let e = self.translate(e, tys);
                self.apply_un_op(*op, e)
            }
        };
        tys.check_ty(line.ty);
        self.new_line(line)
    }

    fn mk_line(&mut self, kind: ExpLineKind<'tcx>, ty: Ty<'tcx>) -> ExpLine<'tcx> {
        let cond = self.get_condition(&kind);
        ExpLine { ty, cond, kind }
    }

    fn mk_ternary(&mut self, c: &crate::parse::Exp, t: &crate::parse::Exp, e: &crate::parse::Exp, tys: &ExpectedTys<'tcx>) -> Result<ExpLine<'tcx>, Operand<'tcx>> {
        let (cond, _) = self.translate(c, [self.tcx.tcx.types.bool_]);
        if let Some(c) = cond.as_const() {
            return Err(if c.as_bool().unwrap() {
                self.translate(t, tys.clone())
            } else {
                self.translate(e, tys.clone())
            });
        }
        if let Some(cc) = &mut self.curr_cond {
            cc.push(ExpCond { cond, neg: false });
        }
        let mut t = self.translate(t, tys.clone());
        if let Some(cc) = &mut self.curr_cond {
            let c = cc.last_mut().unwrap();
            assert_eq!(c.cond, cond);
            c.neg = true;
        }
        let mut e = self.translate(e, tys.clone());
        if let Some(cc) = &mut self.curr_cond {
            cc.pop();
        }

        self.equate_tys(&mut t, &mut e);
        Ok(self.mk_line(ExpLineKind::Ternary([cond, t.0, e.0]), t.1))
    }

    fn mk_heap_update(&mut self, op: HeapUpdateOp, acc: &AccExp) -> ExpOperand<'tcx> {
        assert_eq!(op, HeapUpdateOp::Unfold, "other heap ops not yet implemented");
        let heap = self.heap.expect("heap update without heap");

        let ty = self.tcx.any_resource_id();
        let loc = self.translate(&acc.acc.loc, [ty]);
        let perm = self.translate(&acc.perm, [self.tcx.tcx.types.real_]);
        let kind = ExpLineKind::HeapUpdate(op, [heap, loc.0, perm.0]);
        let line = self.mk_line(kind, self.tcx.tcx.types.heap_);
        self.new_line(line).0
    }

    fn translate_nest(&mut self, exp: &crate::parse::Exp, tys: impl Into<ExpectedTys<'tcx>>) -> Exp<'tcx> {
        self.curr_nest += 1;
        self.conditionless.push(TiVec::default());
        let e = self.translate_full(exp, tys);
        self.conditionless.pop();
        self.curr_nest -= 1;
        for line in e.lines.iter() {
            let rm = self.evaluated.swap_remove(line);
            assert!(rm.is_some_and(|(.., lvl)| lvl == self.curr_nest + 1));
        }
        e
    }

    fn mk_use(&self, r: ArgRef<'tcx>, tys: ExpectedTys<'tcx>) -> Operand<'tcx> {
        let local = self.tcx.get_param(r);
        let ty = self.tcx.locals[local];
        tys.check_ty(ty);
        (ExpOperand::Local(local), ty)
    }

    fn possible_bin_op(&self, op: BinOp) -> (bool, ExpectedTys<'tcx>, ExpectedTys<'tcx>) {
        let types = &self.tcx.tcx.types;
        use BinOp::*;
        match op {
            Implies | Or | And | Gt | Ge => unreachable!(),
            Iff => (false, [types.bool_].into(), [types.bool_].into()),
            Eq | Neq => (true, None.into(), None.into()),
            Lt | Le | Plus | Minus | Mult | Div | Mod => (true, [types.int_, types.real_].into(), [types.int_, types.real_].into()),
            IntDiv => (false, [types.int_].into(), [types.int_].into()),
            _ => todo!(),
        }
    }

    fn apply_bin_op(&mut self, op: BinOp, lhs: Operand<'tcx>, rhs: Operand<'tcx>) -> ExpLine<'tcx> {
        use BinOp::*;
        let types = &self.tcx.tcx.types;
        let ty = match op {
            Implies | Or | And => unreachable!(),
            Iff | Eq | Neq | Lt | Le | Gt | Ge | In => types.bool_,
            Plus | Minus | Mult | Div => lhs.1,
            Mod | IntDiv => types.int_,
            _ => todo!(),
        };
        self.mk_line(ExpLineKind::BinOp(op, [lhs.0, rhs.0]), ty)
    }

    fn possible_un_op(&self, op: UnOp) -> ExpectedTys<'tcx> {
        let types = &self.tcx.tcx.types;
        use UnOp::*;
        match op {
            Not => [types.bool_].into(),
            Neg => [types.int_, types.real_].into(),
            IntToReal => [types.int_].into(),
            Deref | Perm => [self.tcx.any_resource_id()].into(),
            Abs => todo!(),
        }
    }

    fn apply_un_op(&mut self, op: UnOp, e: Operand<'tcx>) -> ExpLine<'tcx> {
        use UnOp::*;
        let ty = match op {
            Not | Neg => e.1,
            IntToReal => self.tcx.tcx.types.real_,
            Abs => todo!(),
            Deref => {
                let heap = self.heap.expect("heap deref without heap");
                let kind = ExpLineKind::Heap(HeapOp::Deref, [heap, e.0]);
                let TyKind::ResourceId(ty) = *e.1.kind() else {
                    unreachable!();
                };
                return self.mk_line(kind, ty)
            }
            Perm => {
                let heap = self.heap.expect("heap perm without heap");
                let kind = ExpLineKind::Heap(HeapOp::Perm, [heap, e.0]);
                return self.mk_line(kind, self.tcx.tcx.types.real_);
            }
        };
        self.mk_line(ExpLineKind::UnOp(op, e.0), ty)
    }

    fn equate_tys(&mut self, lhs: &mut Operand<'tcx>, rhs: &mut Operand<'tcx>) {
        // TODO: go from `int` type to `ref` type here if required
        assert_eq!(lhs.1, rhs.1);
    }

    pub(super) fn new_line(&mut self, line: ExpLine<'tcx>) -> Operand<'tcx> {
        let ty = line.ty;
        let op = self.optimise_line(line).unwrap_or_else(|line| {
            let ty = line.ty;
            // TODO: check for the same line evaluated with fewer conds
            let (l, prev_ty, _) = self.evaluated.entry(line).or_insert_with_key(|line| {
                self.conditionless[self.curr_nest as usize].push(line.cond.as_ref().is_some_and(|c| c.is_empty()));
                let l = self.e.lines.push_and_get_key(line.clone());
                (l, ty, self.curr_nest)
            });
            assert_eq!(*prev_ty, ty);
            ExpOperand::ExpLocal(self.curr_nest, *l)
        });
        (op, ty)
    }

    pub(super) fn negate(&mut self, c: ExpOperand<'tcx>) -> ExpOperand<'tcx> {
        let line = self.mk_line(ExpLineKind::UnOp(UnOp::Not, c), self.tcx.tcx.types.bool_);
        self.new_line(line).0
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

#[derive(Debug, Default, Clone)]
pub struct ExpectedTys<'tcx> {
    tys: Option<HashSet<Ty<'tcx>>>,
}

impl<'tcx, I: IntoIterator<Item = Ty<'tcx>>> From<I> for ExpectedTys<'tcx> {
    fn from(tys: I) -> Self {
        let tys: HashSet<_> = tys.into_iter().collect();
        let tys = Some(tys).filter(|ty| !ty.is_empty());
        Self { tys }
    }
}

impl<'tcx> ExpectedTys<'tcx> {
    fn check_ty(&self, ty: Ty<'tcx>) {
        if let Some(ty_) = &self.tys {
            if ty_.contains(&ty) {
                return;
            }
            if ty_.iter().any(|&t| Self::tys_match(t, ty)) {
                return;
            }
            panic!("type error: expected one of {ty_:?}, found {ty:?}");
        }
    }

    fn tys_match(e: Ty<'tcx>, ty: Ty<'tcx>) -> bool {
        if core::mem::discriminant(e.kind()) != core::mem::discriminant(ty.kind()) {
            return false;
        }
        match (e.kind(), ty.kind()) {
            (TyKind::Compound(..), TyKind::Compound(..)) => unreachable!(),
            (TyKind::ResourceId(e), TyKind::ResourceId(..)) => matches!(e.kind(), TyKind::Heap),
            (TyKind::Domain(s, ..), TyKind::Domain(s_, ..)) if s != s_ => false,
            (TyKind::Domain(..), TyKind::Domain(..)) => todo!(),
            (a, b) => a == b,
        }
    }
}
