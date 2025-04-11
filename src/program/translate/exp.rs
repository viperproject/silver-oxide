use crate::{HashMap, HashSet};

use crate::{parse::{AccExp, BinOp, ConstHeapKind, ConstKind, ExpKind, HeapUpdateOp, UnOp}, program::{exp::*, *}};

use super::TranslationCtxt;

impl<'tcx> TranslationCtxt<'_, 'tcx> {
    pub(crate) fn translate_exp(&self, exp: &crate::parse::Exp, ty: Ty<'tcx>, have_heap: bool) -> Exp<'tcx> {
        self.translate_exp_inner(exp, ty, have_heap.then(|| true))
    }

    pub(super) fn translate_exp_inner(&self, exp: &crate::parse::Exp, ty: Ty<'tcx>, use_old_heap: Option<bool>) -> Exp<'tcx> {
        let heap = use_old_heap.map(|old| {
            let kind = if old {
                ConstHeapKind::Old
            } else {
                ConstHeapKind::SelfFraming
            };
            ExpOperand::Const(self.tcx.interner.mk_const(ConstKind::Heap(kind)))
        });
        let mut et = ExpTranslator {
            tcx: self,
            e: Default::default(),
            evaluated: HashMap::default(),
            added_at_level: Vec::new(),

            let_bound: HashMap::default(),
            curr_nest: 0,
            heap,
        };
        let e = et.translate_full(exp, [ty]);
        let rt = e.result_ty();
        assert!(ExpectedTys::tys_match(ty, rt), "type error: expected {ty:?}, found {rt:?}");
        e
    }
}

type Operand<'tcx> = (ExpOperand<'tcx>, Ty<'tcx>);

pub(super) struct ExpTranslator<'a, 'b, 'tcx> {
    pub(super) tcx: &'a TranslationCtxt<'b, 'tcx>,
    e: Exp<'tcx>,
    evaluated: HashMap<ExpLineKind<'tcx>, (u16, ExpLocal, Ty<'tcx>)>,
    added_at_level: Vec<ExpLineKind<'tcx>>,

    let_bound: HashMap<Symbol<'tcx>, Operand<'tcx>>,
    curr_nest: u16,
    heap: Option<ExpOperand<'tcx>>,
}

impl<'tcx> ExpTranslator<'_, '_, 'tcx> {
    fn translate_full(&mut self, exp: &crate::parse::Exp, tys: impl Into<ExpectedTys<'tcx>>) -> Exp<'tcx> {
        let e = core::mem::take(&mut self.e);
        let (r, ty) = self.translate(exp, tys);
        match r {
            ExpOperand::ExpLocal(n, l) => {
                assert_eq!(n, self.curr_nest);
                assert_eq!(l, self.e.lines.last_key().unwrap());
            }
            ExpOperand::Local(..) | ExpOperand::Const(..) => {
                self.e.lines.push(ExpLine {
                    ty,
                    kind: ExpLineKind::Use(r),
                });
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
                self.curr_nest += 1;
                let (idns, tys): (Vec<_>, Vec<_>) = qvars.iter().map(|qv| {
                    let idn = self.tcx.tcx.interner.mk_symbol(&qv.idn.0);
                    let ty = self.tcx.tcx.translate_type(&qv.ty);
                    (idn, ty)
                }).unzip();
                let tys = self.tcx.tcx.interner.mk_ty_list(tys);
                for (i, (idn, ty)) in idns.iter().zip(tys.iter()).enumerate() {
                    let op = ExpOperand::ExpLocal(self.curr_nest, ExpLocal::from(i));
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

                self.curr_nest -= 1;
                ExpLine {
                    ty: self.tcx.tcx.types.bool_,
                    kind: ExpLineKind::Quantifier(*kind, tys, triggers, body),
                }
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
                ExpLine { ty, kind }
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
                    Or | Implies => unreachable!(),
                    And => {
                        let false_ = Box::new(ExpKind::Const(ConstKind::Bool(false)));
                        let tys = [self.tcx.tcx.types.bool_].into();
                        self.mk_ternary(lhs, rhs, &false_, &tys)
                    },
                    op => {
                        let (eq, lhs_tys, rhs_tys) = self.possible_bin_op(op);
                        let mut lhs = self.translate(lhs, lhs_tys);
                        let mut rhs = self.translate(rhs, rhs_tys);
                        if eq {
                            (lhs, rhs) = self.equate_tys(lhs, rhs);
                        }
                        self.apply_bin_op(op, lhs, rhs)
                    }
                }
            }
            ExpKind::Ternary(c, t, e) => self.mk_ternary(c, t, e, &tys),
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

    fn mk_ternary(&mut self, c: &crate::parse::Exp, t: &crate::parse::Exp, e: &crate::parse::Exp, tys: &ExpectedTys<'tcx>) -> ExpLine<'tcx> {
        let c = self.translate(c, [self.tcx.tcx.types.bool_]);
        let mut t = self.translate_nest(t, tys.clone());
        let mut e = self.translate_nest(e, tys.clone());
        self.equate_result_tys(&mut t, &mut e);
        ExpLine {
            ty: t.result_ty(),
            kind: ExpLineKind::Ternary(c.0, t, e),
        }
    }

    fn mk_heap_update(&mut self, op: HeapUpdateOp, acc: &AccExp) -> ExpOperand<'tcx> {
        assert_eq!(op, HeapUpdateOp::Unfold, "other heap ops not yet implemented");
        let heap = self.heap.expect("heap update without heap");

        let ty = self.tcx.any_resource_id();
        let loc = self.translate(&acc.acc.loc, [ty]);
        let perm = self.translate(&acc.perm, [self.tcx.tcx.types.real_]);
        let kind = ExpLineKind::HeapUpdate(op, heap, loc.0, perm.0);
        let line = ExpLine { ty: self.tcx.tcx.types.heap_, kind };
        self.new_line(line).0
    }

    fn translate_nest(&mut self, exp: &crate::parse::Exp, tys: impl Into<ExpectedTys<'tcx>>) -> Exp<'tcx> {
        let above = core::mem::take(&mut self.added_at_level);
        self.curr_nest += 1;
        let e = self.translate_full(exp, tys);
        self.curr_nest -= 1;
        let added = core::mem::replace(&mut self.added_at_level, above);
        for added in added {
            let rm = self.evaluated.swap_remove(&added);
            assert!(rm.is_some_and(|(lvl, ..)| lvl == self.curr_nest + 1));
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
        ExpLine { ty, kind: ExpLineKind::BinOp(op, lhs.0, rhs.0) }
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
                let kind = ExpLineKind::Heap(HeapOp::Deref, heap, e.0);
                let TyKind::ResourceId(ty) = *e.1.kind() else {
                    unreachable!();
                };
                return ExpLine { ty, kind }
            }
            Perm => {
                let heap = self.heap.expect("heap perm without heap");
                let kind = ExpLineKind::Heap(HeapOp::Perm, heap, e.0);
                return ExpLine { ty: self.tcx.tcx.types.real_, kind }
            }
        };
        ExpLine { ty, kind: ExpLineKind::UnOp(op, e.0) }
    }

    fn equate_tys(&mut self, lhs: Operand<'tcx>, rhs: Operand<'tcx>) -> (Operand<'tcx>, Operand<'tcx>) {
        // TODO: go from `int` type to `ref` type here if required
        assert_eq!(lhs.1, rhs.1);
        (lhs, rhs)
    }

    fn equate_result_tys(&self, lhs: &mut Exp<'tcx>, rhs: &mut Exp<'tcx>) {
        // TODO: go from `int` type to `ref` type here if required
        assert_eq!(lhs.result_ty(), rhs.result_ty());
    }

    pub(super) fn new_line(&mut self, line: ExpLine<'tcx>) -> Operand<'tcx> {
        let ty = line.ty;
        let op = self.optimise_line(line).unwrap_or_else(|line| {
            let (n, l, prev_ty) = self.evaluated.entry(line.kind).or_insert_with_key(|kind| {
                self.added_at_level.push(kind.clone());
                let l = self.e.lines.push_and_get_key(ExpLine { ty, kind: kind.clone() });
                (self.curr_nest, l, ty)
            });
            assert_eq!(*prev_ty, ty);
            ExpOperand::ExpLocal(*n, *l)
        });
        (op, ty)
    }

    pub(super) fn inline_exp(&mut self, mut exp: Exp<'tcx>) -> ExpOperand<'tcx> {
        if let Some(o) = exp.as_operand() {
            return o;
        }
        exp.walk_operands(&mut |op| self.inline_operand(op));
        let op = exp.lines.last().unwrap().kind.as_use();
        if op.is_some() {
            exp.lines.pop();
        }
        for line in exp.lines.into_iter() {
            let curr_line = self.e.lines.push_and_get_key(line.clone());
            let old = self.evaluated.insert(line.kind.clone(), (self.curr_nest, curr_line, line.ty));
            assert!(old.is_none(), "duplicate line");
            self.added_at_level.push(line.kind);
        }
        op.unwrap_or_else(|| {
            let k = self.e.lines.last_key().unwrap();
            ExpOperand::ExpLocal(self.curr_nest, k)
        })
    }

    fn inline_operand(&self, operand: &mut ExpOperand<'tcx>) {
        let ExpOperand::ExpLocal(n, l) = operand else {
            return;
        };
        if *n <= self.curr_nest {
            return;
        }
        *n -= 1;
        if *n == self.curr_nest {
            *l = ExpLocal::from(self.e.lines.len() + usize::from(*l))
        }
    }
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
