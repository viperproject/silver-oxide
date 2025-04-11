use fxhash::{FxHashMap, FxHashSet};

use crate::{parse::{AccExp, BinOp, ConstKind, ExpKind, HeapUpdateOp, UnOp}, program::{exp::*, *}};

use super::TranslationCtxt;

impl<'tcx> TranslationCtxt<'_, 'tcx> {
    pub(crate) fn translate_exp(&self, exp: &crate::parse::Exp, ty: Ty<'tcx>) -> Exp<'tcx> {
        let heap = self.curr_heap.map_or_else(
            |_| todo!(),
            |heap| heap.then(|| ExpOperand::Const(self.tcx.interner.mk_const(&ConstKind::SelfFramingHeap)))
        );
        let mut et = ExpTranslator {
            e: Default::default(),
            tcx: self,
            let_bound: FxHashMap::default(),
            curr_nest: 0,
            heap,
        };
        let e = et.translate_full(exp, [ty]);
        let rt = e.result_ty();
        assert!(ty == rt || ExpectedTys::tys_match(ty, rt), "type error: expected {ty:?}, found {rt:?}");
        e
    }
}

type Operand<'tcx> = (ExpOperand<'tcx>, Ty<'tcx>);

struct ExpTranslator<'a, 'b, 'tcx> {
    e: Exp<'tcx>,
    tcx: &'a TranslationCtxt<'b, 'tcx>,
    let_bound: FxHashMap<Symbol<'tcx>, Operand<'tcx>>,
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
                let c = self.tcx.tcx.interner.mk_const(c);
                let ty = self.tcx.tcx.const_ty(c);
                tys.check_ty(ty);
                return (ExpOperand::Const(c), ty);
            }
            ExpKind::Result => return self.mk_use(ArgRef::Result, tys),
            ExpKind::Old(ident, exp_kind) => todo!(),
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
            ExpKind::Quantifier(..) => todo!(),
            ExpKind::LetIn(decl, val, e) => {
                let decl = self.tcx.tcx.interner.mk_symbol(&decl.0);
                let v = self.translate(val, None);

                let old = self.let_bound.insert(decl, v);
                assert!(old.is_none(), "duplicate let bound");

                let r = self.translate(e, tys);
                self.let_bound.remove(&decl);
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
                        let (eq, tys) = self.possible_bin_op(op);
                        let mut lhs = self.translate(lhs, tys.clone());
                        let mut rhs = self.translate(rhs, tys);
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
        self.e.lines.push(line);
        let (l, ln) = self.e.lines.last_key_value().unwrap();
        (ExpOperand::ExpLocal(self.curr_nest, l), ln.ty)
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
        let l = self.e.lines.push_and_get_key(line);
        ExpOperand::ExpLocal(self.curr_nest, l)
    }

    fn translate_nest(&mut self, exp: &crate::parse::Exp, tys: ExpectedTys<'tcx>) -> Exp<'tcx> {
        self.curr_nest += 1;
        let e = self.translate_full(exp, tys);
        self.curr_nest -= 1;
        e
    }

    fn mk_use(&self, r: ArgRef<'tcx>, tys: ExpectedTys<'tcx>) -> Operand<'tcx> {
        let local = self.tcx.get_param(r);
        let ty = self.tcx.locals[local];
        tys.check_ty(ty);
        (ExpOperand::Local(local), ty)
    }

    fn possible_bin_op(&self, op: BinOp) -> (bool, impl Into<ExpectedTys<'tcx>> + Clone) {
        // TODO:
        (true, [])
    }

    fn apply_bin_op(&mut self, op: BinOp, lhs: Operand<'tcx>, rhs: Operand<'tcx>) -> ExpLine<'tcx> {
        use BinOp::*;
        let types = &self.tcx.tcx.types;
        let ty = match op {
            Implies | Or | And => unreachable!(),
            Iff | Eq | Neq | Lt | Le | Gt | Ge | In => types.bool_,
            Plus | Minus | Mult | Div => lhs.1,
            Mod | IntDiv => types.int_,
            Union => todo!(),
            SetMinus => todo!(),
            Intersection => todo!(),
            Subset => todo!(),
            Concat => todo!(),
            MagicWand => todo!(),
            Range => todo!(),
            InhaleExhale => todo!(),
        };
        let kind = ExpLineKind::BinOp(op, lhs.0, rhs.0);
        ExpLine { ty, kind }
    }

    fn possible_un_op(&self, op: UnOp) -> impl Into<ExpectedTys<'tcx>> {
        []
    }

    fn apply_un_op(&mut self, op: UnOp, e: Operand<'tcx>) -> ExpLine<'tcx> {
        match op {
            UnOp::Deref => {
                let heap = self.heap.expect("heap deref without heap");
                let kind = ExpLineKind::Heap(HeapOp::Deref, heap, e.0);
                let TyKind::ResourceId(ty) = *e.1.kind() else {
                    unreachable!();
                };
                ExpLine { ty, kind }
            }
            UnOp::Perm => {
                let heap = self.heap.expect("heap perm without heap");
                let kind = ExpLineKind::Heap(HeapOp::Perm, heap, e.0);
                ExpLine { ty: self.tcx.tcx.types.ref_, kind }
            }
            _ => todo!(),
        }
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
}

#[derive(Debug, Default, Clone)]
pub struct ExpectedTys<'tcx> {
    tys: Option<FxHashSet<Ty<'tcx>>>,
}

impl<'tcx, I: IntoIterator<Item = Ty<'tcx>>> From<I> for ExpectedTys<'tcx> {
    fn from(tys: I) -> Self {
        let tys: FxHashSet<_> = tys.into_iter().collect();
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
            (TyKind::ResourceId(e), TyKind::ResourceId(..)) => matches!(e.kind(), TyKind::Heap),
            (TyKind::Compound(e), TyKind::Compound(ty)) => todo!(),
            (TyKind::Domain(s, e), TyKind::Domain(s_, ty)) => {
                if s != s_ {
                    return false;
                }
                todo!()
            }
            _ => true,
        }
    }
}
