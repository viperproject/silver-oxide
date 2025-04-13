use crate::{parse::{ConstHeapKind, ConstKind, HeapExp}, program::{exp::{Exp, ExpLineKind, ExpOperand}, resource::{Resource, ResourceExp}, DefId, ExpLocal}};

use super::TranslationCtxt;

impl<'tcx> TranslationCtxt<'_, 'tcx> {
    pub(crate) fn translate_resource(&mut self, h: &HeapExp, heap: Option<ExpOperand<'tcx>>) -> ResourceExp<'tcx> {
        let heap = heap.unwrap_or_else(|| {
            ExpOperand::Const(self.tcx.interner.mk_const(ConstKind::Heap(ConstHeapKind::SelfFraming)))
        });
        let mut r = ResourceExp::default();
        r.resources = h.res.iter().map(|r| {
            let mut et = self.prepare_translator(Some(heap));
            for (neg, cond) in &r.cond {
                et.translate_cond(*neg, cond);
            }
            let ty = self.any_resource_id();
            let loc = et.translate_chain(&r.acc.acc.loc, ty);
            let perm = et.translate_chain(&r.acc.perm, self.tcx.types.real_);
            let (exp, cond) = et.finish_chain();
            Resource { exp, cond, loc, perm }
        }).collect();

        r.pure = self.translate_exp_inner(&h.exp, self.tcx.types.bool_, Some(heap));
        r
    }

    /// Translate the resource represented by a `new(...)`
    pub(crate) fn translate_resource_for_new(&self, tmp: ExpOperand<'tcx>, fields: Vec<DefId>) -> ResourceExp<'tcx> {
        let mut r = ResourceExp::default();
        r.resources = fields.into_iter().map(|f| {
            let ty = self.tcx.fn_sig(f).unwrap().returns().1;
            assert_eq!(ty.len(), 1);
            let exp = Exp::new_simple(ty[0], ExpLineKind::Call(f, vec![tmp]));
            let perm = ExpOperand::Const(self.tcx.interner.mk_const(ConstKind::write()));
            Resource { exp, cond: Some(Default::default()), loc: ExpOperand::ExpLocal(0, ExpLocal::ZERO), perm }
        }).collect();
        r.pure = Exp::new_use(ExpOperand::Const(self.tcx.interner.mk_const(ConstKind::Bool(true))), self.tcx.types.bool_);
        r
    }
}
