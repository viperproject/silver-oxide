use crate::{parse::{ConstHeapKind, ConstKind, HeapExp}, program::{exp::{Exp, ExpOperand}, resource::{Resource, ResourceExp}, Local, Ty}};

use super::TranslationCtxt;

impl<'tcx> TranslationCtxt<'_, 'tcx> {
    pub(crate) fn translate_resource(&mut self, h: &HeapExp, heap: Option<Local>) -> ResourceExp<'tcx> {
        let heap = heap.map(ExpOperand::Local).unwrap_or_else(|| {
            ExpOperand::Const(self.tcx.interner.mk_const(ConstKind::Heap(ConstHeapKind::SelfFraming)))
        });
        let mut r = ResourceExp::default();
        r.resources = h.res.iter().map(|r| {
            let mut false_cond = None;
            let mut cond = r.cond.iter().filter_map(|c| {
                if false_cond.is_some() {
                    return None;
                }
                let e = self.translate_exp_framed(c, self.tcx.types.bool_, heap);
                if let Some(c) = e.as_const() {
                    false_cond = (!c.as_bool().unwrap()).then_some(e);
                    return None;
                }
                Some(e)
            }).collect();
            if let Some(e) = false_cond {
                cond = vec![e];
            }
            let ty = self.any_resource_id();
            let loc = self.translate_exp_framed(&r.acc.acc.loc, ty, heap);
            let perm = self.translate_exp_framed(&r.acc.perm, self.tcx.types.real_, heap);
            Resource { cond, loc, perm }
        }).collect();

        r.pure = self.translate_exp_framed(&h.exp, self.tcx.types.bool_, heap);
        r
    }

    fn translate_exp_framed(&mut self, exp: &crate::parse::Exp, ty: Ty<'tcx>, heap: ExpOperand<'tcx>) -> Exp<'tcx> {
        self.translate_exp_inner(exp, ty, Some(heap))
    }
}
