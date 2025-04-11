use crate::{parse::HeapExp, program::{exp::Exp, resource::{Resource, ResourceExp}, Ty}};

use super::TranslationCtxt;

impl<'tcx> TranslationCtxt<'_, 'tcx> {
    pub(crate) fn translate_resource(&mut self, h: &HeapExp) -> ResourceExp<'tcx> {
        let mut r = ResourceExp::default();
        r.resources = h.res.iter().map(|r| {
            let mut false_cond = None;
            let mut cond = r.cond.iter().filter_map(|c| {
                if false_cond.is_some() {
                    return None;
                }
                let e = self.translate_exp_framed(c, self.tcx.types.bool_);
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
            let loc = self.translate_exp_framed(&r.acc.acc.loc, ty);
            let perm = self.translate_exp_framed(&r.acc.perm, self.tcx.types.real_);
            Resource { cond, loc, perm }
        }).collect();

        r.pure = self.translate_exp_framed(&h.exp, self.tcx.types.bool_);
        r
    }

    fn translate_exp_framed(&mut self, exp: &crate::parse::Exp, ty: Ty<'tcx>) -> Exp<'tcx> {
        self.translate_exp_inner(exp, ty, Some(false))
    }
}
