use crate::{parse::HeapExp, program::resource::{Resource, ResourceExp}};

use super::TranslationCtxt;

impl<'tcx> TranslationCtxt<'_, 'tcx> {
    pub(crate) fn translate_resource(&mut self, h: &HeapExp) -> ResourceExp<'tcx> {
        assert!(self.curr_heap.is_ok());
        self.curr_heap = Ok(true);

        let mut r = ResourceExp::default();
        r.resources = h.res.iter().map(|r| {
            let cond = r.cond.iter().map(|c| self.translate_exp(c, self.tcx.types.bool_)).collect();
            let ty = self.any_resource_id();
            let loc = self.translate_exp(&r.acc.acc.loc, ty);
            let perm = self.translate_exp(&r.acc.perm, self.tcx.types.real_);
            Resource { cond, loc, perm }
        }).collect();

        r.pure = self.translate_exp(&h.exp, self.tcx.types.bool_);
        r
    }
}
