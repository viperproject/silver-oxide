use crate::{
    parse::{ConstHeapKind, ConstKind, HeapExp},
    program::{
        exp::{Exp, ExpLineKind, ExpOperand, ExpOperandKind},
        resource::{Resource, ResourceExp},
        DefId,
    },
};

use super::TranslationCtxt;

impl<'tcx> TranslationCtxt<'_, 'tcx> {
    pub(crate) fn translate_resource(
        &mut self,
        h: &HeapExp,
        heap: Option<ExpOperandKind<'tcx>>,
    ) -> ResourceExp<'tcx> {
        let locals = heap
            .is_none()
            .then(|| self.locals.clone())
            .unwrap_or_default();
        let heap = heap.unwrap_or_else(|| {
            ExpOperandKind::Const(
                self.tcx
                    .interner
                    .mk_const(ConstKind::Heap(ConstHeapKind::SelfFraming)),
            )
        });
        let mut r = ResourceExp::default();
        r.locals = locals;
        r.resources = h
            .res
            .iter()
            .map(|r| {
                let mut et = self.prepare_translator(Some(heap));
                for (neg, cond) in &r.cond {
                    et.translate_cond(*neg, cond);
                }
                let ty = self.any_resource_id();
                let loc = et.translate_chain(&r.acc.acc.loc, ty);
                let perm = et.translate_chain(&r.acc.perm, self.tcx.types.real_);
                let (exp, cond) = et.finish_chain();
                Resource {
                    exp,
                    cond,
                    loc,
                    perm,
                }
            })
            .collect();

        r.pure = self.translate_exp_inner(&h.exp, self.tcx.types.bool_, Some(heap));
        r
    }

    /// Translate the resource represented by a `new(...)`
    pub(crate) fn translate_resource_for_new(
        &self,
        tmp: ExpOperand<'tcx>,
        fields: Vec<DefId>,
    ) -> ResourceExp<'tcx> {
        assert_eq!(tmp.ty, self.tcx.types.ref_);
        let resources = fields
            .into_iter()
            .map(|f| {
                let ty = self.tcx.fn_sig(f).unwrap().returns().1;
                assert_eq!(ty.len(), 1);
                let exp = Exp::new_simple(ty[0], ExpLineKind::Call(f, vec![tmp]));
                let loc = exp.result();
                let perm = ExpOperandKind::Const(self.tcx.interner.mk_const(ConstKind::write()));
                let perm = ExpOperand {
                    ty: self.tcx.types.real_,
                    kind: perm,
                };
                Resource {
                    exp,
                    cond: Some(Default::default()),
                    loc,
                    perm,
                }
            })
            .collect();
        let pure = Exp::new_use(
            ExpOperandKind::Const(self.tcx.interner.mk_const(ConstKind::Bool(true))),
            self.tcx.types.bool_,
        );
        ResourceExp {
            resources,
            pure,
            ..Default::default()
        }
    }
}
