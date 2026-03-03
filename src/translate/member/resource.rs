use crate::{
    parse::{ConstHeapKind, ConstKind, HeapExp},
    vmir::{middle::*, DefId},
};

use super::TranslationCtxt;

impl<'tcx> TranslationCtxt<'_, 'tcx> {
    pub(crate) fn translate_resource(
        &mut self,
        h: &HeapExp,
        heap: Option<OperandKind<'tcx>>,
    ) -> ResourceExp<'tcx> {
        let heap = heap.unwrap_or_else(|| {
            OperandKind::Const(
                self.tcx
                    .interner
                    .mk_const(ConstKind::Heap(ConstHeapKind::SelfFraming)),
            )
        });
        let mut et = self.prepare_translator(Some(heap));
        let resources = h
            .res
            .iter()
            .map(|r| {
                for (neg, cond) in &r.cond {
                    et.translate_cond(*neg, cond);
                }
                if let crate::parse::ExpKind::MagicWand(..) = &*r.acc.acc.loc {
                    // TODO:
                    return Resource {
                        after_line: et.curr_line().unwrap_or(ExpLocal::ZERO),
                        cond: Default::default(),
                        loc: self.tcx.const_operand(ConstKind::bool(true)).into(),
                        perm: self.tcx.const_operand(ConstKind::write()).into(),
                    };
                }
                let loc = et.translate_chain_any_address(&r.acc.acc.loc);
                let perm = et.translate_chain(&r.acc.perm, self.tcx.types.real_);
                let (after_line, cond) = et.finish_chain();
                Resource {
                    after_line,
                    cond,
                    loc,
                    perm,
                }
            })
            .collect();

        // eprintln!("translating exp {h:?}");
        let pure = et.translate_final(&h.exp, Some(self.tcx.types.bool_));
        ResourceExp {
            resources,
            pure,
        }
    }

    /// Translate the resource represented by a `new(...)`
    pub(crate) fn translate_resource_for_new(
        &self,
        tmp: Operand<'tcx>,
        fields: Vec<FunctionId>,
    ) -> ResourceExp<'tcx> {
        assert_eq!(tmp.ty, self.tcx.types.ref_);
        let mut et = self.prepare_translator(None);
        let resources = fields
            .into_iter()
            .map(|f| {
                let ty = self.tcx.members.functions[f].sig().ret().no_bound_vars().unwrap();
                let loc = et.add_line(ty, ExpLineKind::Call(f, vec![tmp.into()]));
                let perm = OperandKind::Const(self.tcx.interner.mk_const(ConstKind::write()));
                let perm = Operand {
                    ty: self.tcx.types.real_,
                    kind: perm,
                };
                Resource {
                    after_line: et.curr_line().unwrap(),
                    cond: Default::default(),
                    loc,
                    perm: perm.into(),
                }
            })
            .collect();
        let pure = et.translate_final_true();
        ResourceExp {
            resources,
            pure,
        }
    }
}
