use crate::{parse::StmtBlock, program::{body::Body, ArgRef}};

use super::{cfg::Cfg, TranslationCtxt};

impl<'tcx> TranslationCtxt<'_, 'tcx> {
    pub(crate) fn translate_body(mut self, body: &StmtBlock) -> Body<'tcx> {
        self.add_body(body);
        let cfg = Cfg::new(&self.tcx, self.goto_labels.iter().copied(), body);

        let name = self.tcx.item_name(self.id).unwrap();
        cfg.dump_dot(&format!("log/cfg/{name}.dot"));

        let mut result = Body::default();
        let heap = self.params[&ArgRef::Heap(None)];

        let preorder = cfg.preorder();
    
        // TODO
        result
    }
}
