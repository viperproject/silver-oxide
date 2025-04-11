use crate::{parse::StmtBlock, program::{body::{Body, Location}, BasicBlock}};

use super::TranslationCtxt;

impl<'tcx> TranslationCtxt<'_, 'tcx> {
    pub(crate) fn translate_body(&mut self, body: &StmtBlock) -> Body<'tcx> {
        self.add_body(body);
        self.curr_heap = Err(Location { block: BasicBlock::ZERO, statement: 0 });
        // TODO:
        Default::default()
    }
}
