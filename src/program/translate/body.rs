use crate::{parse::{AstWalker, Block}, program::{body::Body, idx::LocalDefId, TyCtxt}};

use super::TranslationCtxt;

impl<'tcx> TranslationCtxt<'_, 'tcx> {
    pub(crate) fn translate_body(&mut self, body: &Block) -> Body<'tcx> {
        self.add_body(body);
        // TODO:
        Default::default()
    }
}
