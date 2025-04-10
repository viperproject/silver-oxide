use crate::program::{idx::LocalDefId, resource::ResourceExp, TyCtxt};

use super::TranslationCtxt;

impl<'tcx> TranslationCtxt<'_, 'tcx> {
    pub(crate) fn translate_resource(&self, exp: &crate::parse::Exp) -> ResourceExp<'tcx> {
        // TODO:
        Default::default()
    }
}
