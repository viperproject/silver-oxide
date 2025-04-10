mod exp;
mod resource;
mod body;
mod ctx;
mod desugar;

pub(crate) use ctx::*;

use crate::parse::Ident;

use super::{DefId, TyCtxt};

impl<'tcx> TyCtxt<'tcx> {
    fn get_callee(&self, ident: &Ident) -> DefId {
        let ident = self.interner.mk_symbol(ident);
        self.global_ref(ident).unwrap()
    }
}
