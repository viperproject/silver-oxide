mod ty;
mod resolve;
mod id;

pub use ty::*;
pub use resolve::*;
pub use id::*;

use crate::{parse::{MemberId, Program}, vmir::{ty::TyCtxt, StdId}};

pub struct GlobalT<'a, 'tcx>(IdT<'a, 'tcx>);

/// DefId equivalent before interning and such
type AnyId<'tcx> = Result<MemberId, StdId<'tcx>>;

impl<'tcx> TyCtxt<'tcx> {
    pub fn prepare_translate(&mut self, program: &Program) -> GlobalT<'_, 'tcx> {
        let resolved = ResolveT::new(self, program);
        let ty = TypeT::new(resolved, program);
        GlobalT(IdT::new(ty, program))
    }
}

impl<'a, 'tcx> core::ops::Deref for GlobalT<'a, 'tcx> {
    type Target = IdT<'a, 'tcx>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'tcx> core::ops::DerefMut for GlobalT<'_, 'tcx> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
