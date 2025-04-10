use crate::{program::DefId, TiVec};

use super::{exp::Exp, idx::*};

#[derive(Debug, Default)]
pub struct ResourceExp<'tcx> {
    pub resources: TiVec<CompoundIdx, Resource<'tcx>>,
    pub pure: Exp<'tcx>,
}

#[derive(Debug)]
pub struct Resource<'tcx> {
    pub resource: DefId,
    pub args: Vec<Exp<'tcx>>,
    pub perm: Exp<'tcx>,
}
