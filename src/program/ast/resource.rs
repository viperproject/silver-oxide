use core::fmt;

use crate::TiVec;

use super::{exp::Exp, idx::*, newline};

#[derive(Default)]
pub struct ResourceExp<'tcx> {
    pub resources: TiVec<CompoundIdx, Resource<'tcx>>,
    pub pure: Exp<'tcx>,
}

pub struct Resource<'tcx> {
    pub cond: Vec<Exp<'tcx>>,
    pub loc: Exp<'tcx>,
    pub perm: Exp<'tcx>,
}

// fmt

impl fmt::Debug for ResourceExp<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (_, line) in self.resources.iter_enumerated() {
            line.fmt(f)?;
            newline(f)?;
        }
        let indent = f.width().unwrap_or_default() + 1;
        write!(f, "! {:indent$?}", self.pure)
    }
}

impl fmt::Debug for Resource<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let indent = f.width().unwrap_or_default() + 1;
        for cond in self.cond.iter() {
            write!(f, "? {cond:indent$?}")?;
            newline(f)?;
        }
        write!(f, "> {:indent$?}", self.loc)?;
        newline(f)?;
        writeln!(f, "+ {:indent$?}", self.perm)
    }
}
