use core::fmt;

use crate::{
    program::{Ty, TyWalker},
    TiVec,
};

use super::{
    exp::{Exp, ExpCond, ExpLineWalker, ExpOperand},
    idx::*,
    newline,
};

#[derive(Default, Clone)]
pub struct ResourceExp<'tcx> {
    pub locals: TiVec<Local, Ty<'tcx>>,
    pub resources: TiVec<CompoundIdx, Resource<'tcx>>,
    pub pure: Exp<'tcx>,
}

impl<'tcx> ResourceExp<'tcx> {
    pub fn pure(pure: Exp<'tcx>) -> Self {
        Self {
            pure,
            ..Default::default()
        }
    }

    pub fn walk<'a>(&'a self) -> ExpLineWalker<'a, 'tcx> {
        let mut walker = ExpLineWalker::default();
        for resource in &self.resources {
            walker.add_exp(&resource.exp);
        }
        walker.add_exp(&self.pure);
        walker
    }

    pub fn walk_locals(&self) -> TyWalker<'tcx> {
        let mut walker = TyWalker::default();
        for &ty in self.locals.iter().rev() {
            walker.add_ty(ty);
        }
        walker
    }
}

#[derive(Clone)]
pub struct Resource<'tcx> {
    pub exp: Exp<'tcx>,
    pub cond: Option<Box<[ExpCond<'tcx>]>>,
    pub loc: ExpOperand<'tcx>,
    pub perm: ExpOperand<'tcx>,
}

// fmt

impl fmt::Debug for ResourceExp<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (_, line) in self.resources.iter_enumerated() {
            line.fmt(f)?;
            writeln!(f)?;
            newline(f)?;
        }
        let indent = f.width().unwrap_or_default() + 1;
        write!(f, "! {:indent$?}", self.pure)
    }
}

impl fmt::Debug for Resource<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let indent = f.width().unwrap_or_default() + 1;
        write!(f, "> {:indent$?}", self.exp)?;
        newline(f)?;
        match self.cond.as_deref() {
            None => {
                write!(f, "? false")?;
                newline(f)?;
            }
            Some([]) => (),
            Some(cond) => {
                write!(f, "? {cond:?}")?;
                newline(f)?;
            }
        }
        write!(f, "+ acc({:?}, {:?})", self.loc, self.perm)
    }
}
