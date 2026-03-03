use core::fmt;

use crate::{
    vmir::ty::{Ty, TyWalker},
    TiVec,
};

use super::{
    exp::{Exp, ExpConds, ExpLineWalker, ExpOperand},
    idx::*,
    newline,
};

#[derive(Debug, Clone)]
pub struct ResourceExp<'tcx> {
    pub resources: TiVec<CompoundIdx, Resource<'tcx>>,
    pub pure: Exp<'tcx>,
}

impl<'tcx> ResourceExp<'tcx> {
    pub fn pure<O>(pure: O) -> Self
    where
        ExpOperand<'tcx>: From<O>,
    {
        Self {
            resources: Default::default(),
            pure: Exp::new_use(pure),
        }
    }

    pub fn pure_exp(pure: Exp<'tcx>) -> Self {
        Self {
            resources: Default::default(),
            pure,
        }
    }

    pub fn walk<'a>(&'a self) -> ExpLineWalker<'a, 'tcx> {
        let mut walker = ExpLineWalker::default();
        walker.add_exp(&self.pure);
        walker
    }

    // pub fn walk_locals(&self) -> TyWalker<'tcx> {
    //     let mut walker = TyWalker::default();
    //     let Some(locals) = self.locals.as_ref() else {
    //         return walker;
    //     };
    //     for &ty in locals.iter().rev() {
    //         walker.add_ty(ty);
    //     }
    //     walker
    // }
}

#[derive(Debug, Clone)]
pub struct Resource<'tcx> {
    pub after_line: ExpLocal,
    pub cond: ExpConds<'tcx>,
    pub loc: ExpOperand<'tcx>,
    pub perm: ExpOperand<'tcx>,
}

impl<'tcx> Resource<'tcx> {
    pub fn deref_ty(&self) -> Ty<'tcx> {
        self.loc.ty.deref()
    }
}

// fmt

impl fmt::Display for ResourceExp<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut last_line = ExpLocal::ZERO;
        for (_, line) in self.resources.iter_enumerated() {
            let to_line = ExpLocal::from(usize::from(line.after_line) + 1);
            self.pure.fmt_range(f, last_line..to_line)?;
            if last_line != to_line {
                last_line = to_line;
                newline(f)?;
            }
            line.fmt(f)?;
            newline(f)?;
        }
        self.pure.fmt_range(f, last_line..)
    }
}

impl fmt::Display for Resource<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "🆕 ")?;
        self.cond.fmt(f)?;
        write!(f, "acc({}, {})", self.loc, self.perm)
    }
}
