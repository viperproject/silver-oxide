use core::fmt;

use crate::vmir::{middle::{AdtId, FunctionId, MethodId, ResourceId}, ty::TyCtxt, Symbol};

pub use crate::vmir::middle::def_id::LocalDefId;

#[derive(Clone, Copy)]
pub struct Def<'tcx> {
    pub symbol: Symbol<'tcx>,
    pub kind: DefKind,
}

#[derive(Clone, Copy)]
pub enum DefKind {
    Domain,
    Adt,
    Resource,
    Function,
    Method,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DefId {
    /// Is this a reference to a Viper builtin?
    builtin: bool,
    /// The index of the member in the program.
    index: LocalDefId,
}

impl DefId {
    pub fn is_local(self) -> bool {
        !self.builtin
    }

    pub fn as_local(self) -> Option<LocalDefId> {
        self.is_local().then_some(self.index)
    }

    pub fn expect_local(self) -> LocalDefId {
        self.as_local().expect("Expected a local def id")
    }

    pub(crate) const fn builtin(index: usize) -> Self {
        Self {
            builtin: true,
            index: LocalDefId::mk(index),
        }
    }
}

impl From<LocalDefId> for DefId {
    fn from(index: LocalDefId) -> Self {
        Self {
            builtin: false,
            index,
        }
    }
}

impl From<&LocalDefId> for DefId {
    fn from(id: &LocalDefId) -> Self {
        Self::from(*id)
    }
}

// fmt

impl fmt::Display for DefId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let ctx = unsafe { TyCtxt::global_ref_unchecked() };
        let def = ctx.def_data(*self);
        write!(f, "{}", def.symbol)
    }
}

impl fmt::Display for AdtId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let ctx = unsafe { TyCtxt::global_ref_unchecked() };
        let id = ctx.interner.get_adt_def(*self).data().id;
        drop(ctx);
        id.fmt(f)
    }
}

impl fmt::Display for LocalDefId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        DefId::from(*self).fmt(f)
    }
}

fn display_id(f: &mut fmt::Formatter<'_>, id: impl FnOnce(&TyCtxt) -> LocalDefId) -> fmt::Result {
    let ctx = unsafe { TyCtxt::global_ref_unchecked() };
    let id = id(&*ctx);
    drop(ctx);
    write!(f, "{}", id)
}

impl fmt::Display for ResourceId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        display_id(f, |ctx| ctx.members.resources[*self].id)
    }
}

impl fmt::Display for FunctionId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        display_id(f, |ctx| ctx.members.functions[*self].id)
    }
}

impl fmt::Display for MethodId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        display_id(f, |ctx| ctx.members.methods[*self].id)
    }
}
