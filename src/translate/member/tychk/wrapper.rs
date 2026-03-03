use std::marker::PhantomData;

use crate::vmir::{middle::{ExpConds, ExpLine, ExpLineKind, ExpOperand, ExpOperandKind}, ty::Ty};


pub(crate) type ExpOperandIfx<'tcx> = Ifx<'tcx, ExpOperand<'tcx>>;
pub(crate) type ExpLineIfx<'tcx> = Ifx<'tcx, ExpLine<'tcx>>;
pub(crate) type TyIfx<'tcx> = Ifx<'tcx, Ty<'tcx>>;

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct Ifx<'tcx, T: HasType<'tcx>>(T, PhantomData<Ty<'tcx>>);

impl<'tcx, T: HasType<'tcx>> Ifx<'tcx, T> {
    fn wrap(inner: T) -> Self {
        Self(inner, PhantomData)
    }

    pub fn ty(&self) -> TyIfx<'tcx> {
        TyIfx::wrap(self.0.ty())
    }

    pub(super) unsafe fn get(&self) -> &T {
        &self.0
    }
}

impl<'tcx> ExpOperandIfx<'tcx> {
    pub fn new<O: Into<ExpOperandKind<'tcx>>>(ty: TyIfx<'tcx>, kind: O) -> Self {
        Self::wrap(ExpOperand { ty: ty.0, kind: kind.into() })
    }
}

impl<'tcx> ExpLineIfx<'tcx> {
    pub fn kind_mut(&mut self) -> &mut ExpLineKind<'tcx> {
        &mut self.0.kind
    }

    pub fn cond_mut(&mut self) -> &mut ExpConds<'tcx> {
        &mut self.0.cond
    }
}

impl<'tcx, T: HasType<'tcx>> From<T> for Ifx<'tcx, T> {
    fn from(value: T) -> Self {
        Self::wrap(value)
    }
}

trait HasType<'tcx> {
    fn ty(&self) -> Ty<'tcx>;
}

impl<'tcx> HasType<'tcx> for ExpOperand<'tcx> {
    fn ty(&self) -> Ty<'tcx> {
        self.ty
    }
}

impl<'tcx> HasType<'tcx> for ExpLine<'tcx> {
    fn ty(&self) -> Ty<'tcx> {
        self.ty
    }
}

impl<'tcx> HasType<'tcx> for Ty<'tcx> {
    fn ty(&self) -> Ty<'tcx> {
        *self
    }
}
