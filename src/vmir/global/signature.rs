use std::ops::Deref;

use crate::{vmir::{middle::Local, ty::{ArgFolder, Interned, Ty, TyCtxt, TyList, TypeFolder}}, TiSlice};


#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Locals<'tcx>(Interned<'tcx, TiSlice<Local, Ty<'tcx>>>);

impl<'tcx> Locals<'tcx> {
    pub(crate) fn new(i: TyList<'tcx>) -> Self {
        let i = &*i as *const [Ty<'tcx>] as *const TiSlice<Local, Ty<'tcx>>;
        let i = unsafe { &*i };
        Self(Interned(i))
    }

    pub(crate) fn deref(self) -> &'tcx TiSlice<Local, Ty<'tcx>> {
        self.0 .0
    }
}

impl<'tcx> Deref for Locals<'tcx> {
    type Target = TiSlice<Local, Ty<'tcx>>;
    fn deref(&self) -> &Self::Target {
        self.0 .0
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Binder<T> {
    value: T,
    ty_params: usize,
}

impl<T> Binder<T> {
    pub fn new(value: T, ty_params: usize) -> Self {
        Self { value, ty_params }
    }

    pub fn instantiate_identity(&self) -> &T {
        &self.value
    }
}

impl<'tcx> Binder<Ty<'tcx>> {
    pub fn no_bound_vars(self) -> Option<Ty<'tcx>> {
        let no_bound_vars = self.value.walk().all(|ty| ty.param().is_none());
        no_bound_vars.then_some(self.value)
    }

    pub fn instantiate(self, tcx: &TyCtxt<'tcx>, tys: TyList<'tcx>) -> Ty<'tcx> {
        assert_eq!(self.ty_params, tys.len());
        let mut folder = ArgFolder::new(&tcx.interner, tys);
        folder.fold_ty(self.value)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct FnSig<'tcx> {
    pub locals: Locals<'tcx>,
    pub ty_params: usize,
    pub heapless: bool
}

impl<'tcx> FnSig<'tcx> {
    fn params_no_snap(self) -> &'tcx TiSlice<Local, Ty<'tcx>> {
        if self.heapless {
            self.locals.deref()
        } else {
            let end = Local::from(self.locals.len() - 1);
            &self.locals.deref()[..end]
        }
    }

    pub fn params(self) -> Binder<&'tcx [Ty<'tcx>]> {
        Binder::new(&self.params_no_snap().raw[1..], self.ty_params)
    }

    pub fn params_iter(self) -> impl Iterator<Item = (Local, Binder<Ty<'tcx>>)> {
        self.params_no_snap().iter_enumerated().skip(1).map(move |(i, ty)| (i, Binder::new(*ty, self.ty_params)))
    }

    pub fn ret(self) -> Binder<Ty<'tcx>> {
        Binder::new(self.locals[Local::ZERO], self.ty_params)
    }

    pub fn heapless(self) -> bool {
        self.heapless
    }

    pub fn ty_params(self) -> usize {
        self.ty_params
    }
}

#[derive(Debug, Clone, Copy)]
pub struct MethodSig<'tcx> {
    pub locals: Locals<'tcx>,
    pub params: u32,
    pub ty_params: usize,
}

impl<'tcx> MethodSig<'tcx> {
    pub fn params(self) -> &'tcx TiSlice<Local, Ty<'tcx>> {
        &self.locals.deref()[..Local::from(self.params as usize)]
    }

    pub fn rets(&self) -> &'tcx [Ty<'tcx>] {
        &self.locals.deref().raw[self.params as usize..]
    }

    pub fn rets_iter(self) -> impl Iterator<Item = (Local, Ty<'tcx>)> {
        self.rets().iter().enumerate().map(move |(i, ty)| (Local::from(i + self.params as usize), *ty))
    }
}
