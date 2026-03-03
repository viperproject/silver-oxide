use crate::{vmir::{middle::{AdtId, FieldIdx, VariantIdx}, ty::{ParamTy, TypeFolder}, DefId, Symbol}, TiVec};

use super::{ArgFolder, Interned, Ty, TyCtxt, TyList};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct AdtDef<'tcx>(pub(crate) Interned<'tcx, AdtDefData<'tcx>>);

impl<'tcx> AdtDef<'tcx> {
    pub fn data(self) -> &'tcx AdtDefData<'tcx> {
        self.0 .0
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct AdtDefData<'tcx> {
    pub id: DefId,
    pub params: Box<[ParamTy<'tcx>]>,
    pub variants: TiVec<VariantIdx, VariantDef<'tcx>>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct VariantDef<'tcx> {
    pub name: Option<Symbol<'tcx>>,
    pub fields: TiVec<FieldIdx, FieldDef<'tcx>>,
}

impl<'tcx> VariantDef<'tcx> {
    /// Returns the unsubstituted types of the fields in this variant.
    pub fn identity(&self) -> Vec<Ty<'tcx>> {
        self.fields.iter().map(|f| f.ty).collect()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FieldDef<'tcx> {
    pub name: Option<Symbol<'tcx>>,
    ty: Ty<'tcx>,
}

impl<'tcx> FieldDef<'tcx> {
    pub fn new(name: Option<Symbol<'tcx>>, ty: Ty<'tcx>) -> Self {
        Self { name, ty }
    }

    pub fn ty(self, tcx: &TyCtxt<'tcx>, args: TyList<'tcx>) -> Ty<'tcx> {
        ArgFolder::new(&tcx.interner, args).fold_ty(self.ty)
    }
}
