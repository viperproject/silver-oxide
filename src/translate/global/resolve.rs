use core::ops::{Deref, DerefMut};

use crate::{parse::{Declaration, Ident, MemberId, Program}, translate::global::AnyId, vmir::{ty::TyCtxt, Std, Symbol}, HashMap};

pub struct ResolveT<'a, 'tcx> {
    inner: &'a mut TyCtxt<'tcx>,
    resolved: HashMap<Symbol<'tcx>, MemberId>,
    adt_constructors: HashMap<String, MemberId>,
    adt_destructors: HashMap<Symbol<'tcx>, (MemberId, usize)>,
}

impl<'a, 'tcx> ResolveT<'a, 'tcx> {
    pub fn new(tcx: &'a mut TyCtxt<'tcx>, program: &Program) -> Self {
        let mut adt_constructors = HashMap::new();
        let mut adt_destructors = HashMap::new();
        let resolved = program.iter().filter_map(|(id, decl)| {
            if let Declaration::AdtConstructor(c) = decl {
                adt_constructors.insert(c.signature.name.0.0.clone(), id);
                adt_destructors.extend(c.destructors().enumerate().map(|(idx, des)| {
                    (tcx.interner.mk_symbol(&des.idn.0), (id, idx))
                }));
            }
            decl.idn_decl().map(|decl| {
                (tcx.interner.mk_symbol(&decl.0), id)
            })
        }).collect();
        Self {
            inner: tcx,
            resolved,
            adt_constructors,
            adt_destructors,
        }
    }

    pub fn resolve(&self, ident: &Ident) -> Option<AnyId<'tcx>> {
        let ident = self.interner.mk_symbol(ident);
        self.resolve_get(ident)
    }

    pub fn resolve_get(&self, ident: Symbol<'tcx>) -> Option<AnyId<'tcx>> {
        if let Some(local) = self.resolved.get(&ident) {
            Some(Ok(*local))
        } else if let Some(std_id) = Std::resolve(ident) {
            Some(Err(std_id))
        } else {
            None
        }
    }

    pub fn resolve_adt_cons(&self, name: &str) -> Option<MemberId> {
        self.adt_constructors.get(name).copied()
    }

    pub fn resolve_adt_destr(&self, ident: &Ident) -> Option<(MemberId, usize)> {
        self.adt_destructors.get(&self.interner.mk_symbol(ident)).copied()
    }
}

impl<'tcx> Deref for ResolveT<'_, 'tcx> {
    type Target = TyCtxt<'tcx>;
    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<'tcx> DerefMut for ResolveT<'_, 'tcx> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}
