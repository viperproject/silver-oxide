use crate::parse::Ident;

use super::{DefId, FnSig, Interner, MemberKind, Symbol};

#[derive(Debug, Clone, Copy)]
pub struct Std<'tcx> {
    seq: Symbol<'tcx>,
    set: Symbol<'tcx>,
    multiset: Symbol<'tcx>,
    map: Symbol<'tcx>,
}

impl<'tcx> Std<'tcx> {
    pub(super) fn new(i: &Interner<'tcx>) -> Self {
        Self {
            seq: i.mk_symbol(&Ident("Seq".into())),
            set: i.mk_symbol(&Ident("Set".into())),
            multiset: i.mk_symbol(&Ident("Multiset".into())),
            map: i.mk_symbol(&Ident("Map".into())),
        }
    }

    pub(super) fn kind(&self, id: DefId) -> MemberKind {
        match id {
            DefId::SEQ => MemberKind::DomainFunction,
            DefId::SET => MemberKind::DomainFunction,
            DefId::MULTISET => MemberKind::DomainFunction,
            DefId::MAP => MemberKind::DomainFunction,
            _ => panic!("unexpected defid: {id:?}"),
        }
    }

    pub(super) fn fn_sig(&self, id: DefId) -> Option<&FnSig<'tcx>> {
        match id {
            DefId::SEQ => todo!(),
            DefId::SET => todo!(),
            DefId::MULTISET => todo!(),
            DefId::MAP => todo!(),
            _ => None,
        }
    }

    pub(super) fn global_ref(&self, symbol: Symbol<'tcx>) -> Option<DefId> {
        if symbol == self.seq {
            Some(DefId::SEQ)
        } else if symbol == self.set {
            Some(DefId::SET)
        } else if symbol == self.multiset {
            Some(DefId::MULTISET)
        } else if symbol == self.map {
            Some(DefId::MAP)
        } else {
            None
        }
    }
}

impl DefId {
    pub const SEQ: DefId = DefId::builtin(0);
    pub const SET: DefId = DefId::builtin(1);
    pub const MULTISET: DefId = DefId::builtin(2);
    pub const MAP: DefId = DefId::builtin(3);
}
