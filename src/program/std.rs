use crate::parse::Ident;

use super::{DeclSig, DefId, FnSig, Interner, MemberData, MemberKind, Symbol};

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

    pub(super) fn data(&self, id: DefId) -> MemberData<'tcx> {
        match id {
            DefId::SEQ_CONS => MemberData {
                kind: MemberKind::DomainFunction,
                domain: Some(DefId::SEQ_DOMAIN),
                sig: Some(DeclSig {
                    name: self.seq,
                    args: None,
                    rets: 1,
                }),
            },
            DefId::SET_CONS => MemberData {
                kind: MemberKind::DomainFunction,
                domain: Some(DefId::SET_DOMAIN),
                sig: Some(DeclSig {
                    name: self.set,
                    args: None,
                    rets: 1,
                }),
            },
            DefId::MULTISET_CONS => MemberData {
                kind: MemberKind::DomainFunction,
                domain: Some(DefId::MULTISET_DOMAIN),
                sig: Some(DeclSig {
                    name: self.multiset,
                    args: None,
                    rets: 1,
                }),
            },
            DefId::MAP_CONS => MemberData {
                kind: MemberKind::DomainFunction,
                domain: Some(DefId::MAP_DOMAIN),
                sig: Some(DeclSig {
                    name: self.map,
                    args: Some(0),
                    rets: 1,
                }),
            },
            _ => panic!("unexpected defid: {id:?}"),
        }
    }

    pub(super) fn fn_sig(&self, id: DefId) -> Option<&FnSig<'tcx>> {
        match id {
            DefId::SEQ_CONS => todo!(),
            DefId::SET_CONS => todo!(),
            DefId::MULTISET_CONS => todo!(),
            DefId::MAP_CONS => todo!(),
            _ => None,
        }
    }

    pub(super) fn global_ref(&self, symbol: Symbol<'tcx>) -> Option<DefId> {
        if symbol == self.seq {
            Some(DefId::SEQ_CONS)
        } else if symbol == self.set {
            Some(DefId::SET_CONS)
        } else if symbol == self.multiset {
            Some(DefId::MULTISET_CONS)
        } else if symbol == self.map {
            Some(DefId::MAP_CONS)
        } else {
            None
        }
    }
}

impl DefId {
    pub const SEQ_DOMAIN: DefId = DefId::builtin(0);
    pub const SEQ_CONS: DefId = DefId::builtin(1);

    pub const SET_DOMAIN: DefId = DefId::builtin(2);
    pub const SET_CONS: DefId = DefId::builtin(3);

    pub const MULTISET_DOMAIN: DefId = DefId::builtin(4);
    pub const MULTISET_CONS: DefId = DefId::builtin(5);

    pub const MAP_DOMAIN: DefId = DefId::builtin(6);
    pub const MAP_CONS: DefId = DefId::builtin(7);
}
