use crate::parse::ArgOrType;
use crate::translate::global::{GlobalT, TypeCtxt};
use crate::vmir::middle::Local;
use crate::vmir::ty::{Ty, TyCtxt, TyKind};
use crate::vmir::{LocalDefId, Symbol};
use crate::{HashMap, HashSet};

use crate::{
    parse::{AstWalkable, AstWalker, ExpKind, Statement, StmtBlock},
    TiVec,
};

use super::LoopHead;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ArgRef<'tcx> {
    Ident(Symbol<'tcx>),
    Label(Symbol<'tcx>),
    /// The heap state framed around a loop
    LoopFrame(LoopHead),
    Heap(Option<bool>),
    Result,
    Unnamed,
}

pub(crate) struct TranslationCtxt<'a, 'tcx> {
    pub(super) tcx: &'a GlobalT<'a, 'tcx>,
    pub tycx: TypeCtxt<'a, 'tcx>,
    // pub(super) id: LocalDefId,
    pub(super) params: HashMap<ArgRef<'tcx>, (Local, Ty<'tcx>)>,

    pub(super) defined_labels: HashSet<Symbol<'tcx>>,
    pub(super) used_labels: HashSet<Symbol<'tcx>>,
    pub(super) goto_labels: HashSet<Symbol<'tcx>>,
}

impl<'tcx> TranslationCtxt<'_, 'tcx> {
    pub(super) fn get_param(&self, r: ArgRef<'tcx>) -> (Local, Ty<'tcx>) {
        *self.params.get(&r).expect("local not found")
    }
}

impl<'a, 'tcx> TranslationCtxt<'a, 'tcx> {
    pub(crate) fn new(tcx: &'a GlobalT<'a, 'tcx>) -> Self {
        // let (params, tys) = tcx.fn_sig(id).map(|sig| sig.args()).unwrap_or_default();
        // let locals = tys.iter().copied().collect();
        // let params = arg_ref_to_stmt_local(params, 0).collect();
        let self_ = Self {
            tcx,
            tycx: tcx.type_translator(Default::default()),
            // id,
            params: Default::default(),
            defined_labels: Default::default(),
            used_labels: Default::default(),
            goto_labels: Default::default(),
        };
        self_
    }

    pub fn add_locals(&mut self, locals: impl Iterator<Item = (ArgRef<'tcx>, (Local, Ty<'tcx>))>) {
        for (param, v) in locals {
            // let id = self.locals.push_and_get_key(ty);
            let old = self.params.insert(param, v);
            assert!(old.is_none(), "duplicate var in body `{param:?}`");
        }
    }

    // pub(crate) fn add_return(&mut self) {
    //     let sig = self.tcx.fn_sig(self.id).unwrap();
    //     let (ret, tys) = sig.returns();
    //     let ret = arg_ref_to_stmt_local(ret, self.locals.len());
    //     self.params.extend(ret);
    //     self.locals.extend(tys.iter().copied());
    //     assert_eq!(
    //         self.params.len(),
    //         self.locals.len(),
    //         "duplicate parameters in returns"
    //     );
    // }

    // pub(crate) fn fn_result(&self) -> Ty<'tcx> {
    //     self.params[&ArgRef::Result].1
    // }
}

// fn arg_ref_to_stmt_local<'a, 'tcx>(
//     arg_ref: &'a [ArgRef<'tcx>],
//     offset: usize,
// ) -> impl Iterator<Item = (ArgRef<'tcx>, Local)> + 'a {
//     arg_ref
//         .iter()
//         .enumerate()
//         .map(move |(i, param)| (*param, Local::from(i + offset)))
// }
