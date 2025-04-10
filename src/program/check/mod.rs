use crate::parse::{AccExp, AstWalkable, AstWalker, ConstKind, ExpKind, Ident, Program, Statement};

use super::{idx::LocalDefId, FnSig, MemberKind, TyCtxt};

#[derive(Debug)]
pub struct ResolveError {
    pub unresolved: Ident,
}

impl<'tcx> TyCtxt<'tcx> {
    pub(crate) fn resolve_calls(&self, program: &Program) -> Result<(), Vec<ResolveError>> {
        let mut cr = CallResolver { tcx: self, errors: Vec::new(), did: LocalDefId::MAX };
        cr.walk_program(program);
        cr.errors.is_empty().then_some(()).ok_or(cr.errors)
    }
}

struct CallResolver<'a, 'tcx> {
    tcx: &'a TyCtxt<'tcx>,
    errors: Vec<ResolveError>,
    did: LocalDefId,
}

impl<'a> AstWalker<'a> for CallResolver<'_, '_> {
    fn visit_local_def_id(&mut self, did: LocalDefId) {
        self.did = did;
    }

    fn walk_statement(&mut self, ast: &'a Statement) {
        match ast {
            Statement::Assign(tgts, e) => match &**e {
                e@ExpKind::FuncApp(ident, args) => {
                    self.resolve(ident, |kind, sig| {
                        matches!(kind, MemberKind::DomainFunction | MemberKind::Function | MemberKind::Method) &&
                            sig.caller_args().0.len() == args.len() &&
                            sig.caller_returns().0.len() == tgts.len()
                    });
                    tgts.walk_children(self);
                    e.walk_children(self);
                    return;
                }
                _ => assert_eq!(tgts.len(), 1),
            }
            _ => (),
        }
        ast.walk_children(self);
    }

    fn walk_exp_kind(&mut self, ast: &'a ExpKind) {
        match ast {
            ExpKind::FuncApp(ident, args) => {
                self.resolve(ident, |kind, sig| {
                    matches!(kind, MemberKind::DomainFunction | MemberKind::Function | MemberKind::Predicate) &&
                        sig.caller_args().0.len() == args.len()
                });
            }
            ExpKind::Field(_, ident) => {
                self.resolve(ident, |kind, _| matches!(kind, MemberKind::Field));
            }
            ExpKind::HeapUpdate(_, acc, _) if self.is_function() => {
                let perm = acc.perm.as_ref().ok().unwrap();
                assert_eq!(**perm, ExpKind::Const(ConstKind::Write), "specifying perm amount in function heap updates not allowed");
            }
            _ => (),
        }
        ast.walk_children(self);
    }

    fn walk_acc_exp(&mut self, ast: &'a AccExp) {
        match &*ast.acc.loc {
            ExpKind::FuncApp(ident, _) =>
                self.resolve(ident, |kind, _| matches!(kind, MemberKind::Predicate)),
            ExpKind::Field(..) => (),
            // TODO: wands
            _ => panic!("acc location must be a predicate or field")
        }
        ast.walk_children(self);
    }
}

impl<'tcx> CallResolver<'_, 'tcx> {
    fn resolve(&mut self, ident: &Ident, p: impl FnOnce(MemberKind, &FnSig) -> bool) {
        let i = self.tcx.interner.mk_symbol(ident);
        let i = self.tcx.global_ref(i);
        if !i.is_some_and(|id| p(self.tcx.kind(id), self.tcx.fn_sig(id).unwrap())) {
            panic!("unresolved `{:?}`: {:#?}", ident, i.map(|id| {
                let sig = self.tcx.fn_sig(id).unwrap();
                (self.tcx.kind(id), sig.caller_args(), sig.caller_returns())
            }));
            self.errors.push(ResolveError { unresolved: ident.clone() });
        }
    }

    fn is_function(&self) -> bool {
        self.tcx.is_function(self.did)
    }
}
