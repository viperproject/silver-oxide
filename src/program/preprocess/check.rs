use crate::{parse::*, program::*};

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
                    self.resolve(ident, |data| {
                        matches!(data.kind, MemberKind::DomainFunction | MemberKind::Function | MemberKind::Method) &&
                            data.sig.unwrap().args.is_none_or(|params| params == args.len()) &&
                            data.sig.unwrap().rets == tgts.len()
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
                self.resolve(ident, |data| {
                    matches!(data.kind, MemberKind::DomainFunction | MemberKind::Function | MemberKind::Predicate) &&
                        data.sig.unwrap().args.is_none_or(|params| params == args.len())
                });
            }
            ExpKind::Field(_, ident) => {
                self.resolve(ident, |data| matches!(data.kind, MemberKind::Field));
            }
            ExpKind::HeapUpdate(_, acc, _) if self.is_function() => {
                assert_eq!(*acc.perm, ExpKind::Const(ConstKind::write()), "specifying perm amount in function heap updates not allowed");
            }
            _ => (),
        }
        ast.walk_children(self);
    }

    fn walk_acc_exp(&mut self, ast: &'a AccExp) {
        match &*ast.acc.loc {
            ExpKind::FuncApp(ident, _) =>
                self.resolve(ident, |data| matches!(data.kind, MemberKind::Predicate)),
            ExpKind::Field(..) => (),
            // TODO: wands
            _ => panic!("acc location must be a predicate or field")
        }
        ast.walk_children(self);
    }
}

impl<'tcx> CallResolver<'_, 'tcx> {
    fn resolve(&mut self, ident: &Ident, p: impl FnOnce(MemberData) -> bool) {
        let i = self.tcx.interner.mk_symbol(ident);
        let i = self.tcx.global_ref(i);
        if !i.is_some_and(|id| p(self.tcx.data(id))) {
            panic!("unresolved `{:?}`: {:#?}", ident, i.map(|id| self.tcx.data(id)));
            self.errors.push(ResolveError { unresolved: ident.clone() });
        }
    }

    fn is_function(&self) -> bool {
        self.tcx.is_function(self.did)
    }
}
