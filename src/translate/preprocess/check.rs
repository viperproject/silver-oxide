use crate::{parse::*, translate::{global::{GlobalT, MemberKind}, *}, vmir::ty::{DomainKind, Ty, TyKind, TyList}};

#[derive(Debug)]
pub struct ResolveError {
    pub unresolved: Ident,
}

impl GlobalT<'_, '_> {
    pub(crate) fn resolve_calls(&self, program: &Program) -> Result<(), Vec<ResolveError>> {
        let mut cr = CallResolver {
            tcx: self,
            program,
            errors: Vec::new(),
            did: MemberId::MAX,
        };
        cr.walk_program(program);
        cr.errors.is_empty().then_some(()).ok_or(cr.errors)
    }
}

struct CallResolver<'a, 'tcx> {
    tcx: &'a GlobalT<'a, 'tcx>,
    program: &'a Program,
    errors: Vec<ResolveError>,
    did: MemberId,
}

impl<'a> AstWalker<'a> for CallResolver<'_, '_> {
    fn visit_member_id(&mut self, did: MemberId) {
        self.did = did;
    }

    fn walk_statement(&mut self, ast: &'a Statement) {
        match ast {
            Statement::Assign(tgts, AssignRhs::Call(ident, params)) => {
                self.resolve(ident, |data| {
                    matches!(
                        data.kind,
                        MemberKind::Function | MemberKind::Method | MemberKind::AdtConstructor
                    ) && data.args.is_none_or(|args| args.len() == params.len())
                        && data.rets.len() == tgts.len()
                });
            }
            Statement::Assign(tgts, _) => assert_eq!(tgts.len(), 1),
            Statement::Fold(acc) | Statement::Unfold(acc) => {
                let ExpKind::FuncApp(ident, ..) = &*acc.acc.loc else {
                    panic!("acc location must be a predicate");
                };
                self.resolve(ident, |data| matches!(data.kind, MemberKind::Predicate));
            }
            _ => (),
        }
        ast.walk_children(self);
    }

    fn walk_exp_kind(&mut self, ast: &'a ExpKind) {
        match ast {
            ExpKind::FuncApp(ident, params) => {
                let is_axiom = self.is_axiom();
                self.resolve(ident, |data| {
                    matches!(
                        data.kind,
                        MemberKind::Function | MemberKind::Predicate | MemberKind::AdtConstructor
                    ) && data.args.is_none_or(|args| args.len() == params.len())
                        && data.rets.len() == 1
                        && (!is_axiom || data.heapless)
                });
            }
            ExpKind::Field(_, ident) => {
                let is_adt = self.tcx.resolve_adt_destr(ident).is_some();
                let is_adt = is_adt || ident.0.strip_prefix("is").is_some_and(|s| self.tcx.resolve_adt_cons(s).is_some());
                if !is_adt {
                    self.resolve(ident, |data| matches!(data.kind, MemberKind::Field));
                }
            }
            ExpKind::HeapUpdate(kind, acc, _) if self.is_function() => {
                // use num::One;
                // let ok = match &*acc.perm {
                //     ExpKind::Const(ConstKind::Real(r)) => r.is_one(),
                //     ExpKind::Const(ConstKind::Wildcard) => true,
                //     _ => false,
                // };
                // assert!(
                //     ok,
                //     "specifying perm amount in function heap updates not allowed ({acc:?})"
                // );
                match kind {
                    HeapUpdateOp::Fold | HeapUpdateOp::Unfold => {
                        let ExpKind::FuncApp(loc, ..) = &*acc.acc.loc else {
                            panic!("heap update location must be a predicate");
                        };
                        self.resolve(loc, |data| {
                            matches!(data.kind, MemberKind::Predicate)
                        });
                    }
                    HeapUpdateOp::Apply | HeapUpdateOp::Package => todo!(),
                }
            }
            _ => (),
        }
        ast.walk_children(self);
    }

    fn walk_acc_exp(&mut self, ast: &'a AccExp) {
        match &*ast.acc.loc {
            ExpKind::FuncApp(ident, _) => {
                self.resolve(ident, |data| matches!(data.kind, MemberKind::Predicate))
            }
            ExpKind::Field(..) => (),
            ExpKind::MagicWand(..) => (),
            // TODO: wands
            other => panic!("acc location must be a predicate or field, not {other:?}"),
        }
        ast.walk_children(self);
    }

    fn walk_star_or_names(&mut self, ast: &'a StarOrNames) {
        if let StarOrNames::Names(names) = ast {
            for name in names {
                self.resolve(name, |data| matches!(data.kind, MemberKind::Field));
            }
        }
        ast.walk_children(self);
    }
}

struct CalleeData<'a> {
    kind: MemberKind,
    heapless: bool,
    args: Option<&'a [Ty<'a>]>,
    rets: &'a [Ty<'a>],
}

impl CallResolver<'_, '_> {
    fn resolve(&mut self, ident: &Ident, p: impl FnOnce(CalleeData<'_>) -> bool) {
        let mut adt_args = Vec::new();
        let mut adt_ret = None;
        let id = self.tcx.resolve(ident).unwrap_or_else(|| panic!("unresolved identifier {ident:?}"));
        let kind = self.tcx.get_member_kind(id).expect("unexpected member kind");
        let fn_sig = self.tcx.get_any_function(id);
        let fn_sig = fn_sig.as_ref().map(|(.., heapless, args, ret)| (*heapless, args.map(|args| *args.instantiate_identity()), core::slice::from_ref(ret.instantiate_identity())));
        let (heapless, args, rets) = fn_sig.or_else(|| {
            let (.., args, rets) = self.tcx.get_method(id.unwrap())?;
            Some((false, Some(args), rets))
        }).unwrap_or_else(|| {
            let (adt, vid) = self.tcx.get_constructor(id.unwrap()).expect("unknown callable");
            let data = self.tcx.interner.get_adt_def(adt).data();
            adt_args = data.variants[vid].identity();
            // TODO: TyList
            adt_ret = Some(self.tcx.interner.mk_ty_from_kind(TyKind::Domain(DomainKind::Adt(adt), TyList::empty())));
            (true, Some(&adt_args), core::slice::from_ref(adt_ret.as_ref().unwrap()))
        });
        let data = CalleeData {
            kind,
            heapless,
            args,
            rets,
        };
        if !p(data) {
            panic!(
                "error: unresolved `{:?}`: {:#?}",
                ident,
                id,
            );
            self.errors.push(ResolveError {
                unresolved: ident.clone(),
            });
        }
    }

    fn curr_decl(&self) -> &Declaration {
        &self.program[self.did]
    }

    fn is_axiom(&self) -> bool {
        matches!(self.curr_decl(), Declaration::DomainElement(DomainElement {
            kind: DomainElementKind::Axiom(..),
            ..
        }))
    }

    fn is_function(&self) -> bool {
        matches!(self.curr_decl(), Declaration::Function(..))
    }
}
