use crate::{HashMap, HashSet};

use crate::{parse::{AstWalkable, AstWalker, ExpKind, Statement, StmtBlock}, program::{ArgRef, Local, LocalDefId, Symbol, Ty, TyCtxt, TyKind}, TiVec};

pub(crate) struct TranslationCtxt<'a, 'tcx> {
    pub(super) tcx: &'a TyCtxt<'tcx>,
    pub(super) id: LocalDefId,
    pub(super) params: HashMap<ArgRef<'tcx>, Local>,
    pub(super) locals: TiVec<Local, Ty<'tcx>>,

    pub(super) defined_labels: HashSet<Symbol<'tcx>>,
    pub(super) used_labels: HashSet<Symbol<'tcx>>,
    pub(super) goto_labels: HashSet<Symbol<'tcx>>,
}

impl<'a, 'tcx> TranslationCtxt<'a, 'tcx> {
    pub(super) fn get_param(&self, r: ArgRef<'tcx>) -> Local {
        *self.params.get(&r).expect("local not found")
    }
}

impl<'a, 'tcx> TranslationCtxt<'a, 'tcx> {
    pub(crate) fn new(tcx: &'a TyCtxt<'tcx>, id: LocalDefId) -> Self {
        let (params, tys) = tcx.fn_sig(id).map(|sig| sig.args()).unwrap_or_default();
        let locals = tys.iter().copied().collect();
        let params = arg_ref_to_stmt_local(params, 0).collect();
        let self_ = Self {
            tcx,
            id,
            params,
            locals,
            defined_labels: Default::default(),
            used_labels: Default::default(),
            goto_labels: Default::default(),
        };
        assert_eq!(self_.params.len(), self_.locals.len(), "duplicate parameters in signature");
        self_
    }

    pub(crate) fn add_return(&mut self) {
        let sig = self.tcx.fn_sig(self.id).unwrap();
        let (ret, tys) = sig.returns();
        let ret = arg_ref_to_stmt_local(ret, self.locals.len());
        self.params.extend(ret);
        self.locals.extend(tys.iter().copied());
        assert_eq!(self.params.len(), self.locals.len(), "duplicate parameters in returns");
    }

    pub(crate) fn fn_result(&self) -> Ty<'tcx> {
        self.locals[self.params[&ArgRef::Result]]
    }

    pub(super) fn add_body(&mut self, body: &StmtBlock) {
        self.walk_block(body);
        for label in self.used_labels.drain(..) {
            assert!(self.defined_labels.contains(&label), "label `{label}` not defined");
            let heap = self.locals.push_and_get_key(self.tcx.types.heap_);
            let old = self.params.insert(ArgRef::Label(label), heap);
            assert!(old.is_none());
        }
        assert!(self.goto_labels.is_subset(&self.defined_labels), "goto label not defined");
    }

    pub(super) fn any_resource_id(&self) -> Ty<'tcx> {
        let heap_ = self.tcx.types.heap_;
        self.tcx.interner.mk_ty_from_kind(TyKind::ResourceId(heap_))
    }
}

fn arg_ref_to_stmt_local<'a, 'tcx>(arg_ref: &'a [ArgRef<'tcx>], offset: usize) -> impl Iterator<Item = (ArgRef<'tcx>, Local)> + 'a {
    arg_ref.iter().enumerate().map(move |(i, param)| (*param, Local::from(i + offset)))
}

impl<'a> AstWalker<'a> for TranslationCtxt<'_, '_> {
    fn walk_statement(&mut self, ast: &'a Statement) {
        match ast {
            Statement::Var(new, _) => {
                for new in new {
                    let ty = self.tcx.translate_type(&new.ty);
                    let id = self.locals.push_and_get_key(ty);
                    let idn = self.tcx.interner.mk_symbol(&new.idn.0);
                    let old = self.params.insert(ArgRef::Ident(idn), id);
                    assert!(old.is_none(), "duplicate var in body `{}`", new.idn.0.0);
                }
            }
            Statement::Label(decl, _) => {
                self.defined_labels.insert(self.tcx.interner.mk_symbol(&decl.0));
            }
            Statement::Goto(label) => {
                self.goto_labels.insert(self.tcx.interner.mk_symbol(label));
            }
            _ => (),
        }
        ast.walk_children(self);
    }

    fn walk_exp_kind(&mut self, ast: &'a ExpKind) {
        match ast {
            ExpKind::Old(Some(label), ..) => {
                self.used_labels.insert(self.tcx.interner.mk_symbol(label));
            }
            _ => (),
        }
        ast.walk_children(self);
    }
}
