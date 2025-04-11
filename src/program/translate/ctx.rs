use fxhash::FxHashMap;

use crate::{parse::{AstWalkable, AstWalker, Statement, StmtBlock}, program::{body::Location, ArgRef, Local, LocalDefId, Ty, TyCtxt, TyKind}, TiVec};

pub(crate) struct TranslationCtxt<'a, 'tcx> {
    pub(super) tcx: &'a TyCtxt<'tcx>,
    pub(super) id: LocalDefId,
    pub(super) params: FxHashMap<ArgRef<'tcx>, Local>,
    pub(super) locals: TiVec<Local, Ty<'tcx>>,

    /// Ok(false) -> no heap, Ok(true) -> self framing heap, Err(loc) -> heap in method
    pub(super) curr_heap: Result<bool, Location>,
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
            curr_heap: Ok(false),
            params,
            locals,
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
        if let Statement::Var(new, _) = ast {
            for new in new {
                let ty = self.tcx.translate_type(&new.ty);
                let id = self.locals.push_and_get_key(ty);
                let idn = self.tcx.interner.mk_symbol(&new.idn.0);
                let old = self.params.insert(ArgRef::Ident(idn), id);
                assert!(old.is_none(), "duplicate var in body `{}`", new.idn.0.0);
            }
        }
        ast.walk_children(self);
    }
}
