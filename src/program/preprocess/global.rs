use crate::HashMap;

use crate::parse::{ArgOrType, Declaration, DomainElementKind, Method, Program};

use crate::{program::*, TiVec};

#[derive(Debug, Default)]
pub struct Globals<'tcx> {
    pub(crate) data: TiVec<LocalDefId, MemberData<'tcx>>,
    pub(crate) resolved: HashMap<Symbol<'tcx>, LocalDefId>,
    pub(crate) sigs: TiVec<LocalDefId, Option<FnSig<'tcx>>>,
}

impl<'tcx> Globals<'tcx> {
    /// The only preprocessing run before de-sugaring.
    pub(crate) fn calculate_kinds(&mut self, interner: &Interner<'tcx>, program: &Program) {
        assert_eq!(self.data.len(), 0);
        self.data.reserve_exact(program.len());
        for (id, decl) in program.iter() {
            let id = LocalDefId::from(id);
            let data = self.calculate_kind(interner, id, decl);
            let new = self.data.push_and_get_key(data);
            assert_eq!(new, id);
        }
    }

    fn calculate_kind(&mut self, interner: &Interner<'tcx>, id: LocalDefId, decl: &Declaration) -> MemberData<'tcx> {
        let name = decl.idn_decl().map(|d| interner.mk_symbol(&d.0));
        if let Some(name) = name {
            let old = self.resolved.insert(name, id);
            assert!(old.is_none(), "redeclaration of {name:?}");
        }
        use Declaration::*;
        let mut domain = None;
        let mut resolve_domain = |d| domain = Some(self.resolved[&interner.mk_symbol(d)].into());
        let sig = decl.signature().map(|sig|
            DeclSig { name: name.unwrap(), args: Some(sig.args.len()), rets: sig.ret.len() }
        );
        let kind = match decl {
            Import(..) => MemberKind::Import,
            Define(..) => MemberKind::Define,
            Domain(..) => MemberKind::Domain,
            DomainElement(crate::parse::DomainElement { domain, kind: DomainElementKind::Axiom(..) }) => {
                resolve_domain(domain);
                MemberKind::DomainAxiom
            }
            DomainElement(crate::parse::DomainElement { domain, kind: DomainElementKind::Function(..) }) => {
                resolve_domain(domain);
                MemberKind::DomainFunction
            }
            Field(..) => MemberKind::Field,
            Predicate(..) => MemberKind::Predicate,
            Function(..) => MemberKind::Function,
            Method(..) => MemberKind::Method,
            Adt(_) => todo!("adts not supported yet"),
        };
        MemberData { kind, domain, sig }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum MemberKind {
    Import,
    Define,
    Domain,
    DomainFunction,
    DomainAxiom,
    Field,
    Predicate,
    Function,
    Method,
}

#[derive(Debug, Clone, Copy)]
pub struct MemberData<'tcx> {
    pub kind: MemberKind,
    pub domain: Option<DefId>,
    pub sig: Option<DeclSig<'tcx>>,
}

/// The signature as it is declared
#[derive(Debug, Clone, Copy)]
pub struct DeclSig<'tcx> {
    pub name: Symbol<'tcx>,
    pub args: Option<usize>,
    pub rets: usize,
}

/// The signature with heap arguments added
#[derive(Debug)]
pub struct FnSig<'tcx> {
    arg_ref: Vec<ArgRef<'tcx>>,
    args: TyList<'tcx>,
    returns: Option<MethodData<'tcx>>,
    pub heap_dependent: bool,
}

#[derive(Debug)]
pub struct MethodData<'tcx> {
    pub ret_ref: Vec<ArgRef<'tcx>>,
    pub returns: TyList<'tcx>,
}

impl<'tcx> FnSig<'tcx> {
    pub fn args(&self) -> (&[ArgRef<'tcx>], &'tcx [Ty<'tcx>]) {
        let arg_ref = self.arg_ref.as_slice();
        let args = self.args.as_slice();
        assert_eq!(arg_ref.len(), args.len());
        if self.returns.is_none() {
            (&arg_ref[..args.len() - 1], &args[..args.len() - 1])
        } else {
            (arg_ref, args)
        }
    }

    pub fn caller_args(&self) -> (&[ArgRef<'tcx>], &'tcx [Ty<'tcx>]) {
        Self::trim_params(self.args())
    }

    pub fn returns(&self) -> (&[ArgRef<'tcx>], &'tcx [Ty<'tcx>]) {
        if let Some(MethodData { ret_ref, returns, .. }) = &self.returns {
            (ret_ref, returns.as_slice())
        } else {
            let arg_ref = self.arg_ref.as_slice();
            let args = self.args.as_slice();
            assert_eq!(arg_ref.len(), args.len());
            (&arg_ref[arg_ref.len() - 1..], &args[args.len() - 1..])
        }
    }

    pub fn caller_returns(&self) -> (&[ArgRef<'tcx>], &'tcx [Ty<'tcx>]) {
        Self::trim_params(self.returns())
    }

    fn trim_params<'a>((mut param_ref, mut params): (&'a [ArgRef<'tcx>], &'tcx [Ty<'tcx>])) -> (&'a [ArgRef<'tcx>], &'tcx [Ty<'tcx>]) {
        assert_eq!(param_ref.len(), params.len());
        while let Some((ArgRef::Heap(..), pr)) = param_ref.split_last() {
            param_ref = pr;
            params = &params[..params.len() - 1];
        }
        (param_ref, params)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ArgRef<'tcx> {
    Ident(Symbol<'tcx>),
    Label(Symbol<'tcx>),
    Heap(Option<bool>),
    Result,
    Unnamed,
}

impl<'tcx> TyCtxt<'tcx> {
    /// Run after de-sugaring.
    pub(crate) fn calculate_fn_sigs(&mut self, program: &Program) {
        assert_eq!(self.globals.sigs.len(), 0);
        self.globals.sigs.reserve_exact(program.len());
        for (id, decl) in program.iter() {
            let id = LocalDefId::from(id);
            let sig = self.calculate_fn_sig(id, decl);
            let new = self.globals.sigs.push_and_get_key(sig);
            assert_eq!(new, id);
        }
    }

    fn calculate_fn_sig(&mut self, id: LocalDefId, decl: &Declaration) -> Option<FnSig<'tcx>> {
        let sig = decl.signature()?;

        let intern_arg = |a: &ArgOrType| self.translate_type(a.ty());
        let arg_ref = |a: &ArgOrType| match a {
            ArgOrType::Arg(idt) => ArgRef::Ident(self.interner.mk_symbol(&idt.idn.0)),
            ArgOrType::Type(..) => ArgRef::Unnamed,
        };
        let mk_compound = |contract| {
            let compound = CompoundId { did: id.into(), contract };
            let ret = TyKind::Compound(compound);
            self.interner.mk_ty_from_kind(ret)
        };
        let mk_resource_id = |ty| {
            let ret = TyKind::ResourceId(ty);
            self.interner.mk_ty_from_kind(ret)
        };

        use Declaration::*;
        let (heap_arg, ret) = match decl {
            Function(f) => {
                assert_eq!(sig.ret.len(), 1);
                let heap_dependent = !f.contract.precondition.is_pure();
                // TODO: should this be a `Heap` type arg?
                let earg = heap_dependent.then(|| mk_compound(Some(false)));
                (earg, Ok(intern_arg(&sig.ret[0])))
            }
            DomainElement(crate::parse::DomainElement { kind: DomainElementKind::Function(..), .. }) => {
                assert_eq!(sig.ret.len(), 1);
                (None, Ok(intern_arg(&sig.ret[0])))
            }
            Field(..) => {
                assert_eq!(sig.ret.len(), 1);
                let ret = intern_arg(&sig.ret[0]);
                (None, Ok(mk_resource_id(ret)))
            }
            Predicate(..) => {
                assert_eq!(sig.ret.len(), 0);
                let ret = mk_compound(None);
                (None, Ok(mk_resource_id(ret)))
            }
            Method(m) => {
                let ret_ref: Vec<_> = sig.ret.iter().map(arg_ref).chain([ArgRef::Heap(Some(true))]).collect();
                let ret = sig.ret.iter().map(&intern_arg).chain([mk_compound(Some(true))]).collect();
                let returns = self.interner.mk_ty_list(ret);
                let data = self.calculate_method_data(id, m, ret_ref, returns);
                // TODO: this should be a `Heap` type arg?
                let earg = mk_compound(Some(false));
                (Some(earg), Err(data))
            }
            Adt(_) => todo!(),
            _ => unreachable!(),
        };
        let ret_ok = ret.as_ref().ok().copied();

        let arg_ref = sig.args.iter().map(arg_ref);
        let arg_ref = arg_ref.chain(heap_arg.map(|_| ArgRef::Heap(Some(false))));
        let arg_ref: Vec<_> = arg_ref.chain(ret_ok.map(|_| ArgRef::Result)).collect();

        let args = sig.args.iter().map(&intern_arg).chain(heap_arg).chain(ret_ok).collect();
        let args = self.interner.mk_ty_list(args);
        assert_eq!(arg_ref.len(), args.len());

        Some(FnSig {
            arg_ref,
            args,
            returns: ret.err(),
            heap_dependent: heap_arg.is_some(),
        })
    }

    fn calculate_method_data(&self, _id: LocalDefId, _method: &Method, ret_ref: Vec<ArgRef<'tcx>>, returns: TyList<'tcx>) -> MethodData<'tcx> {
        assert_eq!(returns.len(), ret_ref.len());
        MethodData {
            ret_ref,
            returns,
        }
    }
}
