use core::fmt;
use core::cell::Cell;
use core::ops::{Deref, DerefMut};

use petgraph::prelude::DiGraphMap;

use crate::vmir::ty::DomainKind;
use crate::{vmir::{middle::*, Symbol}, TiVec};

use super::Axiom;
use super::{member::{Function, Method, Resource}, Def, DefId, DefKind, LocalDefId};

#[derive(Default)]
pub struct Members<'tcx> {
    pub(crate) map: MembersMap<'tcx>,

    // pub(crate) bodies: TiVec<LocalDefId, Member<'tcx>>,
    pub(crate) dep_graph: DepGraph,
    pub(crate) topo: Vec<MemberRef>,
}

#[derive(Default)]
pub struct MembersMap<'tcx> {
    pub(crate) local_defs: TiVec<LocalDefId, Def<'tcx>>,

    pub(crate) resources: TiVec<ResourceId, Resource<'tcx>>,
    pub(crate) functions: TiVec<FunctionId, Function<'tcx>>,
    pub(crate) methods: TiVec<MethodId, Method<'tcx>>,
    pub(crate) axioms: TiVec<AxiomId, Axiom<'tcx>>,
}

impl<'tcx> Members<'tcx> {
    pub fn register(&mut self, symbol: Symbol<'tcx>, kind: DefKind) -> LocalDefId {
        let def = Def {
            symbol,
            kind,
        };
        self.local_defs.push_and_get_key(def)
    }
}

impl<'tcx> Members<'tcx> {
    pub fn dump_vmir(&self, force: bool) -> Option<String> {
        if !force && std::env::var("VIPER_VMIR").is_err() {
            return None;
        }
        let mut path = crate::log_dir();
        path += "/program.vmir";
        let file = std::path::Path::new(&path);
        std::fs::create_dir_all(file.parent().unwrap()).unwrap();
        let mut file = std::fs::File::create(file).unwrap();

        for &topo in &self.topo {
            self.dump_member(&mut file, topo).unwrap();
        }
        Some(path)
    }

    fn dump_member(&self, f: &mut std::fs::File, member: MemberRef) -> std::io::Result<()> {
        use std::io::Write;
        use MemberRef::*;
        match member {
            // Could print these here
            Type(..) => Ok(()),
            Resource(r) => writeln!(f, "{}", &self.resources[r]),
            Function(fi) => writeln!(f, "{}", &self.functions[fi]),
            Method(m) => writeln!(f, "{}", &self.methods[m]),
            Axiom(a) => writeln!(f, "{}", &self.axioms[a]),
        }
    }
}

pub(crate) type DepGraph = DiGraphMap<MemberRef, bool>;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum MemberRef {
    Type(DomainKind),
    Resource(ResourceId),
    Function(FunctionId),
    Method(MethodId),
    Axiom(AxiomId),
}

// #[derive(Debug)]
// pub enum Member<'tcx> {
//     Import,
//     Define,
//     Domain,
//     DomainFunction(LocalDefId),
//     DomainAxiom(LocalDefId, Exp<'tcx>),
//     Field,
//     Predicate(Option<ResourceExp<'tcx>>),
//     Function(ResourceExp<'tcx>, Exp<'tcx>, Option<Exp<'tcx>>),
//     Method(ResourceExp<'tcx>, ResourceExp<'tcx>, Option<Body<'tcx>>),
// }

impl fmt::Debug for MemberRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            MemberRef::Type(dk) => write!(f, "{dk}"),
            MemberRef::Axiom(ax) => write!(f, "{ax:?}"),
            MemberRef::Resource(r) => write!(f, "{r}"),
            MemberRef::Function(fi) => write!(f, "{fi}"),
            MemberRef::Method(m) => write!(f, "{m}"),
        }
    }
}

impl<'tcx> Deref for Members<'tcx> {
    type Target = MembersMap<'tcx>;
    fn deref(&self) -> &Self::Target {
        &self.map
    }
}
impl<'tcx> DerefMut for Members<'tcx> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.map
    }
}
