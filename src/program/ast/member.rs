use core::fmt;

use petgraph::prelude::DiGraphMap;

use crate::{
    parse::{Declaration, DomainElementKind, Program},
    program::{body::StatementKind, translate::TranslationCtxt, CanDot, DefId, Ty, TyCtxt, TyKind},
    HashMap, HashSet, TiVec,
};

use super::{
    body::{Body, Statement},
    exp::{Exp, ExpLine, ExpLineKind},
    idx::LocalDefId,
    resource::ResourceExp,
};

#[derive(Debug, Default)]
pub struct Members<'tcx> {
    pub(crate) bodies: TiVec<LocalDefId, Member<'tcx>>,
    pub(crate) dep_graph: DepGraph,
    pub(crate) topo: Vec<DepNode>,
}

pub(crate) type DepGraph = DiGraphMap<DepNode, ()>;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DepNode {
    pub id: DefId,
    /// `false` by default, `true` for function post/body, method bodies are not
    /// included in the graph.
    pub kind: bool,
}

#[derive(Debug)]
pub enum Member<'tcx> {
    Import,
    Define,
    Domain,
    DomainFunction(LocalDefId),
    DomainAxiom(LocalDefId, Exp<'tcx>),
    Field,
    Predicate(Option<ResourceExp<'tcx>>),
    Function(ResourceExp<'tcx>, Exp<'tcx>, Option<Exp<'tcx>>),
    Method(ResourceExp<'tcx>, ResourceExp<'tcx>, Option<Body<'tcx>>),
}

impl TyCtxt<'_> {
    pub(crate) fn calculate_members(&mut self, program: &Program) {
        let mut self_ = Members::default();
        self_.bodies.reserve_exact(program.len());
        for (id, decl) in program.iter() {
            let member = self_.calculate_member(self, id, decl);
            let new = self_.bodies.push_and_get_key(member);
            assert_eq!(new, id);
            self_.calculate_member_deps(self, id);
        }
        self_.calculate_topo(self);
        self_.dep_graph.dump_dot(false);
        self.members = self_;
    }
}

impl<'tcx> Members<'tcx> {
    // pub fn topo(&self) -> impl Iterator<Item = DepNode> + '_ {
    //     self.topo.iter().copied().map(|node| node.id.expect_local())
    // }

    fn calculate_member(
        &self,
        tcx: &TyCtxt<'tcx>,
        id: LocalDefId,
        decl: &Declaration,
    ) -> Member<'tcx> {
        let mut trcx = TranslationCtxt::new(tcx, id);
        use Declaration::*;
        match decl {
            Import(..) => Member::Import,
            Define(..) => Member::Define,
            Domain(..) => Member::Domain,
            DomainElement(crate::parse::DomainElement {
                domain,
                kind: DomainElementKind::Axiom(ax),
            }) => {
                let did = tcx.resolve_global_ref(domain).expect_local();
                let a = trcx.translate_exp(&ax.exp.0, tcx.types.bool_, false);
                eprintln!("[Translate] axiom {:?}\n{a:?}", ax.name);
                Member::DomainAxiom(did, a)
            }
            DomainElement(crate::parse::DomainElement {
                domain,
                kind: DomainElementKind::Function(..),
            }) => {
                let did = tcx.resolve_global_ref(domain).expect_local();
                Member::DomainFunction(did)
            }
            Field(..) => Member::Field,
            Function(f) => {
                let have_heap = !f.contract.precondition.is_pure();
                let pre = trcx.translate_resource(&f.contract.precondition, None);
                eprintln!("[Translate] fn pre {:?}\n{pre:?}", f.signature.name.0 .0);
                trcx.add_return();
                let post =
                    trcx.translate_exp(&f.contract.postcondition.exp, tcx.types.bool_, have_heap);
                eprintln!("[Translate] fn post {:?}\n{post:?}", f.signature.name.0 .0);
                let body = f.body.as_ref().map(|b| {
                    let ty = trcx.fn_result();
                    let body = trcx.translate_exp(&b.0, ty, have_heap);
                    eprintln!("[Translate] fn body {:?}\n{body:?}", f.signature.name.0 .0);
                    body
                });
                Member::Function(pre, post, body)
            }
            Predicate(p) => {
                let body = p.body.as_ref().map(|b| trcx.translate_resource(&b.0, None));
                if let Some(body) = &body {
                    eprintln!(
                        "[Translate] predicate {:?}\n{body:?}",
                        p.signature.name.0 .0
                    );
                }
                Member::Predicate(body)
            }
            Method(m) => {
                let pre = trcx.translate_resource(&m.contract.precondition, None);
                eprintln!(
                    "[Translate] method pre {:?}\n{pre:?}",
                    m.signature.name.0 .0
                );
                trcx.add_return();
                let post = trcx.translate_resource(&m.contract.postcondition, None);
                eprintln!(
                    "[Translate] method post {:?}\n{post:?}",
                    m.signature.name.0 .0
                );
                let body = m.body.as_ref().map(|b| trcx.translate_body(b));
                if let Some(body) = &body {
                    eprintln!(
                        "[Translate] method body {:?}\n{body:?}",
                        m.signature.name.0 .0
                    );
                }
                Member::Method(pre, post, body)
            }
            Adt(..) => todo!(),
        }
    }

    fn calculate_member_deps(&mut self, tcx: &TyCtxt<'tcx>, id: LocalDefId) {
        let this = |kind| DepNode {
            id: id.into(),
            kind,
        };
        match &self.bodies[id] {
            Member::Domain => (),
            Member::DomainFunction(..) => (),
            Member::DomainAxiom(domain, ..) => {
                // TODO: check that any called functions do not have preconditions
                self.dep_graph.add_edge(
                    this(false),
                    DepNode {
                        id: domain.into(),
                        kind: false,
                    },
                    (),
                );
            }
            Member::Field => (),
            Member::Predicate(Some(re)) => {
                let this = this(false);
                tcx.walk_resource_exp(&mut self.dep_graph, this, re);
            }
            Member::Function(pre, post, body) => {
                let pre_ref = this(false);
                tcx.walk_resource_exp(&mut self.dep_graph, pre_ref, pre);
                let body_ref = this(true);
                self.dep_graph.add_edge(pre_ref, body_ref, ());
                tcx.walk_lines(&mut self.dep_graph, body_ref, post.walk());
                if let Some(body) = body {
                    tcx.walk_lines(&mut self.dep_graph, body_ref, body.walk());
                }
            }
            Member::Method(pre, post, body) => {
                let contract = this(false);
                tcx.walk_resource_exp(&mut self.dep_graph, contract, pre);
                tcx.walk_resource_exp(&mut self.dep_graph, contract, post);
                if let Some(body) = body {
                    let body_ref = this(true);
                    self.dep_graph.add_edge(contract, body_ref, ());
                    tcx.walk_stmts(&mut self.dep_graph, body_ref, body.walk());
                }
            }
            _ => (),
        }
    }

    fn calculate_topo(&mut self, tcx: &TyCtxt<'tcx>) {
        let topo = petgraph::algo::toposort(&self.dep_graph, None);
        let topo = topo.expect("Cyclic dependencies in the program");
        let mut tr_closure = HashMap::<_, HashSet<_>>::default();
        for &node in topo.iter().rev() {
            let succs = tr_closure.entry(node).or_default();
            let succs = unsafe { &mut *(succs as *mut HashSet<_>) };
            for succ in self.dep_graph.neighbors(node) {
                succs.insert(succ);
                succs.extend(&tr_closure[&succ]);
            }
            // Remove foreign nodes from the graph
            if succs.is_empty() && !node.id.is_local() {
                tr_closure.swap_remove(&node);
                self.dep_graph.remove_node(node);
            }
        }
        let is_fn = |node: DepNode| {
            node.id.as_local().is_some_and(|id| {
                let member = &self.bodies[id];
                matches!(member, Member::Function(..))
            })
        };
        let tr_dep = |mut from: DepNode, to: DepNode| {
            if from == to {
                return true;
            }
            if is_fn(from) {
                from.kind = false;
            }
            assert!(from != to);
            let tr_dep = tr_closure[&from].contains(&to);
            if tr_dep {
                let (from, to) = (
                    tcx.item_name(from.id).unwrap(),
                    tcx.item_name(to.id).unwrap(),
                );
                eprintln!("warning: the post/body of function `{to}` cannot be used in the post/body of `{from}` due to a cyclic dependency");
            }
            tr_dep
        };
        for &node in &topo {
            if !node.kind && is_fn(node) {
                continue;
            }
            let fn_pre = DepNode {
                id: node.id,
                kind: false,
            };
            let children = self.dep_graph.neighbors(fn_pre);
            let possible: Vec<_> = children.filter(|&child| !tr_dep(child, node)).collect();
            for child in possible {
                self.dep_graph.remove_edge(fn_pre, child);
                self.dep_graph.add_edge(node, child, ());
            }
        }
        let topo = petgraph::algo::toposort(&self.dep_graph, None);
        self.topo = topo.unwrap();
    }
}

impl<'tcx> TyCtxt<'tcx> {
    fn walk_resource_exp(&self, dep_graph: &mut DepGraph, this: DepNode, re: &ResourceExp<'tcx>) {
        self.walk_tys(dep_graph, this, re.walk_locals());
        self.walk_lines(dep_graph, this, re.walk());
    }

    fn walk_lines<'a>(
        &self,
        dep_graph: &mut DepGraph,
        this: DepNode,
        lines: impl Iterator<Item = &'a ExpLine<'tcx>>,
    ) where
        'tcx: 'a,
    {
        for line in lines {
            self.walk_tys(dep_graph, this, line.ty.walk());
            match line.kind {
                ExpLineKind::Call(id, ..) => {
                    if self.is_function(id) {
                        let did = DepNode { id, kind: false };
                        dep_graph.add_edge(did, this, ());
                    }
                }
                ExpLineKind::HeapUpdate(.., [_, loc, _]) => {
                    self.walk_fold_unfold(dep_graph, this, loc.ty);
                }
                _ => (),
            }
        }
    }

    fn walk_tys(
        &self,
        dep_graph: &mut DepGraph,
        this: DepNode,
        tys: impl Iterator<Item = Ty<'tcx>>,
    ) {
        for ty in tys {
            if let TyKind::Domain(domain, ..) = *ty.kind() {
                let domain = DepNode {
                    id: domain,
                    kind: false,
                };
                dep_graph.add_edge(domain, this, ());
            }
        }
    }

    fn walk_stmts<'a>(
        &self,
        dep_graph: &mut DepGraph,
        this: DepNode,
        stmts: impl Iterator<Item = &'a Statement<'tcx>>,
    ) where
        'tcx: 'a,
    {
        for stmt in stmts {
            use StatementKind::*;
            match &stmt.kind {
                Eval(.., exp) => self.walk_lines(dep_graph, this, exp.walk()),
                Havoc(..) | MergeHeap(..) | Assign(..) => (),
                Call(_, method, _) => {
                    dep_graph.add_edge(
                        DepNode {
                            id: *method,
                            kind: false,
                        },
                        this,
                        (),
                    );
                }
                Ghost(.., re) => self.walk_resource_exp(dep_graph, this, re),
                Predicate(.., loc, _) => self.walk_fold_unfold(dep_graph, this, loc.ty),
            }
        }
    }

    fn walk_fold_unfold(&self, dep_graph: &mut DepGraph, this: DepNode, loc: Ty<'tcx>) {
        let TyKind::ResourceId(ty) = loc.kind() else {
            unreachable!()
        };
        if let TyKind::Compound(cid) = ty.kind() {
            assert!(cid.contract.is_none());
            let cid = DepNode {
                id: cid.did,
                kind: false,
            };
            dep_graph.add_edge(cid, this, ());
        }
    }
}

impl fmt::Debug for DepNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.id)?;
        if self.kind {
            write!(f, ".body")?;
        }
        Ok(())
    }
}
