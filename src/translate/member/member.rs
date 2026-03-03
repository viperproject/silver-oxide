use crate::{
    parse::{ArgOrType, Declaration, DomainElementKind, MemberId, Program},
    translate::{global::{GlobalT, IdData}, member::TranslationCtxt},
    vmir::{members::*, middle::*, ty::*, Axiom, CanDot},
    HashMap, HashSet,
};

use super::ArgRef;

impl<'tcx> GlobalT<'_, 'tcx> {
    pub(crate) fn calculate_members(mut self, program: &Program) {
        self.post_desugar_intern(program);
        for (id, decl) in program.iter() {
            self.calculate_member(id, decl);
            // self.calculate_member_deps(id);
        }
        self.members.calculate_dependencies();
        self.members.dep_graph.dump_dot(false);
        // self.members = self_;
    }

    fn calculate_member(
        &mut self,
        id: MemberId,
        decl: &Declaration,
    ) {
        let mut trcx = TranslationCtxt::new(self);
        use Declaration::*;
        match (decl, self.get_id_data(id)) {
            (Import(..) | Define(..) | Domain(..) | Adt(..), None) => (),
            (AdtConstructor(..), Some(IdData::AdtConstructor { .. })) => (),
            (Field(..), Some(IdData::Field { .. })) => (),
            (DomainElement(crate::parse::DomainElement {
                kind: DomainElementKind::Function(..), ..
            }), Some(IdData::Function { .. })) => (),
            (Predicate(..), Some(IdData::Predicate { resource_and_fold: None, .. })) => (),
            (DomainElement(crate::parse::DomainElement {
                domain,
                kind: DomainElementKind::Axiom(ax),
            }), None) => {
                let did = self.resolve(domain).unwrap();
                let associated = self.get_domain_or_adt(did.unwrap()).unwrap();
                let params = self.get_params(did).unwrap_or_default();
                assert_eq!(params.len(), 0, "axioms of domains with params not supported yet");

                eprintln!("\n[Translate] axiom {:?}", ax.name);
                let body = trcx.translate_exp(&ax.exp.0, self.types.bool_, false);
                eprintln!("{body}");
                let a = Axiom {
                    associated,
                    body,
                };
                self.members.axioms.push(a);
            }
            (Function(f), Some(IdData::Function { function })) => {
                let f_data = &self.members.functions[function];
                let f_sig = f_data.sig();
                let args = self.translate_args(f.signature.args.iter());
                let args = args.zip(f_sig.params_iter()).filter_map(|(a, (l, t))|
                    a.map(|a| (a, (l, *t.instantiate_identity())))
                );
                trcx.add_locals(args);
                let mut pre_data = None;
                if let Some(pre) = &f_data.pre {
                    eprintln!("\n[Translate] fn pre {:?}", f.signature.name.0 .0);
                    let pre_res = pre.res;
                    let pre = trcx.translate_resource(&f.contract.precondition.as_ref().unwrap(), None);
                    eprintln!("{pre}");
                    pre_data = Some((pre_res, pre));
                }

                let mut body_data = None;
                if let Some(body) = &f.body {
                    let body = trcx.translate_exp(&body.0, *f_sig.ret().instantiate_identity(), pre_data.is_some());
                    eprintln!("\n[Translate] fn body {:?}\n{body}", f.signature.name.0 .0);
                    body_data = Some((function, body))
                }

                let mut post_data = None;
                if let Some(post) = &f_data.post {
                    eprintln!("\n[Translate] fn post {:?}", f.signature.name.0 .0);
                    trcx.add_locals([(ArgRef::Result, (Local::from(post.args.len() - 1), *f_sig.ret().instantiate_identity()))].into_iter());
                    let post_fun = post.fun;
                    let post = trcx.translate_exp(&f.contract.postcondition.as_ref().unwrap().exp, self.types.bool_, pre_data.is_some());
                    eprintln!("{post}");
                    post_data = Some((post_fun, post));
                }
                
                if let Some((pre_res, data)) = pre_data {
                    let body = &mut self.members.resources[pre_res].body;
                    assert!(body.is_none());
                    *body = Some(data);
                }

                if let Some((function, data)) = body_data {
                    let body = &mut self.members.functions[function].body;
                    assert!(body.is_none());
                    *body = Some(data);
                }

                if let Some((post_fun, data)) = post_data {
                    let body = &mut self.members.functions[post_fun].body;
                    assert!(body.is_none());
                    *body = Some(data);
                }
            }
            (Predicate(p), Some(IdData::Predicate { resource_and_fold: Some((resource, _)), .. })) => {
                let r_data = &self.members.resources[resource];
                let args = self.translate_args(p.signature.args.iter());
                let args = args.zip(r_data.locals.iter_enumerated()).filter_map(|(a, v)|
                    a.map(|a| (a, (v.0, *v.1)))
                );
                trcx.add_locals(args);

                eprintln!("\n[Translate] predicate {:?}", p.signature.name.0 .0);
                let r = p.body.as_ref().unwrap();
                let r = trcx.translate_resource(&r.0, None);
                eprintln!("{r}");
                let body = &mut self.members.resources[resource].body;
                assert!(body.is_none());
                *body = Some(r);
            }
            (Method(m), Some(IdData::Method { method })) => {
                let m_data = &self.members.methods[method];
                let m_sig = m_data.sig();
                let args = self.translate_args(m.signature.args.iter());
                let args = args.zip(m_sig.params().iter_enumerated()).filter_map(|(a, v)|
                    a.map(|a| (a, (v.0, *v.1)))
                );
                trcx.add_locals(args);

                let mut pre_data = None;
                if let Some(pre) = &m_data.pre {
                    eprintln!("\n[Translate] method pre {:?}", m.signature.name.0 .0);
                    let pre_res = pre.res;
                    let pre = trcx.translate_resource(&m.contract.precondition.as_ref().unwrap(), None);
                    eprintln!("{pre}");
                    pre_data = Some((pre_res, pre));
                }

                let rets = self.translate_args(m.signature.ret.iter());
                let rets = rets.zip(m_sig.rets_iter()).filter_map(|(a, v)|
                    a.map(|a| (a, v))
                );
                trcx.add_locals(rets);

                let mut post_data = None;
                if let Some(post) = &m_data.post {
                    eprintln!("\n[Translate] method post {:?}", m.signature.name.0 .0);
                    let post_res = post.res;
                    let post = trcx.translate_resource(&m.contract.postcondition.as_ref().unwrap(), None);
                    eprintln!("{post}");
                    post_data = Some((post_res, post));
                }

                let mut body_data = None;
                if let Some(body) = &m.body {
                    let name = trcx.tcx.interner.mk_symbol(&m.signature.name.0);
                    let params = m_sig.params().iter_enumerated().map(|(l, t)| (l, *t)).chain(m_sig.rets_iter());
                    let body = trcx.translate_body(body, name, params);
                    eprintln!("\n[Translate] method body {:?}\n{body}", m.signature.name.0 .0);
                    body_data = Some((method, body));
                }

                if let Some((pre_res, data)) = pre_data {
                    let body = &mut self.members.resources[pre_res].body;
                    assert!(body.is_none());
                    *body = Some(data);
                }
                if let Some((method, data)) = body_data {
                    let body = &mut self.members.methods[method].body;
                    assert!(body.is_none());
                    *body = Some(data);
                }
                if let Some((post_res, data)) = post_data {
                    let body = &mut self.members.resources[post_res].body;
                    assert!(body.is_none());
                    *body = Some(data);
                }
            }
            _ => unreachable!(),
        }
    }

    pub fn translate_args<'b>(&'b self, args: impl Iterator<Item = &'b ArgOrType> + 'b) -> impl Iterator<Item = Option<ArgRef<'tcx>>> + 'b {
        args.map(move |arg| arg.idn().map(|idn| ArgRef::Ident(self.interner.mk_symbol(&idn.0))))
    }

    // fn calculate_member_deps(&self, id: MemberId) {
    //     let this = |kind| MemberRef {
    //         id: id.into(),
    //         kind,
    //     };
    //     match &self.bodies[id] {
    //         Member::Domain => (),
    //         Member::DomainFunction(..) => (),
    //         Member::DomainAxiom(domain, ..) => {
    //             // TODO: check that any called functions do not have preconditions
    //             self.dep_graph.add_edge(
    //                 this(false),
    //                 MemberRef {
    //                     id: domain.into(),
    //                     kind: false,
    //                 },
    //                 (),
    //             );
    //         }
    //         Member::Field => (),
    //         Member::Predicate(Some(re)) => {
    //             let this = this(false);
    //             tcx.walk_resource_exp(&mut self.dep_graph, this, re);
    //         }
    //         Member::Function(pre, post, body) => {
    //             let pre_ref = this(false);
    //             tcx.walk_resource_exp(&mut self.dep_graph, pre_ref, pre);
    //             let body_ref = this(true);
    //             self.dep_graph.add_edge(pre_ref, body_ref, ());
    //             tcx.walk_lines(&mut self.dep_graph, body_ref, post.walk());
    //             if let Some(body) = body {
    //                 tcx.walk_lines(&mut self.dep_graph, body_ref, body.walk());
    //             }
    //         }
    //         Member::Method(pre, post, body) => {
    //             let contract = this(false);
    //             tcx.walk_resource_exp(&mut self.dep_graph, contract, pre);
    //             tcx.walk_resource_exp(&mut self.dep_graph, contract, post);
    //             if let Some(body) = body {
    //                 let body_ref = this(true);
    //                 self.dep_graph.add_edge(contract, body_ref, ());
    //                 tcx.walk_stmts(&mut self.dep_graph, body_ref, body.walk());
    //             }
    //         }
    //         _ => (),
    //     }
    // }
}

pub struct MembersDeps<'a, 'tcx> {
    mmap: &'a MembersMap<'tcx>,
    dep_graph: &'a mut DepGraph,
    topo: &'a mut Vec<MemberRef>,
}

impl<'tcx> Members<'tcx> {
    fn calculate_dependencies(&mut self) {
        let mut deps = MembersDeps {
            mmap: &self.map,
            dep_graph: &mut self.dep_graph,
            topo: &mut self.topo,
        };
        deps.calculate_dependencies();
    }
}

impl<'tcx> MembersDeps<'_, 'tcx> {
    fn calculate_dependencies(&mut self) {
        for (r, data) in self.mmap.resources.iter_enumerated() {
            let this = MemberRef::Resource(r);
            self.dep_graph.add_node(this);
            if let Some(pre) = &data.pre {
                self.dep_graph.add_edge(MemberRef::Resource(pre.res), this, true);
            }
            if let Some(post) = &data.post {
                self.dep_graph.add_edge(MemberRef::Function(post.fun), this, true);
            }
            if let Some(body) = &data.body {
                self.walk_resource_exp(this, body);
            }
        }
        for (f, data) in self.mmap.functions.iter_enumerated() {
            let this = MemberRef::Function(f);
            self.dep_graph.add_node(this);
            if let Some(pre) = &data.pre {
                self.dep_graph.add_edge(MemberRef::Resource(pre.res), this, true);
            }
            if let Some(post) = &data.post {
                self.dep_graph.add_edge(MemberRef::Function(post.fun), this, true);
            }
            if let Some(body) = &data.body {
                self.walk_lines(this, body.walk());
            }
        }
        for (m, data) in self.mmap.methods.iter_enumerated() {
            let this = MemberRef::Method(m);
            self.dep_graph.add_node(this);
            if let Some(pre) = &data.pre {
                self.dep_graph.add_edge(MemberRef::Resource(pre.res), this, true);
            }
            if let Some(post) = &data.post {
                self.dep_graph.add_edge(MemberRef::Resource(post.res), this, true);
            }
            if let Some(body) = &data.body {
                self.walk_stmts(this, body.walk());
            }
        }
        for (a, ax) in self.mmap.axioms.iter_enumerated() {
            let ty = MemberRef::Type(ax.associated);
            self.dep_graph.add_edge(MemberRef::Axiom(a), ty, true);
        }
        self.calculate_topo();
    }

    fn calculate_topo(&mut self) {
        self.remove_cycles();
        let topo = petgraph::algo::toposort(&*self.dep_graph, None);
        *self.topo = topo.expect("Cyclic dependencies in the program");
    }

    fn remove_cycles(&mut self) {
        let sccs = petgraph::algo::tarjan_scc(&*self.dep_graph);
        let mut scc_member = HashMap::default();
        let mut i = 0;
        for scc in sccs.into_iter() {
            assert!(!scc.is_empty());
            if scc.len() == 1 && !self.dep_graph.contains_edge(scc[0], scc[0]) {
                continue;
            }
            for r in scc.iter() {
                let old = scc_member.insert(*r, i);
                assert!(old.is_none(), "duplicate member in scc");
            }
            i += 1;
        }
        if i == 0 {
            return;
        }

        let mut dep_graph = DepGraph::new();
        for node in self.dep_graph.nodes() {
            dep_graph.add_node(node);
        }
        for (from, to, required) in self.dep_graph.all_edges() {
            match (scc_member.get(&from), scc_member.get(&to)) {
                (Some(f), Some(t)) if f == t && !required => {
                    eprintln!("warning: `{to:?}` cannot be used in `{from:?}` due to a cyclic dependency");
                    match (from, to) {
                        (_, MemberRef::Function(..)) => (),
                        (MemberRef::Function(f), MemberRef::Resource(t)) => {
                            let pre = self.mmap.functions[f].pre.as_ref();
                            if pre.is_some_and(|pre| pre.res == t) {
                                // TODO: make this a proper error
                                eprintln!("error: cyclic dependency in function precondition!");
                            }
                        }
                        (MemberRef::Resource(_), MemberRef::Resource(_)) => {
                            // TODO: make this a proper error
                            eprintln!("error: cyclic dependency between resources `{from:?}` and `{to:?}`");
                        }
                        other => todo!("{other:?}"),
                    }
                }
                _ => {
                    dep_graph.add_edge(from, to, *required);
                }
            }
        }
        *self.dep_graph = dep_graph;
    }
}

impl<'tcx> MembersDeps<'_, 'tcx> {
    fn walk_resource_exp(&mut self, this: MemberRef, re: &ResourceExp<'tcx>) {
        // TODO: walk the resource itself, not just the exp
        // self.walk_tys(dep_graph, this, re.walk_locals());
        self.walk_lines(this, re.walk());
    }

    fn walk_lines<'a>(
        &mut self,
        this: MemberRef,
        lines: impl Iterator<Item = &'a ExpLine<'tcx>>,
    ) where
        'tcx: 'a,
    {
        for line in lines {
            self.walk_tys(this, line.ty.walk());
            match line.kind {
                ExpLineKind::Call(id, ..) => {
                    let function = &self.mmap.functions[id];
                    if let Some(pre) = &function.pre {
                        self.dep_graph.add_edge(MemberRef::Resource(pre.res), this, true);
                    }
                    // These will get removed by cycle-analysis if required
                    if let Some(post) = &function.post {
                        self.dep_graph.add_edge(MemberRef::Function(post.fun), this, false);
                    }
                    self.dep_graph.add_edge(MemberRef::Function(id), this, false);
                }
                ExpLineKind::Calling(_, m, ..) => {
                    let method = &self.mmap.methods[m];
                    if let Some(pre) = &method.pre {
                        self.dep_graph.add_edge(MemberRef::Resource(pre.res), this, true);
                    }
                    if let Some(post) = &method.post {
                        self.dep_graph.add_edge(MemberRef::Resource(post.res), this, true);
                    }
                }
                _ => (),
            }
        }
    }

    fn walk_tys(
        &mut self,
        this: MemberRef,
        tys: impl Iterator<Item = Ty<'tcx>>,
    ) {
        for ty in tys {
            if let TyKind::Domain(domain, ..) = ty.kind() {
                let ty = MemberRef::Type(*domain);
                self.dep_graph.add_edge(ty, this, true);
            }
        }
    }

    fn walk_stmts<'a>(
        &mut self,
        this: MemberRef,
        stmts: impl Iterator<Item = &'a Statement<'tcx>>,
    ) where
        'tcx: 'a,
    {
        for stmt in stmts {
            use StatementKind::*;
            match &stmt.kind {
                Eval(.., exp) => self.walk_lines(this, exp.walk()),
                MergeHeap(..) | Assign(..) => (),
                Call(_, method, _) | GhostCall(_, method, ..) => {
                    let m = &self.mmap.methods[*method];
                    if let Some(pre) = &m.pre {
                        self.dep_graph.add_edge(
                            MemberRef::Resource(pre.res),
                            this,
                            true,
                        );
                    }
                    if let Some(post) = &m.post {
                        self.dep_graph.add_edge(
                            MemberRef::Resource(post.res),
                            this,
                            true,
                        );
                    }
                }
                Ghost(.., re) => self.walk_resource_exp(this, re),
            }
        }
    }
}
