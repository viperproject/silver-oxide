use crate::{parse::{Declaration, DomainElementKind, Program}, program::{translate::TranslationCtxt, TyCtxt}, TiVec};

use super::{exp::Exp, idx::LocalDefId, resource::ResourceExp, body::Body};

#[derive(Debug, Default)]
pub struct Members<'tcx> {
    pub(crate) bodies: TiVec<LocalDefId, Member<'tcx>>,
}

#[derive(Debug)]
pub enum Member<'tcx> {
    Import,
    Define,
    Domain,
    DomainFunction,
    DomainAxiom(Exp<'tcx>),
    Field,
    Predicate(Option<ResourceExp<'tcx>>),
    Function(ResourceExp<'tcx>, Exp<'tcx>, Option<Exp<'tcx>>),
    Method(ResourceExp<'tcx>, ResourceExp<'tcx>, Option<Body<'tcx>>),
}

impl<'tcx> TyCtxt<'tcx> {
    pub(crate) fn calculate_members(&mut self, program: &Program) {
        assert_eq!(self.members.bodies.len(), 0);
        self.members.bodies.reserve_exact(program.len());
        for (id, decl) in program.iter() {
            let member = self.calculate_member(id, decl);
            let new = self.members.bodies.push_and_get_key(member);
            assert_eq!(new, id);
        }
    }

    fn calculate_member(&self, id: LocalDefId, decl: &Declaration) -> Member<'tcx> {
        let mut tcx = TranslationCtxt::new(self, id);
        use Declaration::*;
        match decl {
            Import(..) => Member::Import,
            Define(..) => Member::Define,
            Domain(..) => Member::Domain,
            DomainElement(crate::parse::DomainElement { kind: DomainElementKind::Axiom(ax), .. }) => {
                let a = tcx.translate_exp(&ax.exp.0, self.types.bool_, false);
                eprintln!("[Translate] axiom {:?}\n{a:?}", ax.name);
                Member::DomainAxiom(a)
            }
            DomainElement(crate::parse::DomainElement { kind: DomainElementKind::Function(..), .. }) =>
                Member::DomainFunction,
            Field(..) => Member::Field,
            Function(f) => {
                let have_heap = !f.contract.precondition.is_pure();
                let pre = tcx.translate_resource(&f.contract.precondition, None);
                eprintln!("[Translate] fn pre {:?}\n{pre:?}", f.signature.name.0.0);
                tcx.add_return();
                let post = tcx.translate_exp(&f.contract.postcondition.exp, self.types.bool_, have_heap);
                eprintln!("[Translate] fn post {:?}\n{post:?}", f.signature.name.0.0);
                let body = f.body.as_ref().map(|b| {
                    let ty = tcx.fn_result();
                    let body = tcx.translate_exp(&b.0, ty, have_heap);
                    eprintln!("[Translate] fn body {:?}\n{body:?}", f.signature.name.0.0);
                    body
                });
                Member::Function(pre, post, body)
            }
            Predicate(p) => {
                let body = p.body.as_ref().map(|b| tcx.translate_resource(&b.0, None));
                if let Some(body) = &body {
                    eprintln!("[Translate] predicate {:?}\n{body:?}", p.signature.name.0.0);
                }
                Member::Predicate(body)
            }
            Method(m) => {
                let pre = tcx.translate_resource(&m.contract.precondition, None);
                eprintln!("[Translate] method pre {:?}\n{pre:?}", m.signature.name.0.0);
                tcx.add_return();
                let post = tcx.translate_resource(&m.contract.postcondition, None);
                eprintln!("[Translate] method post {:?}\n{post:?}", m.signature.name.0.0);
                let body = m.body.as_ref().map(|b| tcx.translate_body(b));
                Member::Method(pre, post, body)
            }
            Adt(..) => todo!(),
        }
    }
}
