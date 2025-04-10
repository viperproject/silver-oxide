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
        let sig = self.fn_sig(id);
        let mut tcx = TranslationCtxt::new(self, id);
        use Declaration::*;
        match decl {
            Import(..) => Member::Import,
            Define(..) => Member::Define,
            Domain(..) => Member::Domain,
            DomainElement(crate::parse::DomainElement { kind: DomainElementKind::Axiom(a), .. }) => {
                let a = tcx.translate_exp(&a.exp.0, self.types.bool_);
                Member::DomainAxiom(a)
            }
            DomainElement(crate::parse::DomainElement { kind: DomainElementKind::Function(..), .. }) =>
                Member::DomainFunction,
            Field(..) => Member::Field,
            Function(f) => {
                let pre = tcx.translate_resource(&f.contract.precondition);
                tcx.add_return();
                tcx.set_heap();
                let post = tcx.translate_exp(&f.contract.postcondition, self.types.bool_);
                let body = f.body.as_ref().map(|b| {
                    let ty = tcx.fn_result();
                    tcx.translate_exp(&b.0, ty)
                });
                Member::Function(pre, post, body)
            }
            Predicate(p) => {
                let body = p.body.as_ref().map(|b| tcx.translate_resource(&b.0));
                Member::Predicate(body)
            }
            Method(m) => {
                let pre = tcx.translate_resource(&m.contract.precondition);
                tcx.add_return();
                let post = tcx.translate_resource(&m.contract.postcondition);
                let body = m.body.as_ref().map(|b| tcx.translate_body(b));
                Member::Method(pre, post, body)
            }
            Adt(..) => todo!(),
        }
    }
}
