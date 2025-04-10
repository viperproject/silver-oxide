use super::*;

impl Program {
    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn iter(&self) -> impl Iterator<Item = (crate::program::idx::LocalDefId, &Declaration)> {
        self.0.iter().enumerate().map(|(id, decl)| (id.into(), decl))
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = (crate::program::idx::LocalDefId, &mut Declaration)> {
        self.0.iter_mut().enumerate().map(|(id, decl)| (id.into(), decl))
    }
}

impl Declaration {
    pub fn contract(&self) -> Option<&Contract> {
        match self {
            Declaration::Function(f) => Some(&f.contract),
            Declaration::Method(m) => Some(&m.contract),
            _ => None,
        }
    }

    pub fn signature(&self) -> Option<&Signature> {
        use Declaration::*;
        match self {
            Function(f) => Some(&f.signature),
            Method(m) => Some(&m.signature),
            Predicate(p) => Some(&p.signature),
            DomainElement(e) => match &e.kind {
                DomainElementKind::Function(f) => Some(&f.signature),
                DomainElementKind::Axiom(_) => None,
            },
            Field(f) => Some(&f.0),
            Import(_) | Define(_) | Domain(_) | Adt(_) => None,
        }
    }

    pub fn idn_decl(&self) -> Option<&IdnDecl> {
        use Declaration::*;
        match self {
            Function(f) => Some(&f.signature.name),
            Method(m) => Some(&m.signature.name),
            Predicate(p) => Some(&p.signature.name),
            DomainElement(e) => match &e.kind {
                DomainElementKind::Function(f) => Some(&f.signature.name),
                DomainElementKind::Axiom(a) => a.name.as_ref(),
            },
            Import(_) => None,
            Define(d) => Some(&d.name),
            Domain(d) => Some(&d.name),
            Field(f) => Some(&f.0.name),
            Adt(a) => Some(&a.name),
        }
    }
}
