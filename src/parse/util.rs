use super::*;

impl Program {
    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn iter(&self) -> impl Iterator<Item = (crate::program::LocalDefId, &Declaration)> {
        self.0
            .iter()
            .enumerate()
            .map(|(id, decl)| (id.into(), decl))
    }

    pub fn iter_mut(
        &mut self,
    ) -> impl Iterator<Item = (crate::program::LocalDefId, &mut Declaration)> {
        self.0
            .iter_mut()
            .enumerate()
            .map(|(id, decl)| (id.into(), decl))
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

impl HeapExp {
    pub(crate) fn new(exp: Exp) -> Self {
        Self { res: vec![], exp }
    }

    pub(super) fn conjoin(exp: Vec<Exp>) -> Self {
        let init = ExpKind::bool(true);
        let exp = exp.into_iter().fold(init, ExpKind::conjoin);
        Self::new(exp)
    }
}

impl From<Vec<PrePostDec>> for Contract {
    fn from(value: Vec<PrePostDec>) -> Self {
        let mut precondition = ExpKind::bool(true);
        let mut decreases = vec![];
        for p in value {
            match p {
                PrePostDec::Pre(e) => precondition = ExpKind::conjoin(precondition, e),
                PrePostDec::Decreases(d) => decreases.push(d),
                _ => {}
            }
        }
        let postcondition = HeapExp::new(ExpKind::bool(true));
        Self {
            precondition: HeapExp::new(precondition),
            postcondition,
            decreases,
        }
    }
}

impl Contract {
    pub(super) fn add_posts(mut self, posts: Vec<PrePostDec>) -> Self {
        for p in posts {
            match p {
                PrePostDec::Post(e) => {
                    *self.postcondition.exp = match *self.postcondition.exp {
                        ExpKind::Const(ConstKind::Bool(true)) => *e,
                        post => ExpKind::BinOp(BinOp::And, Box::new(post), e),
                    }
                }
                PrePostDec::Decreases(d) => self.decreases.push(d),
                _ => {}
            }
        }
        self
    }
}

impl<T> Block<T> {
    pub(super) fn map<U>(self, f: impl FnOnce(T) -> U) -> Block<U> {
        Block(f(self.0))
    }
}

impl ExpKind {
    fn conjoin(mut acc: Exp, new: Exp) -> Exp {
        *acc = match *acc {
            ExpKind::Const(ConstKind::Bool(true)) => *new,
            other => ExpKind::BinOp(BinOp::And, Box::new(other), new),
        };
        acc
    }
}

impl HeapExp {
    pub fn is_pure(&self) -> bool {
        self.res.is_empty()
    }
}
