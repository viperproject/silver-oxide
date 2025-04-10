use super::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BuiltinType {
    Seq,
    Set,
    Multiset,
    Map,
}

impl Ident {
    pub fn set() -> Self {
        Self("Set".to_string())
    }

    pub fn multiset() -> Self {
        Self("Multiset".to_string())
    }

    pub fn seq() -> Self {
        Self("Seq".to_string())
    }

    pub fn map() -> Self {
        Self("Map".to_string())
    }

    pub fn builtin_type(&self) -> Option<BuiltinType> {
        let bt = match self.0.as_str() {
            "Set" => BuiltinType::Set,
            "Multiset" => BuiltinType::Multiset,
            "Seq" => BuiltinType::Seq,
            "Map" => BuiltinType::Map,
            _ => return None,
        };
        Some(bt)
    }
}

impl ConstKind {
    pub fn bool(b: bool) -> Exp {
        Box::new(ExpKind::Const(ConstKind::Bool(b)))
    }

    pub fn write() -> Exp {
        Box::new(ExpKind::Const(ConstKind::Write))
    }

    pub fn none() -> Exp {
        Box::new(ExpKind::Const(ConstKind::None))
    }

    pub fn wildcard() -> Exp {
        Box::new(ExpKind::Const(ConstKind::Wildcard))
    }
}

impl Signature {
    pub fn field(f: IdnDeclTyped) -> Self {
        Self { name: f.idn, args: vec![ArgOrType::Type(Type::Ref)], ret: vec![ArgOrType::Type(f.ty)] }
    }
}
