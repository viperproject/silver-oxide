use core::fmt;

use typed_index_collections::TiSlice;

use crate::{vmir::{middle::{Body, Exp, FunctionId, Local, MethodId, ResourceExp, ResourceId}, ty::{DomainKind, Interned, ParamTy, Ty, TyList}, DefId, FnSig, Locals, MethodSig, Symbol}, AsBrackets, TiVec};

use super::LocalDefId;

pub struct Axiom<'tcx> {
    pub associated: DomainKind,
    pub body: Exp<'tcx>,
}

#[derive(Debug)]
pub struct Resource<'tcx> {
    pub id: LocalDefId,
    pub nested_in: Option<MethodId>,
    pub snap: Ty<'tcx>,
    pub locals: Locals<'tcx>,
    pub pre: Option<ResourceCall>,
    pub post: Option<FunctionCall>,
    pub body: Option<ResourceExp<'tcx>>,
}

#[derive(Debug)]
pub struct Function<'tcx> {
    pub id: LocalDefId,
    pub nested_in: Option<MethodId>,
    pub ty_params: Box<[ParamTy<'tcx>]>,
    pub locals: Locals<'tcx>,
    pub pre: Option<ResourceCall>,
    pub post: Option<FunctionCall>,
    pub body: Option<Exp<'tcx>>,
}

impl<'tcx> Function<'tcx> {
    pub fn sig(&self) -> FnSig<'tcx> {
        FnSig {
            locals: self.locals,
            ty_params: self.ty_params.len(),
            heapless: self.heapless(),
        }
    }

    pub fn heap_pre(&self) -> Option<(ResourceId, Ty<'tcx>)> {
        self.pre.as_ref().map(|call| (call.res, *self.locals.last().unwrap()))
    }

    pub fn heapless(&self) -> bool {
        self.pre.is_none()
    }
}

#[derive(Debug)]
pub struct Method<'tcx> {
    pub id: LocalDefId,
    pub nested_in: Option<MethodId>,
    pub ghost: bool,
    pub params: u32,
    pub locals: Locals<'tcx>,
    pub pre: Option<ResourceCall>,
    pub post: Option<ResourceCall>,
    pub body: Option<Body<'tcx>>,
}

impl<'tcx> Method<'tcx> {
    pub fn sig(&self) -> MethodSig<'tcx> {
        MethodSig {
            locals: self.locals,
            params: self.params,
            ty_params: 0, // Methods do not have type parameters in this context
        }
    }
}

#[derive(Debug, Clone)]
pub struct ResourceCall {
    pub res: ResourceId,
    pub args: Vec<Local>,
}

#[derive(Debug, Clone)]
pub struct FunctionCall {
    pub fun: FunctionId,
    pub args: Vec<Local>,
}

impl fmt::Display for Axiom<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "axiom {{{{ {} }}}} {{", self.associated)?;
        writeln!(f, "\n  {:1}", self.body)?;
        writeln!(f, "}}")
    }
}

impl fmt::Display for Resource<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "resource {}{}", self.id, self.locals.parenthesised())?;
        if let Some(pre) = &self.pre {
            write!(f, "\n  requires {pre}")?;
        }
        if let Some(post) = &self.post {
            write!(f, "\n  ensures {post}")?;
        }
        if let Some(body) = &self.body {
            writeln!(f, " {{")?;
            writeln!(f, "  {body:1}")?;
            write!(f, "}}")?;
        }
        writeln!(f)
    }
}

impl fmt::Display for Function<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // TODO: return type
        write!(f, "function {}{}", self.id, self.locals.parenthesised())?;
        if let Some(pre) = &self.pre {
            write!(f, "\n  requires {pre}")?;
        }
        if let Some(post) = &self.post {
            write!(f, "\n  ensures {post}")?;
        }
        if let Some(body) = &self.body {
            writeln!(f, " {{")?;
            writeln!(f, "  {body:1}")?;
            write!(f, "}}")?;
        }
        writeln!(f)
    }
}

impl fmt::Display for Method<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.ghost {
            write!(f, "ghost ")?;
        }
        write!(f, "method {}{}", self.id, self.locals.parenthesised())?;
        if let Some(pre) = &self.pre {
            write!(f, "\n  requires {pre}")?;
        }
        if let Some(post) = &self.post {
            write!(f, "\n  ensures {post}")?;
        }
        if let Some(body) = &self.body {
            writeln!(f, " {{")?;
            writeln!(f, "  {body:1}")?;
            write!(f, "}}")?;
        }
        writeln!(f)
    }
}

impl fmt::Display for ResourceCall {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.res, self.args.parenthesised())
    }
}

impl fmt::Display for FunctionCall {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.fun, self.args.parenthesised())
    }
}
