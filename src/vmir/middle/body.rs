use core::fmt;

use crate::{
    parse::ConstKind, vmir::{
        ty::{Const, Ty, TyCtxt, TyWalker},
        DefId,
    }, AsBrackets, TiVec
};

use super::{exp::Exp, idx::*, newline, resource::ResourceExp, CallKind};

#[derive(Debug, Default)]
pub struct Body<'tcx> {
    pub locals: TiVec<Local, Ty<'tcx>>,
    pub blocks: Vec<Block<'tcx>>,
}

#[derive(Debug, Default)]
pub struct Block<'tcx> {
    /// The disjunction of conjunctions that must be true to execute this block.
    pub conds: BlockConds<'tcx>,
    pub temporaries: TiVec<Temporary, Ty<'tcx>>,
    pub stmts: Vec<Statement<'tcx>>,
}

#[derive(Debug, Default)]
pub struct BlockConds<'tcx>(pub Vec<Vec<(Operand<'tcx>, bool)>>);

#[derive(Debug)]
pub struct Statement<'tcx> {
    pub kind: StatementKind<'tcx>,
}

#[derive(Debug)]
pub enum StatementKind<'tcx> {
    // x := fun(x) + 2 > 0 ? none : write
    Eval(MutableTarget, Exp<'tcx>),
    MergeHeap(Local, Local),
    // *x.f := y
    Assign(MutableTarget, Operand<'tcx>, Operand<'tcx>),
    // x, y := mthd(a, b)
    Call(Vec<MutableTarget>, MethodId, Vec<Operand<'tcx>>),
    // inhale/exhale acc(pred(x, y), write)
    Ghost(Option<Local>, InhaleExhale, Local, ResourceExp<'tcx>),
    GhostCall(CallKind, MethodId, Local, Operand<'tcx>, Vec<Operand<'tcx>>),
}

#[derive(Debug, Clone, Copy)]
pub enum InhaleExhale {
    Inhale,
    Exhale,
}

#[derive(Debug, Clone, Copy)]
pub struct Operand<'tcx> {
    pub ty: Ty<'tcx>,
    pub kind: OperandKind<'tcx>,
}

impl<'tcx> Operand<'tcx> {
    pub fn as_const(self) -> Option<Const<'tcx>> {
        self.kind.as_const()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum OperandKind<'tcx> {
    Const(Const<'tcx>),
    Local(Local),
    Temporary(Temporary),
}

pub type MutableTarget = Result<Local, Temporary>;

impl<'tcx> OperandKind<'tcx> {
    pub fn as_const(self) -> Option<Const<'tcx>> {
        match self {
            OperandKind::Const(c) => Some(c),
            _ => None,
        }
    }
}

impl<'tcx> Body<'tcx> {
    pub fn walk<'a>(&'a self) -> StmtWalker<'a, 'tcx> {
        StmtWalker {
            stack: self.blocks.iter(),
            ..Default::default()
        }
    }

    pub fn walk_locals(&self) -> TyWalker<'tcx> {
        let mut walker = TyWalker::default();
        for &ty in self.locals.iter().rev() {
            walker.add_ty(ty);
        }
        walker
    }
}

#[derive(Default)]
pub struct StmtWalker<'a, 'tcx> {
    block: core::slice::Iter<'a, Statement<'tcx>>,
    stack: core::slice::Iter<'a, Block<'tcx>>,
}

impl<'a, 'tcx> Iterator for StmtWalker<'a, 'tcx> {
    type Item = &'a Statement<'tcx>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(stmt) = self.block.next() {
                return Some(stmt);
            }
            self.block = self.stack.next()?.stmts.iter();
        }
    }
}

impl<'tcx> TyCtxt<'tcx> {
    pub fn const_operand(&self, const_: ConstKind) -> Operand<'tcx> {
        let const_ = self.interner.mk_const(const_);
        let ty = self.const_ty(const_);
        let kind = OperandKind::Const(const_);
        Operand { ty, kind }
    }
}

// fmt

impl fmt::Display for Body<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}", self.locals.raw.bracketed())?;
        for block in &self.blocks {
            block.fmt(f)?;
            writeln!(f)?;
        }
        Ok(())
    }
}

impl fmt::Display for Block<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {{", self.conds)?;
        if !self.stmts.is_empty() {
            writeln!(f)?;
        }
        for stmt in &self.stmts {
            writeln!(f, "  {}", stmt.kind)?;
        }
        write!(f, "}}")?;
        Ok(())
    }
}

impl fmt::Display for BlockConds<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "if (")?;
        if self.0.is_empty() {
            write!(f, "false")?;
        }
        for (i, cond) in self.0.iter().enumerate() {
            if cond.is_empty() {
                assert_eq!(self.0.len(), 1);
                write!(f, "true")?;
            }
            if i > 0 {
                write!(f, " || ")?;
            }
            for (j, (local, neg)) in cond.iter().enumerate() {
                if j > 0 {
                    write!(f, " && ")?;
                }
                if *neg {
                    write!(f, "!")?;
                }
                write!(f, "{local}")?;
            }
        }
        write!(f, ")")
    }
}

fn fmt_result<T: fmt::Display, E: fmt::Display>(
    f: &mut fmt::Formatter<'_>,
    result: &Result<T, E>,
) -> fmt::Result {
    match result {
        Ok(ok) => ok.fmt(f),
        Err(err) => err.fmt(f),
    }
}

impl fmt::Display for StatementKind<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use StatementKind::*;
        match self {
            Eval(local, exp) => {
                fmt_result(f, local)?;
                write!(f, " :=")?;
                if exp.lines.len() > 1 {
                    newline(f)?;
                    write!(f, "    {exp:2}")
                } else {
                    write!(f, " {exp:2}")
                }
            }
            MergeHeap(to, from) => write!(f, "⟦{to}⟧ += ⟦{from}⟧"),
            Assign(heap, to, from) => {
                write!(f, "*⟦")?;
                fmt_result(f, heap)?;
                write!(f, "⟧ {to} := {from}")
            }
            Call(locals, def_id, args) => {
                for (i, local) in locals.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    fmt_result(f, local)?;
                }
                if !locals.is_empty() {
                    write!(f, " := ")?;
                }
                write!(f, "{def_id:?}(")?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{arg}")?;
                }
                write!(f, ")")
            }
            Ghost(local, mode, heap, resource) => {
                if let Some(local) = local {
                    write!(f, "{local} := ")?;
                }
                writeln!(f, "{mode:?} ⟦{heap}⟧")?;
                write!(f, "    {resource:2}")
            }
            GhostCall(kind, method, heap, perm, args) => {
                write!(f, "{kind} acc⟦{heap}⟧({method:?}{}, {perm})", args.parenthesised())
            }
        }
    }
}

impl fmt::Display for Operand<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.kind.fmt(f)
    }
}

impl fmt::Display for OperandKind<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            OperandKind::Const(c) => write!(f, "{c}"),
            OperandKind::Local(l) => write!(f, "{l}"),
            OperandKind::Temporary(t) => write!(f, "{t}"),
        }
    }
}
