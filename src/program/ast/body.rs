use core::fmt;

use crate::{program::{ast::newline, Const, DefId, Ty, TyWalker}, TiVec};

use super::{exp::Exp, idx::*, resource::ResourceExp};

#[derive(Default)]
pub struct Body<'tcx> {
    pub locals: TiVec<Local, Ty<'tcx>>,
    pub blocks: Vec<Block<'tcx>>,
}

#[derive(Default)]
pub struct Block<'tcx> {
    /// The disjunction of conjunctions that must be true to execute this block.
    pub conds: BlockConds<'tcx>,
    pub stmts: Vec<Statement<'tcx>>,
}

#[derive(Default)]
pub struct BlockConds<'tcx>(pub Vec<Vec<(Operand<'tcx>, bool)>>);

pub struct Statement<'tcx> {
    pub kind: StatementKind<'tcx>,
}

pub enum StatementKind<'tcx> {
    // x := fun(x) + 2 > 0 ? none : write
    Eval(Local, Exp<'tcx>),
    Havoc(Local),
    MergeHeap(Local, Local),
    // *x.f := y
    Assign(Local, Operand<'tcx>, Operand<'tcx>),
    // x, y := mthd(a, b)
    Call(Vec<Local>, DefId, Vec<Operand<'tcx>>),
    // inhale/exhale acc(pred(x, y), write)
    Ghost(Option<Local>, InhaleExhale, Local, ResourceExp<'tcx>),
    Predicate(FoldUnfold, Local, Operand<'tcx>, Operand<'tcx>),
}

#[derive(Debug, Clone, Copy)]
pub enum InhaleExhale {
    Inhale,
    Exhale,
}

#[derive(Debug, Clone, Copy)]
pub enum FoldUnfold {
    Fold,
    Unfold,
}

#[derive(Clone, Copy)]
pub struct Operand<'tcx> {
    pub ty: Ty<'tcx>,
    pub kind: OperandKind<'tcx>,
}

#[derive(Clone, Copy)]
pub enum OperandKind<'tcx> {
    Const(Const<'tcx>),
    Local(Local),
}

impl<'tcx> Operand<'tcx> {
    pub fn as_const(self) -> Option<Const<'tcx>> {
        match self.kind {
            OperandKind::Const(c) => Some(c),
            _ => None,
        }
    }
}

impl<'tcx> Body<'tcx> {
    pub fn walk<'a>(&'a self) -> StmtWalker<'a, 'tcx> {
        let mut walker = StmtWalker::default();
        walker.stack = self.blocks.iter();
        walker
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

// fmt

impl fmt::Debug for Body<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{:?}", self.locals)?;
        for block in &self.blocks {
            block.fmt(f)?;
            writeln!(f)?;
        }
        Ok(())
    }
}

impl fmt::Debug for Block<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} {{", self.conds)?;
        if !self.stmts.is_empty() {
            writeln!(f)?;
        }
        for stmt in &self.stmts {
            writeln!(f, "  {:?}", stmt.kind)?;
        }
        write!(f, "}}")?;
        Ok(())
    }
}

impl fmt::Debug for BlockConds<'_> {
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
                write!(f, "{local:?}")?;
            }
        }
        write!(f, ")")
    }
}

impl fmt::Debug for StatementKind<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use StatementKind::*;
        match self {
            Eval(local, exp) => {
                write!(f, "{local} :=")?;
                if exp.lines.len() > 1 {
                    newline(f)?;
                    write!(f, "    {exp:2?}")
                } else {
                    write!(f, " {exp:2?}")
                }
            }
            Havoc(local) => write!(f, "{local:?} := ?"),
            MergeHeap(to, from) => write!(f, "⟦{to:?}⟧ += ⟦{from:?}⟧"),
            Assign(heap, to, from) => write!(f, "*⟦{heap:?}⟧ {to:?} := {from:?}"),
            Call(locals, def_id, args) => {
                for (i, local) in locals.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{local:?}")?;
                }
                if !locals.is_empty() {
                    write!(f, " := ")?;
                }
                write!(f, "{def_id:?}(")?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{arg:?}")?;
                }
                write!(f, ")")
            }
            Ghost(local, mode, heap, resource) => {
                if let Some(local) = local {
                    write!(f, "{local:?} := ")?;
                }
                writeln!(f, "{mode:?} ⟦{heap:?}⟧")?;
                write!(f, "    {resource:2?}")
            }
            Predicate(mode, heap, resource, perm) => {
                write!(f, "{mode:?} ⟦{heap:?}⟧ acc({resource:?}, {perm:?})")
            }
        }
    }
}

impl fmt::Debug for Operand<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.kind {
            OperandKind::Const(c) => write!(f, "{c:?}"),
            OperandKind::Local(l) => write!(f, "{l:?}"),
        }
    }
}
