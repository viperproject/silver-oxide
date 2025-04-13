use crate::{program::{DefId, Ty}, TiVec};

use super::{exp::Exp, idx::*, resource::ResourceExp};

#[derive(Debug, Default)]
pub struct Body<'tcx> {
    pub locals: TiVec<Local, Ty<'tcx>>,
    pub blocks: Vec<Block<'tcx>>,
}

#[derive(Debug)]
pub struct Block<'tcx> {
    /// The disjunction of conjunctions that must be true to execute this block.
    pub conds: Vec<Vec<Local>>,
    pub stmts: Vec<Statement<'tcx>>,
}

#[derive(Debug)]
pub struct Statement<'tcx> {
    pub kind: StatementKind<'tcx>,
}

#[derive(Debug)]
pub enum StatementKind<'tcx> {
    // x := fun(x) + 2 > 0 ? none : write
    Eval(Local, Exp<'tcx>),
    // x.f := y
    Assign(Local, DefId, Local),
    // x, y := mthd(a, b)
    Call(Vec<Local>, DefId, Vec<Local>),
    // inhale/exhale acc(pred(x, y), write)
    Ghost(Local, UpdateMode, ResourceExp<'tcx>),
}

#[derive(Debug, Clone, Copy)]
pub enum UpdateMode {
    Inhale,
    Exhale,
}
