use crate::{program::{DefId, Ty}, TiVec};

use super::{exp::Exp, idx::*, resource::ResourceExp};

#[derive(Debug, Default)]
pub struct Body<'tcx> {
    pub locals: TiVec<Local, Ty<'tcx>>,
    pub basic_blocks: TiVec<BasicBlock, BasicBlockData<'tcx>>,
}

#[derive(Debug)]
pub struct BasicBlockData<'tcx> {
    pub stmts: Vec<Statement<'tcx>>,
    pub terminator: Terminator,
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
    Ghost(UpdateMode, ResourceExp<'tcx>),
}

#[derive(Debug)]
pub struct Terminator {
    pub kind: TerminatorKind,
}

#[derive(Debug)]
pub enum TerminatorKind {
    Branch(Local, BasicBlock, BasicBlock, BasicBlock),
    GotoLoop(BasicBlock),
    Return,
}

#[derive(Debug, Clone, Copy)]
pub enum UpdateMode {
    AssumeInhale,
    AssertExhale,
}

#[derive(Debug, Clone, Copy)]
pub struct Location {
    pub block: BasicBlock,
    pub statement: usize,
}
