use crate::{parse::{BinOp, HeapUpdateOp, UnOp}, program::{Const, DefId, Ty}, TiVec};

use super::idx::*;

#[derive(Debug, Default)]
pub struct Exp<'tcx> {
    pub lines: TiVec<ExpLocal, ExpLine<'tcx>>,
}

impl<'tcx> Exp<'tcx> {
    pub fn result_ty(&self) -> Ty<'tcx> {
        self.lines.last().unwrap().ty
    }
}

#[derive(Debug)]
pub struct ExpLine<'tcx> {
    pub ty: Ty<'tcx>,
    pub kind: ExpLineKind<'tcx>,
}

#[derive(Debug)]
pub struct Location;

#[derive(Debug)]
pub enum ExpLineKind<'tcx> {
    Use(ExpOperand<'tcx>),
    Call(DefId, Vec<ExpOperand<'tcx>>),
    /// A deref or perm
    Heap(HeapOp, ExpOperand<'tcx>, ExpOperand<'tcx>),
    HeapUpdate(HeapUpdateOp, ExpOperand<'tcx>, ExpOperand<'tcx>, ExpOperand<'tcx>),
    Ternary(ExpOperand<'tcx>, Exp<'tcx>, Exp<'tcx>),
    UpdateHeap((), Exp<'tcx>), // TODO: unfolding/folding
    Quantifier((), Exp<'tcx>), // TODO: I'm thinking something like a closure

    UnOp(UnOp, ExpOperand<'tcx>),
    BinOp(BinOp, ExpOperand<'tcx>, ExpOperand<'tcx>),
}

#[derive(Debug, Clone, Copy)]
pub enum ExpOperand<'tcx> {
    Const(Const<'tcx>),
    /// A local from an earlier line, the u16 specifies how much nesting to go
    /// back up in the stack.
    ExpLocal(u16, ExpLocal),
    /// A function/method argument/result or a local variable
    Local(Local),
}

#[derive(Debug, Clone, Copy)]
pub enum HeapOp {
    Deref,
    Perm,
}
