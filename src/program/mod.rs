mod ast;
mod ctx;
mod dot;
mod intern;
mod misc;
mod preprocess;
mod std;
pub mod translate;
mod ty;

pub use ast::*;
pub use ctx::*;
pub use dot::*;
pub use intern::*;
pub use misc::*;
pub use preprocess::*;
pub use std::*;
pub use ty::*;

use crate::parse::Program;

impl TyCtxt<'_> {
    pub fn new(program: &mut Program) -> Result<Self, ProcessError> {
        let mut tcx = TyCtxt::default();
        tcx.calculate_kinds(program);
        tcx.resolve_calls(program)?;
        tcx.desugar_program(program);
        tcx.calculate_fn_sigs(program);
        tcx.calculate_members(program);
        Ok(tcx)
    }
}

#[derive(Debug)]
pub enum ProcessError {
    ResolveError(Vec<ResolveError>),
}

impl From<Vec<ResolveError>> for ProcessError {
    fn from(e: Vec<ResolveError>) -> Self {
        ProcessError::ResolveError(e)
    }
}
