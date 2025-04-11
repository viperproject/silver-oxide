mod ast;
mod ty;
mod misc;
mod intern;
mod ctx;
mod std;
mod preprocess;
pub mod translate;

pub use ast::*;
pub use ty::*;
pub use misc::*;
pub use intern::*;
pub use ctx::*;
pub use std::*;
pub use preprocess::*;

use crate::parse::Program;

impl<'tcx> TyCtxt<'tcx> {
    pub fn new(program: &mut Program) -> Result<Self, ProcessError> {
        let mut tcx = TyCtxt::default();
        tcx.calculate_kinds(&program);
        tcx.resolve_calls(&program)?;
        tcx.desugar_program(program);
        tcx.calculate_fn_sigs(&program);
        tcx.calculate_members(&program);
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
