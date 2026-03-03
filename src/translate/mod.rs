mod dot;
mod preprocess;
pub mod member;
pub mod global;

pub use dot::*;
pub use preprocess::*;

use crate::{parse::Program, vmir::ty::TyCtxt};

impl TyCtxt<'_> {
    pub fn new(program: &mut Program) -> Result<Self, ProcessError> {
        let mut tcx = TyCtxt::default();
        let global = tcx.prepare_translate(program);
        // tcx.calculate_kinds(program);
        global.resolve_calls(program)?;
        global.desugar_program(program);
        // tcx.calculate_fn_sigs(program);
        global.calculate_members(program);
        tcx.members.dump_vmir(false);
        Ok(tcx)
    }

    // pub(super) fn calculate_kinds(&mut self, program: &Program) {
    //     let self_ = &mut **self;
    //     self_.globals.calculate_kinds(&self_.interner, program);
    // }
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
