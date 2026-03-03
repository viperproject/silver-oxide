pub mod parse;
pub mod translate;
mod util;
pub mod vmir;

use translate::ProcessError;
pub use util::*;
use vmir::ty::TyCtxt;

pub struct Silver<'tcx> {
    pub program: parse::Program,
    pub tcx: TyCtxt<'tcx>,
}

pub fn full(input: &str) -> Result<Silver, SilverError> {
    let mut program = parse::silver_parser::sil_program(input)?;
    parse::Macro::inline_macros(&mut program);

    let tcx = TyCtxt::new(&mut program)?;

    Ok(Silver { program, tcx })
}

type PegErr = peg::error::ParseError<<str as peg::Parse>::PositionRepr>;

#[derive(Debug)]
pub enum SilverError {
    ParseError(PegErr),
    ProcessError(ProcessError),
}

impl From<PegErr> for SilverError {
    fn from(e: PegErr) -> Self {
        SilverError::ParseError(e)
    }
}

impl From<ProcessError> for SilverError {
    fn from(e: ProcessError) -> Self {
        SilverError::ProcessError(e)
    }
}
