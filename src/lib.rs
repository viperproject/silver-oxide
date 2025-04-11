use program::{ProcessError, TyCtxt};

pub mod analysis;
pub mod parse;
pub mod program;

pub struct Silver<'tcx> {
    pub program: parse::Program,
    pub tcx: TyCtxt<'tcx>,
}

pub fn full(input: &str) -> Result<Silver, SilverError> {
    let mut program = parse::silver_parser::sil_program(input)?;
    parse::Macro::inline_macros(&mut program);

    let tcx = TyCtxt::new(&mut program)?;

    Ok(Silver {
        program,
        tcx,
    })
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

type NonMaxU32 = nonmax::NonMaxU32;
type TiVec<K, V> = typed_index_collections::TiVec<K, V>;
type HashMap<K, V> = indexmap::IndexMap<K, V>;
type HashSet<K> = indexmap::IndexSet<K>;
