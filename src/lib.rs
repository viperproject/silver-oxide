use program::{ResolveError, TyCtxt};

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

    let mut tcx = TyCtxt::default();
    tcx.calculate_fn_sigs(&program);
    tcx.resolve_calls(&program)?;
    tcx.desugar_program(&mut program);
    tcx.calculate_members(&program);

    Ok(Silver {
        program,
        tcx,
    })
}

type PegErr = peg::error::ParseError<<str as peg::Parse>::PositionRepr>;

#[derive(Debug)]
pub enum SilverError {
    ParseError(PegErr),
    ResolveError(Vec<ResolveError>),
}

impl From<PegErr> for SilverError {
    fn from(e: PegErr) -> Self {
        SilverError::ParseError(e)
    }
}

impl From<Vec<ResolveError>> for SilverError {
    fn from(e: Vec<ResolveError>) -> Self {
        SilverError::ResolveError(e)
    }
}

type NonMaxU32 = nonmax::NonMaxU32;
type TiVec<K, V> = typed_index_collections::TiVec<K, V>;
