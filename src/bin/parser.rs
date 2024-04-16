use std::{fs::read_to_string, io, process::exit};

use pest::Parser;
use silver_oxide::*;

fn main() -> io::Result<()> {
    let mut failed = vec![];

    let total = std::env::args().count() - 1;
    for file in std::env::args().skip(1) {
        let Ok(contents) = read_to_string(file.clone()) else {
            continue;
        };
        // let  = Silver::parse(Rule::sil_program, &contents);

        let peg_parse = peg::silver_parser::sil_program(&contents);

        if let Err(e) = peg_parse {
            failed.push((file, format!("peg:  {e}")));
        }
    }

    if !failed.is_empty() {
        eprintln!("{}", failed[0].0);
        eprintln!("{}", failed[0].1);

        eprintln!("Failed to parse {} / {} files", failed.len(), total);
        exit(1);
    }
    Ok(())
}
