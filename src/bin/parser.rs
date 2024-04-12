use std::{fs::read_to_string, io, process::exit};

use pest::Parser;
use silver_oxide::*;

fn main() -> io::Result<()> {
    let mut failed = vec![];

    let total = std::env::args().count();
    for file in std::env::args().skip(1) {
        let file = read_to_string(file)?;
        let parse = Silver::parse(Rule::sil_program, &file);

        if let Err(e) = parse {
          failed.push(e);
        }
    }

    if !failed.is_empty() {
      if failed.len() == 1 {
        eprintln!("{}", failed[0]);
      } else {
        eprintln!("Failed to parse {} / {} files", failed.len(), total);
      }
      exit(1);
    }
    Ok(())
}
