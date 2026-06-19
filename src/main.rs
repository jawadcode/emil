use std::{env, fs, process};

use parser::{program::program, ParserState};
use semantic_analysis::Analyser;

mod ast;
mod lexer;
mod parser;
mod semantic_analysis;
mod utils;

fn main() {
    let mut args = env::args();
    let exec_name = args.next().unwrap();
    let source_path = {
        let help = format!("usage: {exec_name} <input file>");
        args.next().expect(&help)
    };

    let source_content = fs::read_to_string(&source_path).unwrap();

    let mut parser = ParserState::new(&source_content);

    match program(&mut parser) {
        Ok(program) => {
            println!("Parse Result:\n{:?}", program.clone());
            println!();
            let result = Analyser::new(parser.yeehaw()).analyse_program(&program.node);
            if let Err(err) = result {
                eprintln!("Analyser failed with: {err:?}");
            }
        }
        Err(err) => {
            eprintln!("{err}");
            process::exit(1)
        }
    }
}
