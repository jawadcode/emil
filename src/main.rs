use std::{env, fs, process};

use parser::{ParserState, program::program};
use sema::Analyser;

mod ast;
mod lexer;
mod parser;
mod sema;
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
            let mut analyser = Analyser::new(parser.yeehaw());
            let result = analyser.check_program(&program.node);
            println!();
            if let Err(err) = result {
                analyser.print_error(err, &source_path, &source_content);
                process::exit(1)
            }
        }
        Err(err) => {
            parser.print_error(err, &source_path);
            process::exit(1)
        }
    }
}
