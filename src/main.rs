use std::{env, fs, process};

use parser::{program::program, Parser};

mod lexer;
mod parser;
mod utils;

fn main() {
    let mut args = env::args();
    let exec_name = args.next().unwrap();
    let source_path = {
        let help = format!("usage: {exec_name} <input file>");
        args.next().expect(&help)
    };

    let source_content = fs::read_to_string(&source_path).unwrap();

    let mut parser = Parser::new(&source_content);

    match program(&mut parser) {
        Ok(program) => println!("result: {:?}", program.clone()),
        Err(err) => {
            eprintln!("{err}");
            process::exit(1)
        }
    }
}
