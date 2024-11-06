use std::{env, fs};

use lexer::Lexer;

mod lexer;
mod utils;

fn main() {
    let mut args = env::args();
    let exec_name = args.next().unwrap();
    let source_path = {
        let help = format!("usage: {exec_name} <input file>");
        args.next().expect(&help)
    };

    let source_content = fs::read_to_string(source_path).unwrap();
    let tokens = Lexer::new(&source_content).collect::<Vec<_>>();
    println!("{tokens:?}");
}
