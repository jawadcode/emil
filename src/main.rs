use std::{env, fs};

use ariadne::{Color, Label, Report, ReportKind, Source};

use parser::parse;

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

    match parse(&source_content) {
        Ok(program) => println!("result: {program:?}"),
        Err(errs) => {
            for err in errs {
                Report::build(ReportKind::Error, (&source_path, err.span().into_range()))
                    .with_code(3)
                    .with_message(err.to_string())
                    .with_label(
                        Label::new((&source_path, err.span().into_range()))
                            .with_message(err.reason().to_string())
                            .with_color(Color::Red),
                    )
                    .finish()
                    .print((&source_path, Source::from(&source_content)))
                    .unwrap();
            }
        }
    }
}
