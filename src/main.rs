use std::{
    env,
    fmt::{self, Display},
    fs,
    ops::Range,
};

use ariadne::{Color, Label, Report, ReportKind, Source};

use parser::parse;
use utils::SpanInfo;

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
        Ok(program) => println!("result: {:?}", program.clone()),
        Err(errs) => {
            for err in errs {
                Report::build(ReportKind::Error, SpanInfo(source_path.clone(), err.span()))
                    .with_code(3)
                    .with_message(err.to_string())
                    .with_label(
                        Label::new(SpanInfo(source_path.clone(), err.span().into()))
                            .with_message(format!("{}", err.reason()))
                            .with_color(Color::Red),
                    )
                    .finish()
                    .print((source_path.clone(), Source::from(&source_content)))
                    .unwrap();
            }
        }
    }
}
