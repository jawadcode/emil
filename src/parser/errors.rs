use ariadne::{Config, IndexType, Label, LabelAttach, Report, ReportKind, Source};

use crate::{lexer::Token, parser::ParserState, utils::Spanned};

pub struct SyntaxError {
    pub expected: String,
    pub got: Token,
}

pub type SpanParseResult<T> = Result<Spanned<T>, SyntaxError>;

pub type ParseResult<T> = Result<T, SyntaxError>;

impl ParserState<'_> {
    pub fn print_error(&self, err: SyntaxError, file_path: &str) {
        Report::build(ReportKind::Error, (file_path, err.got.span.to_range()))
            .with_message("Syntax Error - Unexpected token")
            .with_label(Label::new((file_path, err.got.span.to_range())).with_message(format!(
                "Syntax Error: Got {}, expected {}",
                err.got.node, err.expected
            )))
            .with_config(
                Config::new()
                    .with_index_type(IndexType::Byte)
                    .with_label_attach(LabelAttach::Start),
            )
            .finish()
            .eprint((file_path, Source::from(self.source)))
            .unwrap()
    }
}
