use miette::Diagnostic;
use thiserror::Error;

use chumsky::error::SimpleReason;
use chumsky::{prelude::*, Stream};

use crate::{ast, Span};

mod lexer;
mod parser;

type ParseErr<T> = Simple<T, Span>;

#[derive(Debug, Error, Diagnostic)]
pub enum ParsingError {
    #[error("Found an unclosed delimiter: {delimiter}")]
    #[diagnostic(help("Must be closed before: {must_close_before}"))]
    UnclosedDelimiter {
        delimiter: String,
        must_close_before: String,

        #[label("unclosed delimiter")]
        span: Span,
    },

    #[error("Encountered unexpected input: {token}")]
    UnexpectedInput {
        token: String,

        #[help]
        expected: Option<String>,

        #[label("here")]
        span: Span,
    },

    #[error("{msg}")]
    Custom {
        msg: String,

        #[label("here")]
        span: Span,
    },
}

pub fn parse(source: &str) -> Result<ast::Program<()>, Vec<ParsingError>> {
    let end_of_input = Span::marker(source.chars().count());

    let char_iter = source
        .chars()
        .enumerate()
        .map(|(i, c)| (c, Span::single(i)));

    let (tokens, lex_errs) =
        lexer::lex().parse_recovery(Stream::from_iter(end_of_input, char_iter));

    let parse_errs = if let Some(tokens) = tokens {
        let (program, parse_errs) =
            parser::parser().parse_recovery(Stream::from_iter(end_of_input, tokens.into_iter()));

        if let Some(program) = program.filter(|_| lex_errs.len() + parse_errs.len() == 0) {
            return Ok(program);
        }

        parse_errs
    } else {
        Vec::new()
    };

    let errors = lex_errs
        .into_iter()
        .map(|e| e.map(|c| c.to_string()))
        .chain(parse_errs.into_iter().map(|e| e.map(|tok| tok.to_string())))
        .map(build_error)
        .collect();

    Err(errors)
}

fn build_error(err: ParseErr<String>) -> ParsingError {
    let eof = String::from("end of file");
    match err.reason() {
        SimpleReason::Unexpected => {
            let token = err.found().unwrap_or(&eof);
            let expected = if err.expected().len() == 0 {
                None
            } else {
                let toks: Vec<_> = err
                    .expected()
                    .map(|tok| match tok {
                        Some(tok) => tok.to_string(),
                        None => eof.clone(),
                    })
                    .collect();

                let mut help_string = toks.join(", ");
                help_string.insert_str(0, "Expected one of: ");

                Some(help_string)
            };

            ParsingError::UnexpectedInput {
                token: token.clone(),
                expected,
                span: err.span(),
            }
        }
        SimpleReason::Unclosed { span, delimiter } => {
            let must_close_before = err.found().unwrap_or(&eof);
            ParsingError::UnclosedDelimiter {
                delimiter: delimiter.clone(),
                must_close_before: must_close_before.clone(),
                span: *span,
            }
        }
        SimpleReason::Custom(msg) => ParsingError::Custom {
            msg: msg.clone(),
            span: err.span(),
        },
    }
}
