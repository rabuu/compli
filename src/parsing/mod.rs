//! Parsing
//!
//! This module is responsible for turning the source code from its string form into an AST.
//! The main interface is [parse] which takes a string and generates an [ast::UntypedProgram].
//!
//! Internally, parsing works in two phases:
//! - the [lexer] scans the source text and turns it into a stream of tokens
//! - the [parser] generates the AST from these tokens
//!
//! The module relies heavily on the [chumsky] crate for lexing and parsing.

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

/// Parse compli source code into an AST
pub fn parse(source: &str) -> Result<ast::UntypedProgram, Vec<ParsingError>> {
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

/// Turn a chumsky error into our error type
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

#[cfg(test)]
mod tests {
    use crate::Type;

    use super::*;

    #[test]
    fn program() {
        let src = r#"
func foo(): int = 1 func BAr_2(_: float): bool=     true &&  false
        "#;

        assert_eq!(
            parse(src).unwrap(),
            ast::Program {
                functions: vec![
                    ast::Function {
                        name: String::from("foo"),
                        body: ast::Expression {
                            kind: ast::ExpressionKind::Int(1),
                            span: Span::new(19, 20),
                            type_context: ast::NoContext,
                        },
                        params: vec![],
                        return_type: Type::Int,
                        name_span: Span::new(6, 9),
                    },
                    ast::Function {
                        name: String::from("BAr_2"),
                        body: ast::Expression {
                            kind: ast::ExpressionKind::Binary {
                                op: ast::BinaryOperation::And,
                                lhs: Box::new(ast::Expression {
                                    kind: ast::ExpressionKind::Bool(true),
                                    span: Span::new(53, 57),
                                    type_context: ast::NoContext,
                                }),
                                rhs: Box::new(ast::Expression {
                                    kind: ast::ExpressionKind::Bool(false),
                                    span: Span::new(62, 67),
                                    type_context: ast::NoContext,
                                }),

                            },
                            span: Span::new(53, 67),
                            type_context: ast::NoContext,
                        },
                        params: vec![(String::from("_"), Type::Float)],
                        return_type: Type::Bool,
                        name_span: Span::new(26, 31),
                    }
                ]
            }
        )
    }

    #[test]
    #[should_panic]
    fn unclosed() {
        let src = "func foo(): int = (1 + 2";
        parse(src).unwrap();
    }

    #[test]
    #[should_panic]
    fn unopened() {
        let src = "func foo(): int = 1 + 2)";
        parse(src).unwrap();
    }
}
