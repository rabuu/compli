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

use chumsky::error::RichReason;
use chumsky::prelude::*;

use crate::{ast, Span};

mod lexer;
mod parser;

pub use lexer::Token;

#[derive(Debug, Error, Diagnostic)]
pub enum ParsingError {
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

pub fn lex<'src>(source: &'src str) -> Result<Vec<(Token<'src>, Span)>, Vec<ParsingError>> {
    let (tokens, errs) = lexer::lexer()
        .parse(source.map_span(Into::into))
        .into_output_errors();

    if !errs.is_empty() {
        return Err(errs
            .into_iter()
            .map(|e| e.map_token(|c| c.to_string()))
            .map(build_error)
            .collect());
    }

    tokens.ok_or_else(Vec::new)
}

/// Parse compli source code into an AST
pub fn parse<'src, 'tok: 'src>(
    tokens: &'tok [(Token<'src>, Span)],
    eoi: usize,
) -> Result<ast::UntypedAst<'src>, Vec<ParsingError>> {
    let (ast, errs) = parser::parser()
        .parse(
            tokens.map(Span::marker(eoi), |(t, s)| (t, s)),
        )
        .into_output_errors();

    if !errs.is_empty() {
        return Err(errs
            .into_iter()
            .map(|e| e.map_token(|tok| tok.to_string()))
            .map(build_error)
            .collect());
    }

    ast.ok_or_else(Vec::new)
}

/// Turn a chumsky error into our error type
fn build_error(err: Rich<String, Span>) -> ParsingError {
    match err.reason() {
        RichReason::ExpectedFound { expected, found } => {
            let found: String = match found {
                None => "EOF".to_string(),
                Some(found) => found.to_string(),
            };

            let expected = if expected.len() == 0 {
                None
            } else {
                let toks: Vec<_> = expected.iter().map(ToString::to_string).collect();

                let mut help_string = toks.join(", ");
                help_string.insert_str(0, "Expected one of: ");

                Some(help_string)
            };

            ParsingError::UnexpectedInput {
                token: found,
                expected,
                span: *err.span(),
            }
        }
        RichReason::Custom(msg) => ParsingError::Custom {
            msg: msg.clone(),
            span: *err.span(),
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn program() {
        let src = r#"
def foo(): int = 1 def BAr_2(_: float): bool=     true &&  false
rec a = a: int,rec b=b:a,a:a
        "#;

        assert_eq!(
            parse(src).unwrap(),
            ast::Program {
                records: vec![
                    ast::Record {
                        name: String::from("a"),
                        fields: vec![(String::from("a"), ast::Type::Int)],
                        name_span: Span::new(70, 71),
                    },
                    ast::Record {
                        name: String::from("b"),
                        fields: vec![
                            (String::from("b"), ast::Type::Record(String::from("a"))),
                            (String::from("a"), ast::Type::Record(String::from("a"))),
                        ],
                        name_span: Span::new(85, 86),
                    }
                ],
                functions: vec![
                    ast::Function {
                        name: String::from("foo"),
                        body: ast::Expression {
                            kind: ast::ExpressionKind::Int(1),
                            span: Span::new(18, 19),
                            typ: ast::NoTypeContext,
                        },
                        params: vec![],
                        return_type: ast::Type::Int,
                        name_span: Span::new(5, 8),
                    },
                    ast::Function {
                        name: String::from("BAr_2"),
                        body: ast::Expression {
                            kind: ast::ExpressionKind::Binary {
                                op: ast::BinaryOperation::And,
                                lhs: Box::new(ast::Expression {
                                    kind: ast::ExpressionKind::Bool(true),
                                    span: Span::new(51, 55),
                                    typ: ast::NoTypeContext,
                                }),
                                rhs: Box::new(ast::Expression {
                                    kind: ast::ExpressionKind::Bool(false),
                                    span: Span::new(60, 65),
                                    typ: ast::NoTypeContext,
                                }),
                            },
                            span: Span::new(51, 65),
                            typ: ast::NoTypeContext,
                        },
                        params: vec![(String::from("_"), ast::Type::Float)],
                        return_type: ast::Type::Bool,
                        name_span: Span::new(24, 29),
                    }
                ]
            }
        )
    }

    #[test]
    fn simple_record() {
        let src = "rec foo=foo:foo";
        assert_eq!(
            parse(src).unwrap(),
            ast::Program {
                records: vec![ast::Record {
                    name: String::from("foo"),
                    fields: vec![(String::from("foo"), ast::Type::Record(String::from("foo")))],
                    name_span: Span::new(4, 7),
                }],
                functions: vec![]
            }
        );
    }

    #[test]
    #[should_panic]
    fn empty_record() {
        let src = "rec foo =";
        parse(src).unwrap();
    }

    #[test]
    #[should_panic]
    fn unclosed() {
        let src = "def foo(): int = (1 + 2";
        parse(src).unwrap();
    }

    #[test]
    #[should_panic]
    fn unopened() {
        let src = "def foo(): int = 1 + 2)";
        parse(src).unwrap();
    }

    #[test]
    fn unit_record() {
        let src = "rec foo";
        parse(src).unwrap();
    }

    #[test]
    #[should_panic]
    fn unit_record_wrong() {
        let src = "rec foo =";
        parse(src).unwrap();
    }
}
