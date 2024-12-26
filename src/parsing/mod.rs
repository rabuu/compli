use ariadne::{Color, Fmt, Label, Report, ReportKind};
use chumsky::error::SimpleReason;
use chumsky::{prelude::*, Stream};

use crate::{ast, Span};

mod lexer;
mod parser;

pub fn parse(source: &str, filename: String) -> Result<ast::Program, Vec<Report<(String, Span)>>> {
    let (tokens, lex_errs) = lexer::lex().parse_recovery(source);

    let parse_errs = if let Some(tokens) = tokens {
        let len = source.chars().count();
        let (program, parse_errs) =
            parser::parser().parse_recovery(Stream::from_iter(len..len + 1, tokens.into_iter()));

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
        .chain(parse_errs.into_iter().map(|e| e.map(|tok| tok.to_string())));

    Err(build_reports(errors, filename))
}

fn build_reports<E>(errors: E, filename: String) -> Vec<Report<'static, (String, Span)>>
where
    E: Iterator<Item = Simple<String>>,
{
    let mut reports = Vec::new();
    for e in errors {
        let report = Report::build(ReportKind::Error, (filename.clone(), e.span()));

        let report = match e.reason() {
            SimpleReason::Unclosed { span, delimiter } => report
                .with_message(format!(
                    "Unclosed delimiter {}",
                    delimiter.fg(Color::Yellow)
                ))
                .with_label(
                    Label::new((filename.clone(), span.clone()))
                        .with_message(format!(
                            "Unclosed delimiter {}",
                            delimiter.fg(Color::Yellow)
                        ))
                        .with_color(Color::Yellow),
                )
                .with_label(
                    Label::new((filename.clone(), span.clone()))
                        .with_message(format!(
                            "Must be closed before this {}",
                            e.found()
                                .unwrap_or(&"end of file".to_string())
                                .fg(Color::Red)
                        ))
                        .with_color(Color::Red),
                ),
            SimpleReason::Unexpected => report
                .with_message(format!(
                    "{}, expected {}",
                    if e.found().is_some() {
                        "Unexpected token in input"
                    } else {
                        "Unexpected end of input"
                    },
                    if e.expected().len() == 0 {
                        "something else".to_string()
                    } else {
                        e.expected()
                            .map(|expected| match expected {
                                Some(expected) => expected.to_string(),
                                None => "end of input".to_string(),
                            })
                            .collect::<Vec<_>>()
                            .join(", ")
                    }
                ))
                .with_label(
                    Label::new((filename.clone(), e.span()))
                        .with_message(format!(
                            "Unexpected token {}",
                            e.found()
                                .unwrap_or(&"end of file".to_string())
                                .fg(Color::Red)
                        ))
                        .with_color(Color::Red),
                ),
            SimpleReason::Custom(msg) => report.with_message(msg).with_label(
                Label::new((filename.clone(), e.span()))
                    .with_message(format!("{}", msg.fg(Color::Red)))
                    .with_color(Color::Red),
            ),
        };

        reports.push(report.finish());
    }

    reports
}
