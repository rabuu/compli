use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};
use chumsky::error::SimpleReason;
use chumsky::prelude::*;

use crate::ast;
use lexer::Token;

mod lexer;

pub fn parse(text: &str) {
    let (tokens, mut errs) = lexer::lex().parse_recovery(text);

    for e in errs.into_iter().map(|e| e.map(|c| c.to_string())) {
        let report = Report::build(ReportKind::Error, e.span());

        let report = match e.reason() {
            SimpleReason::Unclosed { span, delimiter } => report
                .with_message(format!(
                    "Unclosed delimiter {}",
                    delimiter.fg(Color::Yellow)
                ))
                .with_label(
                    Label::new(span.clone())
                        .with_message(format!(
                            "Unclosed delimiter {}",
                            delimiter.fg(Color::Yellow)
                        ))
                        .with_color(Color::Yellow),
                )
                .with_label(
                    Label::new(e.span())
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
                    Label::new(e.span())
                        .with_message(format!(
                            "Unexpected token {}",
                            e.found()
                                .unwrap_or(&"end of file".to_string())
                                .fg(Color::Red)
                        ))
                        .with_color(Color::Red),
                ),
            SimpleReason::Custom(msg) => report.with_message(msg).with_label(
                Label::new(e.span())
                    .with_message(format!("{}", msg.fg(Color::Red)))
                    .with_color(Color::Red),
            ),
        };

        report.finish().print(Source::from(text)).unwrap();
    }

    println!("TOKENS: {tokens:#?}");
}

fn parser() -> impl Parser<Token, ast::Expression, Error = Simple<Token>> + Clone {
    let expr = recursive(|expr| {
        let val = select! {
            Token::Int(x) => ast::Expression::Int(x),
            Token::Bool(x) => ast::Expression::Bool(x),
        }
        .labelled("value");

        let ident = select! { Token::Ident(ident) => ident }.labelled("identifier");

        let var = ident.map(ast::Expression::Var);

        let items = expr
            .clone()
            .separated_by(just(Token::Comma))
            .allow_trailing();

        let call = ident
            .then(items.delimited_by(just(Token::ParenOpen), just(Token::ParenClose)))
            .map(|(function, args)| ast::Expression::Call { function, args });

        let atom = choice((val, call, var, expr.clone().delimited_by(just(Token::ParenOpen), just(Token::ParenClose))));

        let unary = just(Token::Minus)
            .to(ast::UnaOpKind::Neg)
            .or_not()
            .then(atom)
            .map(|(op, inner)| {
                if let Some(op) = op {
                    ast::Expression::UnaOp {
                        kind: op,
                        inner: Box::new(inner),
                    }
                } else {
                    inner
                }
            });

        let sum_or_diff = unary
            .clone()
            .then(
                just(Token::Plus)
                    .to(ast::BinOpKind::Add)
                    .or(just(Token::Minus).to(ast::BinOpKind::Sub))
                    .then(unary)
                    .repeated(),
            )
            .foldl(|lhs, (kind, rhs)| ast::Expression::BinOp {
                kind,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            });

        let comparison = sum_or_diff
            .clone()
            .then(
                just(Token::Equals)
                    .to(ast::BinOpKind::Equals)
                    .or(just(Token::Less).to(ast::BinOpKind::Less))
                    .then(sum_or_diff)
                    .repeated(),
            )
            .foldl(|lhs, (kind, rhs)| ast::Expression::BinOp {
                kind,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            });

        let and = comparison
            .clone()
            .then(
                just(Token::And)
                    .to(ast::BinOpKind::And)
                    .then(comparison)
                    .repeated(),
            )
            .foldl(|lhs, (kind, rhs)| ast::Expression::BinOp {
                kind,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            });

        #[allow(clippy::let_and_return)]
        and
    });

    expr
}
