use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};
use chumsky::error::SimpleReason;
use chumsky::{prelude::*, Stream};

use crate::{ast, Span, Spanned};
use lexer::Token;

mod lexer;

pub fn parse(text: &str) {
    let (tokens, errs) = lexer::lex().parse_recovery(text);

    let parse_errs = if let Some(tokens) = tokens {
        let len = text.chars().count();
        let (ast, parse_errs) =
            parser().parse_recovery(Stream::from_iter(len..len + 1, tokens.into_iter()));

        if let Some(ast) = ast.filter(|_| errs.len() + parse_errs.len() == 0) {
            println!("SUCESS:\n{ast:#?}");
        }

        parse_errs
    } else {
        Vec::new()
    };

    for e in errs
        .into_iter()
        .map(|e| e.map(|c| c.to_string()))
        .chain(parse_errs.into_iter().map(|e| e.map(|tok| tok.to_string())))
    {
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

        report.finish().eprint(Source::from(text)).unwrap();
    }
}

fn parser() -> impl Parser<Token, ast::Program, Error = Simple<Token>> + Clone {
    let ident = select! { Token::Ident(ident) => ident }.labelled("identifier");

    let expr = recursive(
        |expr: chumsky::recursive::Recursive<Token, Spanned<ast::Expression>, Simple<Token>>| {
            let val = select! {
                Token::Int(x) => ast::Expression::Int(x),
                Token::Bool(x) => ast::Expression::Bool(x),
            }
            .labelled("value");

            let var = ident.map(ast::Expression::Var);

            let items = expr
                .clone()
                .separated_by(just(Token::Comma))
                .allow_trailing();

            let call = ident
                .then(items.delimited_by(just(Token::ParenOpen), just(Token::ParenClose)))
                .map(|(function, args)| ast::Expression::Call { function, args });

            let atom = val
                .or(call)
                .or(var)
                .map_with_span(|e, span: Span| (e, span))
                .or(expr
                    .clone()
                    .delimited_by(just(Token::ParenOpen), just(Token::ParenClose)));

            let unary = just(Token::Minus)
                .to(ast::UnaOpKind::Neg)
                .map_with_span(|e, span: Span| (e, span))
                .or_not()
                .then(atom)
                .map(|(op, inner)| {
                    if let Some(op) = op {
                        let span = op.1.start..inner.1.end;
                        let e = ast::Expression::UnaOp {
                            kind: op.0,
                            inner: Box::new(inner),
                        };
                        (e, span)
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
                .foldl(|lhs, (kind, rhs)| {
                    let span = lhs.1.start..rhs.1.end;
                    let e = ast::Expression::BinOp {
                        kind,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    };
                    (e, span)
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
                .foldl(|lhs, (kind, rhs)| {
                    let span = lhs.1.start..rhs.1.end;
                    let e = ast::Expression::BinOp {
                        kind,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    };
                    (e, span)
                });

            let and = comparison
                .clone()
                .then(
                    just(Token::And)
                        .to(ast::BinOpKind::And)
                        .then(comparison)
                        .repeated(),
                )
                .foldl(|lhs, (kind, rhs)| {
                    let span = lhs.1.start..rhs.1.end;
                    let e = ast::Expression::BinOp {
                        kind,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    };
                    (e, span)
                });

            #[allow(clippy::let_and_return)]
            and.labelled("expression")
        },
    );

    let decl = just(Token::Decl)
        .map_with_span(|_, span: Span| span.start)
        .then(ident)
        .then_ignore(just(Token::Assign))
        .then(expr.clone().map(|(e, _)| e))
        .then(just(Token::Semicolon).map_with_span(|_, span: Span| span.end))
        .map(|(((start, name), e), end)| {
            let decl = ast::Declaration {
                name,
                expr: e,
            };
            (decl, start..end)
        }).labelled("declaration");

    decl.repeated().map(|decls| ast::Program { declarations: decls }).then_ignore(end())
}
