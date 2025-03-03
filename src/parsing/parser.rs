//! Parser
//!
//! This submodule parses a stream of tokens into an AST. Its main interface is the [parser]
//! function.

use chumsky::prelude::*;

use super::lexer::Token;
use super::ParseErr;

use crate::{ast, Span};

/// Parse tokens into an AST
pub fn parser() -> impl Parser<Token, ast::UntypedProgram, Error = ParseErr<Token>> + Clone {
    let ident = select! { Token::Ident(ident) => ident }.labelled("identifier");

    let typ = choice((
        just(Token::KwInt).to(ast::Type::Int),
        just(Token::KwBool).to(ast::Type::Bool),
        just(Token::KwFloat).to(ast::Type::Float),
        ident.map(ast::Type::Record),
    ));

    let expr = recursive(|expr| {
        let val = select! {
            Token::Int(x) => ast::ExpressionKind::Int(x.parse().unwrap()),
            Token::Bool(x) => ast::ExpressionKind::Bool(x),
            Token::Float(x) => ast::ExpressionKind::Float(x.parse().unwrap()),
        }
        .labelled("value");

        let var = ident.map(ast::ExpressionKind::Var);

        let items = expr
            .clone()
            .separated_by(just(Token::Comma))
            .allow_trailing();

        let call = ident
            .then(items.delimited_by(just(Token::ParenOpen), just(Token::ParenClose)))
            .map(|(function, args)| ast::ExpressionKind::Call { function, args });

        let atom = val
            .or(call)
            .or(var)
            .map_with_span(|kind, span: Span| ast::Expression::new(kind, span, ast::NoContext))
            .or(expr
                .clone()
                .delimited_by(just(Token::ParenOpen), just(Token::ParenClose)));

        let selector = atom
            .clone()
            .then(
                just(Token::Dot)
                    .ignore_then(ident.map_with_span(|e, span: Span| (e, span)))
                    .repeated(),
            )
            .foldl(|expr, (field, field_span)| {
                let span = Span::new(expr.span.start, field_span.end);
                let e = ast::ExpressionKind::RecordSelector {
                    expr: Box::new(expr),
                    field,
                };
                ast::Expression::new(e, span, ast::NoContext)
            });

        let unary = just(Token::Minus)
            .to(ast::UnaryOperation::Neg)
            .or(just(Token::Bang).to(ast::UnaryOperation::Not))
            .map_with_span(|e, span: Span| (e, span))
            .or_not()
            .then(selector)
            .map(|(op, inner)| {
                if let Some((op, op_span)) = op {
                    let span = Span::new(op_span.start, inner.span.end);
                    let e = ast::ExpressionKind::Unary {
                        op,
                        inner: Box::new(inner),
                    };
                    ast::Expression::new(e, span, ast::NoContext)
                } else {
                    inner
                }
            });

        let prod_or_quot = unary
            .clone()
            .then(
                just(Token::Asterisk)
                    .to(ast::BinaryOperation::Mul)
                    .or(just(Token::Slash).to(ast::BinaryOperation::Div))
                    .then(unary)
                    .repeated(),
            )
            .foldl(|lhs, (kind, rhs)| {
                let span = Span::new(lhs.span.start, rhs.span.end);
                let e = ast::ExpressionKind::Binary {
                    op: kind,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                };
                ast::Expression::new(e, span, ast::NoContext)
            });

        let sum_or_diff = prod_or_quot
            .clone()
            .then(
                just(Token::Plus)
                    .to(ast::BinaryOperation::Add)
                    .or(just(Token::Minus).to(ast::BinaryOperation::Sub))
                    .then(prod_or_quot)
                    .repeated(),
            )
            .foldl(|lhs, (kind, rhs)| {
                let span = Span::new(lhs.span.start, rhs.span.end);
                let e = ast::ExpressionKind::Binary {
                    op: kind,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                };
                ast::Expression::new(e, span, ast::NoContext)
            });

        let comparison = sum_or_diff
            .clone()
            .then(
                choice((
                    just(Token::Equals).to(ast::BinaryOperation::Equals),
                    just(Token::Less).to(ast::BinaryOperation::Less),
                    just(Token::LessEq).to(ast::BinaryOperation::LessEq),
                    just(Token::Greater).to(ast::BinaryOperation::Greater),
                    just(Token::GreaterEq).to(ast::BinaryOperation::GreaterEq),
                ))
                .then(sum_or_diff)
                .repeated(),
            )
            .foldl(|lhs, (kind, rhs)| {
                let span = Span::new(lhs.span.start, rhs.span.end);
                let e = ast::ExpressionKind::Binary {
                    op: kind,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                };
                ast::Expression::new(e, span, ast::NoContext)
            });

        let and = comparison
            .clone()
            .then(
                just(Token::And)
                    .to(ast::BinaryOperation::And)
                    .then(comparison)
                    .repeated(),
            )
            .foldl(|lhs, (kind, rhs)| {
                let span = Span::new(lhs.span.start, rhs.span.end);
                let e = ast::ExpressionKind::Binary {
                    op: kind,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                };
                ast::Expression::new(e, span, ast::NoContext)
            });

        let or = and
            .clone()
            .then(
                just(Token::Or)
                    .to(ast::BinaryOperation::Or)
                    .then(and)
                    .repeated(),
            )
            .foldl(|lhs, (kind, rhs)| {
                let span = Span::new(lhs.span.start, rhs.span.end);
                let e = ast::ExpressionKind::Binary {
                    op: kind,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                };
                ast::Expression::new(e, span, ast::NoContext)
            });

        let term = or.labelled("term");

        let if_then_else = just(Token::KwIf)
            .map_with_span(|_, span: Span| span.start)
            .then(expr.clone())
            .then_ignore(just(Token::KwThen))
            .then(expr.clone())
            .then_ignore(just(Token::KwElse))
            .then(expr.clone())
            .map(|(((start, condition), yes), no)| {
                let span = Span::new(start, no.span.end);
                let e = ast::ExpressionKind::IfThenElse {
                    condition: Box::new(condition),
                    yes: Box::new(yes),
                    no: Box::new(no),
                };
                ast::Expression::new(e, span, ast::NoContext)
            });

        let let_in = just(Token::KwLet)
            .map_with_span(|_, span: Span| span.start)
            .then(
                ident
                    .then(just(Token::Colon).ignore_then(typ.clone()).or_not())
                    .then_ignore(just(Token::Assign))
                    .then(expr.clone())
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .at_least(1),
            )
            .then_ignore(just(Token::KwIn))
            .then(expr.clone())
            .map(|((start, binds), body)| {
                let binds = binds
                    .into_iter()
                    .map(|((var, annotation), bind)| (var, annotation, bind))
                    .collect();
                let span = Span::new(start, body.span.end);
                let e = ast::ExpressionKind::LetIn {
                    binds,
                    body: Box::new(body),
                };
                ast::Expression::new(e, span, ast::NoContext)
            });

        choice((if_then_else, let_in, term))
    });

    let def = just(Token::KwDef)
        .ignore_then(ident.map_with_span(|name, span: Span| (name, span)))
        .then(
            ident
                .then_ignore(just(Token::Colon))
                .then(typ.clone())
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .delimited_by(just(Token::ParenOpen), just(Token::ParenClose))
                .or_not(),
        )
        .then_ignore(just(Token::Colon))
        .then(typ.clone())
        .then_ignore(just(Token::Assign))
        .then(expr.clone())
        .map(
            |((((name, name_span), params), return_type), body)| ast::Function {
                name,
                params: params.unwrap_or_default(),
                return_type,
                body,
                name_span,
            },
        );

    let record = just(Token::KwRec)
        .ignore_then(ident.map_with_span(|name, span: Span| (name, span)))
        .then_ignore(just(Token::Assign))
        .then(
            ident
                .then_ignore(just(Token::Colon))
                .then(typ)
                .separated_by(just(Token::Comma))
                .allow_trailing(),
        )
        .map(|((name, name_span), fields)| ast::Record {
            name,
            fields,
            name_span,
        });

    enum FunctionOrRecord {
        Function(ast::Function<ast::NoContext>),
        Record(ast::Record),
    }

    let func_or_record = choice((
        def.map(FunctionOrRecord::Function),
        record.map(FunctionOrRecord::Record),
    ));

    func_or_record
        .repeated()
        .collect()
        .map(|items: Vec<FunctionOrRecord>| {
            let mut functions = Vec::new();
            let mut records = Vec::new();
            for item in items {
                match item {
                    FunctionOrRecord::Function(func) => functions.push(func),
                    FunctionOrRecord::Record(rec) => records.push(rec),
                }
            }

            ast::Program { records, functions }
        })
        .then_ignore(end())
}
