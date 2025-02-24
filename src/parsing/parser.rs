use chumsky::prelude::*;

use super::lexer::Token;
use super::ParseErr;

use crate::{ast, Span, Type};

pub fn parser() -> impl Parser<Token, ast::UntypedProgram, Error = ParseErr<Token>> + Clone {
    let ident = select! { Token::Ident(ident) => ident }.labelled("identifier");

    let typ = choice((
        just(Token::KwInt).to(Type::Int),
        just(Token::KwBool).to(Type::Bool),
    ));

    let expr = recursive(|expr| {
        let val = select! {
            Token::Int(x) => ast::ExpressionKind::Int(x),
            Token::Bool(x) => ast::ExpressionKind::Bool(x),
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

        let unary = just(Token::Minus)
            .to(ast::UnaryOperation::Neg)
            .map_with_span(|e, span: Span| (e, span))
            .or_not()
            .then(atom)
            .map(|(op, inner)| {
                if let Some(op) = op {
                    let span = Span::new(op.1.start, inner.span.end);
                    let e = ast::ExpressionKind::Unary {
                        op: op.0,
                        inner: Box::new(inner),
                    };
                    ast::Expression::new(e, span, ast::NoContext)
                } else {
                    inner
                }
            });

        let sum_or_diff = unary
            .clone()
            .then(
                just(Token::Plus)
                    .to(ast::BinaryOperation::Add)
                    .or(just(Token::Minus).to(ast::BinaryOperation::Sub))
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

        let comparison = sum_or_diff
            .clone()
            .then(
                just(Token::Equals)
                    .to(ast::BinaryOperation::Equals)
                    .or(just(Token::Less).to(ast::BinaryOperation::Less))
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

        let term = and.labelled("term");

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
                    .at_least(1)
            )
            .then_ignore(just(Token::KwIn))
            .then(expr.clone())
            .map(|((start, binds), body)| {
                let binds = binds.into_iter().map(|((var, annotation), bind)| (var, annotation, bind)).collect();
                let span = Span::new(start, body.span.end);
                let e = ast::ExpressionKind::LetIn {
                    binds,
                    body: Box::new(body),
                };
                ast::Expression::new(e, span, ast::NoContext)
            });

        choice((if_then_else, let_in, term))
    });

    let func = just(Token::KwFunc)
        .map_with_span(|_, span: Span| span.start)
        .then(ident.map_with_span(|name, span: Span| (name, span)))
        .then(
            ident
                .then_ignore(just(Token::Colon))
                .then(typ.clone())
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .delimited_by(just(Token::ParenOpen), just(Token::ParenClose)),
        )
        .then_ignore(just(Token::Colon))
        .then(typ)
        .then_ignore(just(Token::Assign))
        .then(expr.clone())
        .map(|((((start, (name, name_span)), params), return_type), body)| {
            let full_span = Span::new(start, body.span.end);
            ast::Function {
                name,
                params,
                return_type,
                body,
                full_span,
                name_span,
            }
        });

    func.repeated()
        .collect()
        .map(|functions| ast::Program { functions })
        .then_ignore(end())
}
