use chumsky::prelude::*;

use super::lexer::Token;
use super::ParseErr;

use crate::{ast, Span, Type};

pub fn parser() -> impl Parser<Token, ast::Program<()>, Error = ParseErr<Token>> + Clone {
    let ident = select! { Token::Ident(ident) => ident }.labelled("identifier");

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
            .map_with_span(|kind, span: Span| ast::Expression::new(kind, span, ()))
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
                    let span = Span::new(op.1.start, inner.span.end);
                    let e = ast::ExpressionKind::UnaOp {
                        op_kind: op.0,
                        inner: Box::new(inner),
                    };
                    ast::Expression::new(e, span, ())
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
                let span = Span::new(lhs.span.start, rhs.span.end);
                let e = ast::ExpressionKind::BinOp {
                    op_kind: kind,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                };
                ast::Expression::new(e, span, ())
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
                let span = Span::new(lhs.span.start, rhs.span.end);
                let e = ast::ExpressionKind::BinOp {
                    op_kind: kind,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                };
                ast::Expression::new(e, span, ())
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
                let span = Span::new(lhs.span.start, rhs.span.end);
                let e = ast::ExpressionKind::BinOp {
                    op_kind: kind,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                };
                ast::Expression::new(e, span, ())
            });

        let term = and.labelled("term");

        let if_then_else = just(Token::KwIf)
            .map_with_span(|_, span: Span| span.start)
            .then(expr.clone())
            .then_ignore(just(Token::KwThen))
            .then(expr.clone())
            .then_ignore(just(Token::KwElse))
            .then(expr.clone())
            .map(|(((start, condition), then_branch), else_branch)| {
                let span = Span::new(start, else_branch.span.end);
                let e = ast::ExpressionKind::IfThenElse {
                    condition: Box::new(condition),
                    then_branch: Box::new(then_branch),
                    else_branch: Box::new(else_branch),
                };
                ast::Expression::new(e, span, ())
            });

        let let_in = just(Token::KwLet)
            .map_with_span(|_, span: Span| span.start)
            .then(ident)
            .then_ignore(just(Token::Assign))
            .then(expr.clone())
            .then_ignore(just(Token::KwIn))
            .then(expr.clone())
            .map(|(((start, var), bind), body)| {
                let span = Span::new(start, body.span.end);
                let e = ast::ExpressionKind::LetIn {
                    var,
                    bind: Box::new(bind),
                    body: Box::new(body),
                };
                ast::Expression::new(e, span, ())
            });

        choice((if_then_else, let_in, term))
    });

    let typ = choice((
        just(Token::KwInt).to(Type::Int),
        just(Token::KwBool).to(Type::Bool),
    ));

    let func = just(Token::KwFunc)
        .map_with_span(|_, span: Span| span.start)
        .then(ident)
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
        .map(|((((start, name), params), ret_type), body)| {
            let span = Span::new(start, body.span.end);
            ast::Function {
                name,
                params,
                ret_type,
                body,
                span,
            }
        });

    func.repeated()
        .collect()
        .map(|functions| ast::Program { functions })
        .then_ignore(end())
}
