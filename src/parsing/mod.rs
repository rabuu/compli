use chumsky::prelude::*;

use crate::ast;
use lexer::Token;

mod lexer;

pub fn parse(text: &str) {
    let tokens = lexer::lex().parse(text);
    println!("{tokens:#?}");

    let ast = parser().parse(
        tokens
            .unwrap()
            .into_iter()
            .map(|(tok, _)| tok)
            .collect::<Vec<_>>(),
    );
    println!("{ast:#?}");
}

fn parser() -> impl Parser<Token, ast::Expression, Error = Simple<Token>> + Clone {
    let expr = recursive(|expr| {
        let val = select! {
            Token::Int(x) => ast::Expression::Int(x),
            Token::Bool(x) => ast::Expression::Bool(x),
        }
        .labelled("value");

        let ident = select! { Token::Ident(ident) => ident }.labelled("identifier");

        let items = expr
            .clone()
            .separated_by(just(Token::Comma))
            .allow_trailing();

        let call = ident
            .then(items.delimited_by(just(Token::ParenOpen), just(Token::ParenClose)))
            .map(|(function, args)| ast::Expression::Call { function, args });

        let atom = choice((
            val,
            call,
            ident.map(ast::Expression::Var),
            expr.clone()
                .delimited_by(just(Token::ParenOpen), just(Token::ParenClose)),
        ));

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
