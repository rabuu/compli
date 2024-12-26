use chumsky::prelude::*;

use super::lexer::Token;
use crate::ast::Program;
use crate::{ast, Span, Spanned};

enum Definition {
    Decl(Spanned<ast::Declaration>),
    Func(Spanned<ast::Function>),
}

pub fn parser() -> impl Parser<Token, ast::Program, Error = Simple<Token>> + Clone {
    let ident = select! { Token::Ident(ident) => ident }.labelled("identifier");

    let expr = recursive(|expr| {
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

        let term = and.labelled("term");

        let if_then_else = just(Token::If)
            .map_with_span(|_, span: Span| span.start)
            .then(expr.clone())
            .then_ignore(just(Token::Then))
            .then(expr.clone())
            .then_ignore(just(Token::Else))
            .then(expr.clone())
            .map(|(((start, condition), then_branch), else_branch)| {
                let span = start..else_branch.1.end;
                let e = ast::Expression::IfThenElse {
                    condition: Box::new(condition),
                    then_branch: Box::new(then_branch),
                    else_branch: Box::new(else_branch),
                };
                (e, span)
            });

        let let_in = just(Token::Let)
            .map_with_span(|_, span: Span| span.start)
            .then(ident)
            .then_ignore(just(Token::Assign))
            .then(expr.clone())
            .then_ignore(just(Token::In))
            .then(expr.clone())
            .map(|(((start, var), bind), body)| {
                let span = start..body.1.end;
                let e = ast::Expression::LetIn {
                    var,
                    bind: Box::new(bind),
                    body: Box::new(body),
                };
                (e, span)
            });

        choice((if_then_else, let_in, term))
    });

    let decl = just(Token::Decl)
        .map_with_span(|_, span: Span| span.start)
        .then(ident)
        .then_ignore(just(Token::Assign))
        .then(expr.clone())
        .map(|((start, name), expr)| {
            let span = start..expr.1.end;
            let decl = ast::Declaration { name, expr };
            (decl, span)
        })
        .labelled("declaration");

    let typ = choice((
        just(Token::KwInt).to(ast::Type::Int),
        just(Token::KwBool).to(ast::Type::Bool),
    ));

    let func = just(Token::Func)
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
        .then_ignore(just(Token::Arrow))
        .then(typ)
        .then_ignore(just(Token::Assign))
        .then(expr.clone())
        .map(|((((start, name), params), ret_type), body)| {
            let span = start..body.1.end;
            let func = ast::Function {
                name,
                params,
                ret_type,
                body,
            };
            (func, span)
        });

    choice((decl.map(Definition::Decl), func.map(Definition::Func)))
        .repeated()
        .collect()
        .map(|defs: Vec<Definition>| {
            let mut prg = Program::default();
            for def in defs {
                match def {
                    Definition::Decl(d) => prg.declarations.push(d),
                    Definition::Func(f) => prg.functions.push(f),
                }
            }
            prg
        })
        .then_ignore(end())
}
