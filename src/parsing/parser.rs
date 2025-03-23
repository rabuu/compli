//! Parser
//!
//! This submodule parses a stream of tokens into an AST. Its main interface is the [parser]
//! function.

use chumsky::input::ValueInput;
use chumsky::prelude::*;

use super::lexer::Token;
use super::ParseErr;

use crate::{ast, Span};

pub trait ParseInput<'src>: ValueInput<'src, Token = Token<'src>, Span = Span> {}
impl<'src, T: ValueInput<'src, Token = Token<'src>, Span = Span>> ParseInput<'src> for T {}

/// Parse tokens into an AST
pub fn parser<'src, I: ParseInput<'src>>(
) -> impl Parser<'src, I, ast::UntypedAst<'src>, ParseErr<'src>> + Clone {
    let ident = select! { Token::Ident(ident) => ident }.labelled("identifier");

    let typ = choice((
        just(Token::KwInt).to(ast::Type::Int),
        just(Token::KwBool).to(ast::Type::Bool),
        just(Token::KwFloat).to(ast::Type::Float),
        ident.map(ast::Type::Record),
    ));

    let expr =
        recursive(|expr| {
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
                .allow_trailing()
                .collect::<Vec<_>>();

            let call = ident
                .then(items.delimited_by(just(Token::ParenOpen), just(Token::ParenClose)))
                .map(|(function, args)| ast::ExpressionKind::Call { function, args });

            let atom = val
                .or(call)
                .or(var)
                .map_with(|kind, e| ast::Expression::new(kind, e.span(), ast::NoTypeContext))
                .or(expr
                    .clone()
                    .delimited_by(just(Token::ParenOpen), just(Token::ParenClose)))
                .boxed();

            let complex_expr = atom;

            // selector operator
            let op = just(Token::Dot);
            let complex_expr = complex_expr.clone().foldl_with(
                op.ignore_then(ident).repeated(),
                |expr, field, e| ast::Expression {
                    kind: ast::ExpressionKind::RecordSelector {
                        expr: Box::new(expr),
                        field,
                    },
                    span: e.span(),
                    typ: ast::NoTypeContext,
                },
            );

            // unary operations
            let op = choice((
                just(Token::Minus).to(ast::UnaryOperation::Neg),
                just(Token::Bang).to(ast::UnaryOperation::Not),
            ));
            let complex_expr = op
                .or_not()
                .then(complex_expr)
                .map_with(|(op, inner), e| match op {
                    None => inner,
                    Some(op) => ast::Expression {
                        kind: ast::ExpressionKind::Unary {
                            op,
                            inner: Box::new(inner),
                        },
                        span: e.span(),
                        typ: ast::NoTypeContext,
                    },
                });

            // product operations
            let op = choice((
                just(Token::Asterisk).to(ast::BinaryOperation::Mul),
                just(Token::Slash).to(ast::BinaryOperation::Div),
            ));
            let complex_expr = complex_expr.clone().foldl_with(
                op.then(complex_expr).repeated(),
                |a, (op, b), e| ast::Expression {
                    kind: ast::ExpressionKind::Binary {
                        op,
                        lhs: Box::new(a),
                        rhs: Box::new(b),
                    },
                    span: e.span(),
                    typ: ast::NoTypeContext,
                },
            );

            // sum operations
            let op = choice((
                just(Token::Plus).to(ast::BinaryOperation::Add),
                just(Token::Minus).to(ast::BinaryOperation::Sub),
            ));
            let complex_expr = complex_expr.clone().foldl_with(
                op.then(complex_expr).repeated(),
                |a, (op, b), e| ast::Expression {
                    kind: ast::ExpressionKind::Binary {
                        op,
                        lhs: Box::new(a),
                        rhs: Box::new(b),
                    },
                    span: e.span(),
                    typ: ast::NoTypeContext,
                },
            );

            // comparison operations
            let op = choice((
                just(Token::Equals).to(ast::BinaryOperation::Equals),
                just(Token::Less).to(ast::BinaryOperation::Less),
                just(Token::LessEq).to(ast::BinaryOperation::LessEq),
                just(Token::Greater).to(ast::BinaryOperation::Greater),
                just(Token::GreaterEq).to(ast::BinaryOperation::GreaterEq),
            ));
            let complex_expr = complex_expr.clone().foldl_with(
                op.then(complex_expr).repeated(),
                |a, (op, b), e| ast::Expression {
                    kind: ast::ExpressionKind::Binary {
                        op,
                        lhs: Box::new(a),
                        rhs: Box::new(b),
                    },
                    span: e.span(),
                    typ: ast::NoTypeContext,
                },
            );

            // and operation
            let op = just(Token::And).to(ast::BinaryOperation::And);
            let complex_expr = complex_expr.clone().foldl_with(
                op.then(complex_expr).repeated(),
                |a, (op, b), e| ast::Expression {
                    kind: ast::ExpressionKind::Binary {
                        op,
                        lhs: Box::new(a),
                        rhs: Box::new(b),
                    },
                    span: e.span(),
                    typ: ast::NoTypeContext,
                },
            );

            // or operation
            let op = just(Token::Or).to(ast::BinaryOperation::Or);
            let complex_expr = complex_expr.clone().foldl_with(
                op.then(complex_expr).repeated(),
                |a, (op, b), e| ast::Expression {
                    kind: ast::ExpressionKind::Binary {
                        op,
                        lhs: Box::new(a),
                        rhs: Box::new(b),
                    },
                    span: e.span(),
                    typ: ast::NoTypeContext,
                },
            );

            let term = complex_expr.labelled("term");

            let if_then_else = just(Token::KwIf)
                .ignore_then(expr.clone())
                .then_ignore(just(Token::KwThen))
                .then(expr.clone())
                .then_ignore(just(Token::KwElse))
                .then(expr.clone())
                .map_with(|((condition, yes), no), e| {
                    let kind = ast::ExpressionKind::IfThenElse {
                        condition: Box::new(condition),
                        yes: Box::new(yes),
                        no: Box::new(no),
                    };
                    ast::Expression::new(kind, e.span(), ast::NoTypeContext)
                });

            let let_in = just(Token::KwLet)
                .ignore_then(
                    ident
                        .then(just(Token::Colon).ignore_then(typ.clone()).or_not())
                        .then_ignore(just(Token::Assign))
                        .then(expr.clone())
                        .separated_by(just(Token::Comma))
                        .allow_trailing()
                        .at_least(1)
                        .collect::<Vec<_>>(),
                )
                .then_ignore(just(Token::KwIn))
                .then(expr.clone())
                .map_with(|(binds, body), e| {
                    let binds = binds
                        .into_iter()
                        .map(|((var, annotation), bind)| (var, annotation, bind))
                        .collect();
                    let kind = ast::ExpressionKind::LetIn {
                        binds,
                        body: Box::new(body),
                    };
                    ast::Expression::new(kind, e.span(), ast::NoTypeContext)
                });

            choice((if_then_else, let_in, term))
                .boxed()
                .labelled("expression")
                .as_context()
        });

    let def = just(Token::KwDef)
        .ignore_then(ident.map_with(|name, e| (name, e.span())))
        .then(
            ident
                .then_ignore(just(Token::Colon))
                .then(typ.clone())
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .collect()
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
        .ignore_then(ident.map_with(|name, e| (name, e.span())))
        .then(
            just(Token::Assign)
                .ignore_then(
                    ident
                        .then_ignore(just(Token::Colon))
                        .then(typ)
                        .separated_by(just(Token::Comma))
                        .allow_trailing()
                        .at_least(1)
                        .collect(),
                )
                .or_not(),
        )
        .map(|((name, name_span), fields)| ast::Record {
            name,
            fields: fields.unwrap_or_default(),
            name_span,
        });

    enum FunctionOrRecord<'src> {
        Function(ast::Function<'src, ast::NoTypeContext>),
        Record(ast::Record<'src>),
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
        .boxed()
}
