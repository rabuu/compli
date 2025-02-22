use std::collections::HashMap;

use miette::Diagnostic;
use thiserror::Error;

use crate::{ast, Span, Type};

#[derive(Debug, Error, Diagnostic)]
pub enum TypeCheckError {
    #[error("Expected an expression of type `{expected}` but got `{actual}`")]
    UnexpectedType {
        expected: Type,
        actual: Type,

        #[label("this expression")]
        span: Span,
    },

    #[error("The name `{name}` is not bound")]
    NotBound {
        name: ast::Ident,

        #[label("unknown name")]
        span: Span,
    },

    #[error("Expected {expected} arguments but got {actual}")]
    WrongNumberOfArguments {
        expected: usize,
        actual: usize,

        #[label("this function call")]
        span: Span,
    },
}

type Result<T> = std::result::Result<T, TypeCheckError>;

pub fn type_check(program: ast::UntypedProgram) -> Result<ast::TypedProgram> {
    let prototypes = program
        .functions
        .iter()
        .map(|f| {
            (
                f.name.clone(),
                (f.params.iter().map(|(_, typ)| *typ).collect(), f.ret_type),
            )
        })
        .collect();

    let checker = TypeChecker { prototypes };

    let mut typed_functions = Vec::with_capacity(program.functions.len());
    for function in program.functions {
        typed_functions.push(checker.check_function(function)?);
    }

    Ok(ast::Program {
        functions: typed_functions,
    })
}

struct TypeChecker {
    prototypes: HashMap<ast::Ident, (Vec<Type>, Type)>,
}

impl TypeChecker {
    fn check_function(&self, function: ast::Function<ast::NoContext>) -> Result<ast::Function<Type>> {
        let vars = function.params.iter().cloned().collect();
        let typed_body = self.infer_expr(function.body, &vars)?;
        expect_type(function.ret_type, typed_body.context, function.span)?;

        Ok(ast::Function {
            name: function.name,
            params: function.params,
            ret_type: function.ret_type,
            body: typed_body,
            span: function.span,
        })
    }

    fn infer_expr(
        &self,
        expr: ast::Expression<ast::NoContext>,
        vars: &HashMap<ast::Ident, Type>,
    ) -> Result<ast::Expression<Type>> {
        match expr.kind {
            ast::ExpressionKind::Int(x) => Ok(ast::Expression {
                kind: ast::ExpressionKind::Int(x),
                span: expr.span,
                context: Type::Int,
            }),

            ast::ExpressionKind::Bool(x) => Ok(ast::Expression {
                kind: ast::ExpressionKind::Bool(x),
                span: expr.span,
                context: Type::Bool,
            }),

            ast::ExpressionKind::Var(x) => {
                let typ = vars
                    .get(&x)
                    .copied()
                    .ok_or_else(|| TypeCheckError::NotBound {
                        name: x.clone(),
                        span: expr.span,
                    })?;

                Ok(ast::Expression {
                    kind: ast::ExpressionKind::Var(x),
                    span: expr.span,
                    context: typ,
                })
            }

            ast::ExpressionKind::UnaOp { op_kind, inner } => {
                let arg = self.infer_expr(*inner, vars)?;
                let typ = match op_kind {
                    ast::UnaOpKind::Neg => {
                        expect_type(Type::Int, arg.context, arg.span)?;
                        Type::Int
                    }
                    ast::UnaOpKind::Not => {
                        expect_type(Type::Bool, arg.context, arg.span)?;
                        Type::Bool
                    }
                };

                Ok(ast::Expression {
                    kind: ast::ExpressionKind::UnaOp {
                        op_kind,
                        inner: Box::new(arg),
                    },
                    span: expr.span,
                    context: typ,
                })
            }

            ast::ExpressionKind::BinOp { op_kind, lhs, rhs } => {
                let typed_lhs = self.infer_expr(*lhs, vars)?;
                let typed_rhs = self.infer_expr(*rhs, vars)?;
                let typ = match op_kind {
                    ast::BinOpKind::Add | ast::BinOpKind::Sub => {
                        expect_type(Type::Int, typed_lhs.context, typed_lhs.span)?;
                        expect_type(Type::Int, typed_rhs.context, typed_rhs.span)?;
                        Type::Int
                    }
                    ast::BinOpKind::Equals | ast::BinOpKind::Less => {
                        expect_type(Type::Int, typed_lhs.context, typed_lhs.span)?;
                        expect_type(Type::Int, typed_rhs.context, typed_rhs.span)?;
                        Type::Bool
                    }
                    ast::BinOpKind::And => {
                        expect_type(Type::Bool, typed_lhs.context, typed_lhs.span)?;
                        expect_type(Type::Bool, typed_rhs.context, typed_rhs.span)?;
                        Type::Bool
                    }
                };

                Ok(ast::Expression {
                    kind: ast::ExpressionKind::BinOp {
                        op_kind,
                        lhs: Box::new(typed_lhs),
                        rhs: Box::new(typed_rhs),
                    },
                    span: expr.span,
                    context: typ,
                })
            }

            ast::ExpressionKind::LetIn { var, bind, body } => {
                let typed_bind = self.infer_expr(*bind, vars)?;

                let mut extended_vars = vars.clone();
                extended_vars.insert(var.clone(), typed_bind.context);

                let typed_body = self.infer_expr(*body, &extended_vars)?;
                let typ = typed_body.context;

                Ok(ast::Expression {
                    kind: ast::ExpressionKind::LetIn {
                        var,
                        bind: Box::new(typed_bind),
                        body: Box::new(typed_body),
                    },
                    span: expr.span,
                    context: typ,
                })
            }

            ast::ExpressionKind::IfThenElse {
                condition,
                then_branch,
                else_branch,
            } => {
                let typed_condition = self.infer_expr(*condition, vars)?;
                expect_type(Type::Bool, typed_condition.context, typed_condition.span)?;

                let typed_then_branch = self.infer_expr(*then_branch, vars)?;
                let typed_else_branch = self.infer_expr(*else_branch, vars)?;
                expect_type(
                    typed_then_branch.context,
                    typed_else_branch.context,
                    typed_else_branch.span,
                )?;
                let typ = typed_then_branch.context;

                Ok(ast::Expression {
                    kind: ast::ExpressionKind::IfThenElse {
                        condition: Box::new(typed_condition),
                        then_branch: Box::new(typed_then_branch),
                        else_branch: Box::new(typed_else_branch),
                    },
                    span: expr.span,
                    context: typ,
                })
            }

            ast::ExpressionKind::Call { function, args } => {
                let (params, ret) =
                    self.prototypes
                        .get(&function)
                        .ok_or_else(|| TypeCheckError::NotBound {
                            name: function.clone(),
                            span: expr.span,
                        })?;

                if args.len() != params.len() {
                    return Err(TypeCheckError::WrongNumberOfArguments {
                        expected: params.len(),
                        actual: args.len(),
                        span: expr.span,
                    });
                }

                let mut typed_args = Vec::with_capacity(args.len());
                for (arg, &param) in args.into_iter().zip(params.iter()) {
                    let typed_arg = self.infer_expr(arg, vars)?;
                    expect_type(param, typed_arg.context, typed_arg.span)?;
                    typed_args.push(typed_arg);
                }

                Ok(ast::Expression {
                    kind: ast::ExpressionKind::Call {
                        function,
                        args: typed_args,
                    },
                    span: expr.span,
                    context: *ret,
                })
            }
        }
    }
}

fn expect_type(expected: Type, actual: Type, span: Span) -> Result<()> {
    if actual == expected {
        Ok(())
    } else {
        Err(TypeCheckError::UnexpectedType {
            expected,
            actual,
            span,
        })
    }
}
