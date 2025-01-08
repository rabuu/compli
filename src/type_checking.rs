use std::collections::HashMap;

use miette::Diagnostic;
use thiserror::Error;

use crate::{ast, Span, Spanned, Type};

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

pub fn type_check(program: &ast::Program) -> Result<()> {
    let functions = program
        .functions
        .iter()
        .map(|(f, _)| {
            (
                f.name.clone(),
                (f.params.iter().map(|(_, typ)| *typ).collect(), f.ret_type),
            )
        })
        .collect();

    let checker = TypeChecker { functions };
    for function in &program.functions {
        checker.check_function(function)?;
    }

    Ok(())
}

struct TypeChecker {
    functions: HashMap<ast::Ident, (Vec<Type>, Type)>,
}

impl TypeChecker {
    fn check_function(&self, (function, span): &Spanned<ast::Function>) -> Result<()> {
        let vars = function.params.iter().cloned().collect();
        let ret = self.infer_expr(&function.body, &vars)?;
        expect_type(function.ret_type, ret, *span)
    }

    fn infer_expr(
        &self,
        (expr, span): &Spanned<ast::Expression>,
        vars: &HashMap<ast::Ident, Type>,
    ) -> Result<Type> {
        match expr {
            ast::Expression::Int(_) => Ok(Type::Int),
            ast::Expression::Bool(_) => Ok(Type::Bool),
            ast::Expression::Var(x) => {
                vars.get(x)
                    .copied()
                    .ok_or_else(|| TypeCheckError::NotBound {
                        name: x.clone(),
                        span: *span,
                    })
            }
            ast::Expression::UnaOp { kind, inner } => {
                let arg = self.infer_expr(inner, vars)?;
                match kind {
                    ast::UnaOpKind::Neg => {
                        expect_type(Type::Int, arg, inner.1)?;
                        Ok(Type::Int)
                    }
                    ast::UnaOpKind::Not => {
                        expect_type(Type::Bool, arg, inner.1)?;
                        Ok(Type::Bool)
                    }
                }
            }
            ast::Expression::BinOp { kind, lhs, rhs } => {
                let lhs_t = self.infer_expr(lhs, vars)?;
                let rhs_t = self.infer_expr(rhs, vars)?;
                match kind {
                    ast::BinOpKind::Add | ast::BinOpKind::Sub => {
                        expect_type(Type::Int, lhs_t, lhs.1)?;
                        expect_type(Type::Int, rhs_t, rhs.1)?;
                        Ok(Type::Int)
                    }
                    ast::BinOpKind::Equals | ast::BinOpKind::Less => {
                        expect_type(Type::Int, lhs_t, lhs.1)?;
                        expect_type(Type::Int, rhs_t, rhs.1)?;
                        Ok(Type::Bool)
                    }
                    ast::BinOpKind::And => {
                        expect_type(Type::Bool, lhs_t, lhs.1)?;
                        expect_type(Type::Bool, rhs_t, rhs.1)?;
                        Ok(Type::Bool)
                    }
                }
            }
            ast::Expression::LetIn { var, bind, body } => {
                let bind = self.infer_expr(bind, vars)?;

                let mut extended_vars = vars.clone();
                extended_vars.insert(var.clone(), bind);

                self.infer_expr(body, &extended_vars)
            }
            ast::Expression::IfThenElse {
                condition,
                then_branch,
                else_branch,
            } => {
                let condition_t = self.infer_expr(condition, vars)?;
                expect_type(Type::Bool, condition_t, condition.1)?;

                let then_branch_t = self.infer_expr(then_branch, vars)?;
                let else_branch_t = self.infer_expr(else_branch, vars)?;
                expect_type(then_branch_t, else_branch_t, else_branch.1)?;

                Ok(then_branch_t)
            }
            ast::Expression::Call { function, args } => {
                let (params, ret) =
                    self.functions
                        .get(function)
                        .ok_or_else(|| TypeCheckError::NotBound {
                            name: function.clone(),
                            span: *span,
                        })?;

                if args.len() != params.len() {
                    return Err(TypeCheckError::WrongNumberOfArguments {
                        expected: params.len(),
                        actual: args.len(),
                        span: *span,
                    });
                }

                for (arg, &param) in args.iter().zip(params.iter()) {
                    let arg_t = self.infer_expr(arg, vars)?;
                    expect_type(param, arg_t, arg.1)?;
                }

                Ok(*ret)
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
