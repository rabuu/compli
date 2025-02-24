//! Type checking
//!
//! This module is responsible for checking if the types of all expressions in an AST are valid.
//! Also, this type checker transforms the AST into a "typed AST" so that the compiler can leverage
//! all the type information at a later point. The main interface is the [type_check] function.

use std::collections::{HashMap, HashSet};

use miette::Diagnostic;
use thiserror::Error;

use crate::{ast, Span, Type};

#[derive(Debug, Error, Diagnostic)]
pub enum TypeCheckError {
    #[error("Expected an expression of type `{expected}` but got `{actual}`")]
    UnexpectedType {
        expected: Type,
        actual: Type,

        #[label("expression with unexpected type")]
        span: Span,
    },

    #[error("Expected an expression of type `{expected1}` or `{expected2}` but got `{actual}`")]
    UnexpectedTypeOfTwo {
        expected1: Type,
        expected2: Type,
        actual: Type,

        #[label("expression with unexpected type")]
        span: Span,
    },

    #[error("The name `{name}` is not bound")]
    NotBound {
        name: ast::Ident,

        #[label("unknown name")]
        span: Span,
    },

    #[error("The name `{name}` can not be user-defined")]
    IllegalFunctionName {
        name: ast::Ident,

        #[label("illegal name")]
        span: Span,
    },

    #[error("There are multiple function definitions with the name `{name}`")]
    MultipleFunctionDefinitions {
        name: ast::Ident,

        #[label("second definition")]
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

/// Check and store the types of all expressions
pub fn type_check(program: ast::UntypedProgram) -> Result<ast::TypedProgram> {
    let prototypes = program
        .functions
        .iter()
        .map(|f| {
            (
                f.name.clone(),
                (
                    f.params.iter().map(|(_, typ)| *typ).collect(),
                    f.return_type,
                ),
            )
        })
        .collect();

    let mut checker = TypeChecker::new(prototypes);

    let mut typed_functions = Vec::with_capacity(program.functions.len());
    for function in program.functions {
        typed_functions.push(checker.check_function(function)?);
    }

    Ok(ast::Program {
        functions: typed_functions,
    })
}

/// The main state during type checking
///
/// This state keeps track of function prototypes and which functions were already checked.
struct TypeChecker {
    prototypes: HashMap<ast::Ident, (Vec<Type>, Type)>,
    already_defined: HashSet<ast::Ident>,
}

impl TypeChecker {
    fn new(prototypes: HashMap<ast::Ident, (Vec<Type>, Type)>) -> Self {
        Self {
            prototypes,
            already_defined: HashSet::new(),
        }
    }

    fn check_function(
        &mut self,
        function: ast::Function<ast::NoContext>,
    ) -> Result<ast::Function<Type>> {
        let name = function.name.as_str();

        const ILLEGAL_NAMES: [&str; 1] = ["trace"];
        if ILLEGAL_NAMES.contains(&name) || name.starts_with("__compli") {
            return Err(TypeCheckError::IllegalFunctionName {
                name: function.name,
                span: function.name_span,
            });
        }

        if !self.already_defined.insert(function.name.clone()) {
            return Err(TypeCheckError::MultipleFunctionDefinitions {
                name: function.name,
                span: function.name_span,
            });
        }

        let vars = function.params.iter().cloned().collect();
        let typed_body = self.infer_expr(function.body, &vars)?;
        expect_type(
            function.return_type,
            typed_body.type_context,
            typed_body.span,
        )?;

        Ok(ast::Function {
            name: function.name,
            params: function.params,
            return_type: function.return_type,
            body: typed_body,
            full_span: function.full_span,
            name_span: function.name_span,
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
                type_context: Type::Int,
            }),

            ast::ExpressionKind::Float(x) => Ok(ast::Expression {
                kind: ast::ExpressionKind::Float(x),
                span: expr.span,
                type_context: Type::Float,
            }),

            ast::ExpressionKind::Bool(x) => Ok(ast::Expression {
                kind: ast::ExpressionKind::Bool(x),
                span: expr.span,
                type_context: Type::Bool,
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
                    type_context: typ,
                })
            }

            ast::ExpressionKind::Unary { op, inner } => {
                let arg = self.infer_expr(*inner, vars)?;
                let typ = match op {
                    ast::UnaryOperation::Neg => {
                        expect_type_of_two(Type::Int, Type::Float, arg.type_context, arg.span)?;
                        arg.type_context
                    }
                    ast::UnaryOperation::Not => {
                        expect_type(Type::Bool, arg.type_context, arg.span)?;
                        Type::Bool
                    }
                };

                Ok(ast::Expression {
                    kind: ast::ExpressionKind::Unary {
                        op,
                        inner: Box::new(arg),
                    },
                    span: expr.span,
                    type_context: typ,
                })
            }

            ast::ExpressionKind::Binary { op, lhs, rhs } => {
                let typed_lhs = self.infer_expr(*lhs, vars)?;
                let typed_rhs = self.infer_expr(*rhs, vars)?;
                let typ = match op {
                    ast::BinaryOperation::Add
                    | ast::BinaryOperation::Sub
                    | ast::BinaryOperation::Mul
                    | ast::BinaryOperation::Div => {
                        expect_type_of_two(
                            Type::Int,
                            Type::Float,
                            typed_lhs.type_context,
                            typed_lhs.span,
                        )?;
                        expect_type(
                            typed_lhs.type_context,
                            typed_rhs.type_context,
                            typed_rhs.span,
                        )?;
                        typed_lhs.type_context
                    }
                    ast::BinaryOperation::Equals
                    | ast::BinaryOperation::Less
                    | ast::BinaryOperation::LessEq
                    | ast::BinaryOperation::Greater
                    | ast::BinaryOperation::GreaterEq => {
                        expect_type_of_two(
                            Type::Int,
                            Type::Float,
                            typed_lhs.type_context,
                            typed_lhs.span,
                        )?;
                        expect_type(
                            typed_lhs.type_context,
                            typed_rhs.type_context,
                            typed_rhs.span,
                        )?;
                        Type::Bool
                    }
                    ast::BinaryOperation::And | ast::BinaryOperation::Or => {
                        expect_type(Type::Bool, typed_lhs.type_context, typed_lhs.span)?;
                        expect_type(Type::Bool, typed_rhs.type_context, typed_rhs.span)?;
                        Type::Bool
                    }
                };

                Ok(ast::Expression {
                    kind: ast::ExpressionKind::Binary {
                        op,
                        lhs: Box::new(typed_lhs),
                        rhs: Box::new(typed_rhs),
                    },
                    span: expr.span,
                    type_context: typ,
                })
            }

            ast::ExpressionKind::LetIn { binds, body } => {
                let mut extended_vars = vars.clone();

                let mut typed_binds = Vec::with_capacity(binds.len());
                for (var, annotation, bind) in binds {
                    let typed_bind = self.infer_expr(bind, &extended_vars)?;
                    let typ = typed_bind.type_context;
                    let bind_span = typed_bind.span;
                    extended_vars.insert(var.clone(), typ);
                    typed_binds.push((var, annotation, typed_bind));

                    if let Some(annotation) = annotation {
                        expect_type(annotation, typ, bind_span)?;
                    }
                }

                let typed_body = self.infer_expr(*body, &extended_vars)?;
                let typ = typed_body.type_context;

                Ok(ast::Expression {
                    kind: ast::ExpressionKind::LetIn {
                        binds: typed_binds,
                        body: Box::new(typed_body),
                    },
                    span: expr.span,
                    type_context: typ,
                })
            }

            ast::ExpressionKind::IfThenElse { condition, yes, no } => {
                let typed_condition = self.infer_expr(*condition, vars)?;
                expect_type(
                    Type::Bool,
                    typed_condition.type_context,
                    typed_condition.span,
                )?;

                let typed_yes = self.infer_expr(*yes, vars)?;
                let typed_no = self.infer_expr(*no, vars)?;
                expect_type(typed_yes.type_context, typed_no.type_context, typed_no.span)?;
                let typ = typed_yes.type_context;

                Ok(ast::Expression {
                    kind: ast::ExpressionKind::IfThenElse {
                        condition: Box::new(typed_condition),
                        yes: Box::new(typed_yes),
                        no: Box::new(typed_no),
                    },
                    span: expr.span,
                    type_context: typ,
                })
            }

            ast::ExpressionKind::Call { function, args } => {
                // builtin: trace function
                if function.as_str() == "trace" {
                    if args.len() != 1 {
                        return Err(TypeCheckError::WrongNumberOfArguments {
                            expected: 1,
                            actual: args.len(),
                            span: expr.span,
                        });
                    }

                    let mut args = args;
                    let typed_arg = self.infer_expr(args.swap_remove(0), vars)?;
                    let typ = typed_arg.type_context;

                    return Ok(ast::Expression {
                        kind: ast::ExpressionKind::Call {
                            function,
                            args: vec![typed_arg],
                        },
                        span: expr.span,
                        type_context: typ,
                    });
                }

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
                    expect_type(param, typed_arg.type_context, typed_arg.span)?;
                    typed_args.push(typed_arg);
                }

                Ok(ast::Expression {
                    kind: ast::ExpressionKind::Call {
                        function,
                        args: typed_args,
                    },
                    span: expr.span,
                    type_context: *ret,
                })
            }
        }
    }
}

/// Assert that two types are equal
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

/// Assert that a type is one of a number of others
fn expect_type_of_two(expected1: Type, expected2: Type, actual: Type, span: Span) -> Result<()> {
    if actual == expected1 || actual == expected2 {
        Ok(())
    } else {
        Err(TypeCheckError::UnexpectedTypeOfTwo {
            expected1,
            expected2,
            actual,
            span,
        })
    }
}
