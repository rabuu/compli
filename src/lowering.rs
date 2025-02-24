//! Lowering
//!
//! This module is responsible for lowering the AST (with type checkers type information) down to
//! our intermediate representation ([ir]). The main interface is the [lower] function.

use std::collections::HashMap;

use miette::Diagnostic;
use thiserror::Error;

use crate::{ast::*, ir, Span, Type, Variable};

#[derive(Debug, Error, Diagnostic)]
pub enum LoweringError {
    #[error("No main function defined as entry point for the program")]
    #[diagnostic(help("An entry function must be defined with `func main(): int`"))]
    NoEntryFunction,

    #[error("The main function is malformed")]
    MalformedEntryFunction {
        #[help]
        context: String,

        #[label]
        span: Span,
    },

    #[error("The variable {variable} is not bound")]
    VariableNotBound {
        variable: String,

        #[label("this variable")]
        span: Span,
    },
}

type Result<T> = std::result::Result<T, LoweringError>;

/// Turn the AST (with type information) into IR
pub fn lower(program: TypedProgram) -> Result<ir::Program> {
    let mut lowerer = Lowerer::default();
    lowerer.lower_program(program)
}

/// The main state during lowering
///
/// This state keeps track of which variables can be used as fresh.
#[derive(Debug, Default)]
struct Lowerer {
    fresh_variable: Variable,
}

impl Lowerer {
    fn lower_program(&mut self, program: TypedProgram) -> Result<ir::Program> {
        let mut functions = Vec::with_capacity(program.functions.len());
        let mut entry = None;
        for func in program.functions {
            let func_is_main = func.name.as_str() == "main";
            let name_span = func.name_span;

            let function = self.lower_function(func)?;

            // check if entry/main function is valid
            if func_is_main {
                if !function.prototype.parameters.is_empty() {
                    return Err(LoweringError::MalformedEntryFunction {
                        context: String::from("The main function does not take any arguments"),
                        span: name_span,
                    });
                }

                if function.prototype.return_type != Type::Int {
                    return Err(LoweringError::MalformedEntryFunction {
                        context: String::from("The main function does always return the type int"),
                        span: name_span,
                    });
                }

                entry = Some(function.body);
            } else {
                functions.push(function);
            }
        }

        Ok(ir::Program {
            entry: entry.ok_or(LoweringError::NoEntryFunction)?,
            functions,
        })
    }

    fn lower_function(&mut self, func: Function<Type>) -> Result<ir::FunctionDefinition> {
        let param_vars: Vec<(Ident, Variable, Type)> = func
            .params
            .into_iter()
            .map(|(arg, typ)| (arg, self.fresh_variable(), typ))
            .collect();

        let vars = param_vars
            .clone()
            .into_iter()
            .map(|(name, var, _)| (name, var))
            .collect();

        let prototype = ir::FunctionPrototype {
            name: func.name.clone(),
            parameters: param_vars
                .into_iter()
                .map(|(_, var, typ)| (var, typ))
                .collect(),
            return_type: func.return_type,
        };

        let body = self.lower_expression(func.body, &vars)?;

        Ok(ir::FunctionDefinition { prototype, body })
    }

    fn lower_expression(
        &mut self,
        expr: Expression<Type>,
        vars: &HashMap<Ident, Variable>,
    ) -> Result<ir::Expression> {
        match expr.kind {
            ExpressionKind::Int(i) => Ok(ir::Expression::Direct(ir::Value::Integer(i as i32))),
            ExpressionKind::Float(f) => Ok(ir::Expression::Direct(ir::Value::Float(f))),
            ExpressionKind::Bool(b) => Ok(ir::Expression::Direct(ir::Value::Boolean(b))),
            ExpressionKind::Var(v) => vars
                .get(&v)
                .copied()
                .map(|v| ir::Expression::Direct(ir::Value::Variable(v)))
                .ok_or(LoweringError::VariableNotBound {
                    variable: v,
                    span: expr.span,
                }),
            ExpressionKind::Unary { op, inner } => {
                let arg = self.lower_expression(*inner, vars)?;
                Ok(ir::Expression::UnaryOperation(Box::new(
                    ir::UnaryOperation {
                        kind: match op {
                            UnaryOperation::Neg => ir::UnaryOperationKind::Neg,
                            UnaryOperation::Not => ir::UnaryOperationKind::Not,
                        },
                        inner: arg,
                    }
                )))
            }
            ExpressionKind::Binary { op: kind, lhs, rhs } => {
                let lhs = self.lower_expression(*lhs, vars)?;
                let rhs = self.lower_expression(*rhs, vars)?;
                Ok(ir::Expression::BinaryOperation(Box::new(
                    ir::BinaryOperation {
                        kind: match kind {
                            BinaryOperation::Add => ir::BinaryOperationKind::Add,
                            BinaryOperation::Sub => ir::BinaryOperationKind::Sub,
                            BinaryOperation::Mul => ir::BinaryOperationKind::Mul,
                            BinaryOperation::Div => ir::BinaryOperationKind::Div,
                            BinaryOperation::Equals => ir::BinaryOperationKind::Equals,
                            BinaryOperation::Less => ir::BinaryOperationKind::Less,
                            BinaryOperation::LessEq => ir::BinaryOperationKind::LessEq,
                            BinaryOperation::Greater => ir::BinaryOperationKind::Greater,
                            BinaryOperation::GreaterEq => ir::BinaryOperationKind::GreaterEq,
                            BinaryOperation::And => ir::BinaryOperationKind::And,
                            BinaryOperation::Or => ir::BinaryOperationKind::Or,
                        },
                        lhs,
                        rhs,
                    },
                )))
            }
            ExpressionKind::LetIn { mut binds, body } => {
                let mut extended_vars = vars.clone();

                if binds.len() <= 1 {
                    let (var, _, bind) = binds.pop().expect("at least 1");
                    let bind = self.lower_expression(bind, vars)?;

                    let fresh = self.fresh_variable();
                    extended_vars.insert(var, fresh);

                    let body = self.lower_expression(*body, &extended_vars)?;

                    Ok(ir::Expression::LocalBinding(Box::new(ir::LocalBinding {
                        var: fresh,
                        bind,
                        body,
                    })))
                } else {
                    let (last_var, _, last_expr) = binds.remove(0);
                    let bind = self.lower_expression(last_expr, vars)?;

                    let fresh = self.fresh_variable();
                    extended_vars.insert(last_var, fresh);

                    let rest = Expression {
                        kind: ExpressionKind::LetIn { binds, body },
                        span: expr.span,
                        type_context: expr.type_context,
                    };
                    let rest = self.lower_expression(rest, &extended_vars)?;

                    Ok(ir::Expression::LocalBinding(Box::new(ir::LocalBinding {
                        var: fresh,
                        bind,
                        body: rest,
                    })))
                }
            }
            ExpressionKind::IfThenElse { condition, yes, no } => {
                Ok(ir::Expression::Conditional(Box::new(ir::Conditional {
                    condition: self.lower_expression(*condition, vars)?,
                    yes: self.lower_expression(*yes, vars)?,
                    no: self.lower_expression(*no, vars)?,
                })))
            }
            ExpressionKind::Call { function, args } => {
                // builtin: trace function
                if function.as_str() == "trace" {
                    let function_name = match args[0].type_context {
                        Type::Int => "__compli_trace_int",
                        Type::Float => "__compli_trace_float",
                        Type::Bool => "__compli_trace_bool",
                    }
                    .to_string();

                    let mut args = args;
                    let lowered_arg = self.lower_expression(args.swap_remove(0), vars)?;

                    return Ok(ir::Expression::FunctionCall {
                        function_name,
                        args: vec![lowered_arg],
                    });
                }

                let mut lowered_args = Vec::with_capacity(args.len());
                for arg in args {
                    lowered_args.push(self.lower_expression(arg, vars)?);
                }

                Ok(ir::Expression::FunctionCall {
                    function_name: function,
                    args: lowered_args,
                })
            }
        }
    }

    fn fresh_variable(&mut self) -> Variable {
        let fresh = self.fresh_variable;
        self.fresh_variable.advance();
        fresh
    }
}
