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

pub fn lower(program: Program<Type>) -> Result<ir::Program> {
    let mut lowerer = Lowerer::default();
    lowerer.lower_program(program)
}

#[derive(Debug, Default)]
struct Lowerer {
    fresh_variable: Variable,
}

impl Lowerer {
    fn lower_program(&mut self, program: Program<Type>) -> Result<ir::Program> {
        let mut functions = Vec::with_capacity(program.functions.len());
        let mut entry = None;
        for func in program.functions {
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
                return_type: func.ret_type,
            };

            let body = self.lower_expression(func.body, &vars)?;

            let function = ir::FunctionDefinition { prototype, body };

            if func.name.as_str() == "main" {
                if !function.prototype.parameters.is_empty() {
                    return Err(LoweringError::MalformedEntryFunction {
                        context: String::from("The main function does not take any arguments"),
                        span: func.span,
                    });
                }

                if function.prototype.return_type != Type::Int {
                    return Err(LoweringError::MalformedEntryFunction {
                        context: String::from("The main function does always return the type int"),
                        span: func.span,
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

    fn lower_expression(
        &mut self,
        exp: Expression<Type>,
        vars: &HashMap<Ident, Variable>,
    ) -> Result<ir::Expression> {
        use ir::Expression::*;
        use ir::Value;
        match exp.kind {
            ExpressionKind::Int(i) => Ok(Direct(Value::Number(i as i32))),
            ExpressionKind::Bool(b) => Ok(Direct(Value::Boolean(b))),
            ExpressionKind::Var(v) => vars
                .get(&v)
                .copied()
                .map(|v| Direct(Value::Variable(v)))
                .ok_or(LoweringError::VariableNotBound { variable: v, span: exp.span }),
            ExpressionKind::UnaOp { .. } => {
                todo!()
            }
            ExpressionKind::BinOp { op_kind: kind, lhs, rhs } => {
                let lhs = self.lower_expression(*lhs, vars)?;
                let rhs = self.lower_expression(*rhs, vars)?;
                Ok(BinaryOperation(Box::new(ir::BinaryOperation {
                    kind: match kind {
                        BinOpKind::Add => ir::BinaryOperationKind::Add,
                        BinOpKind::Sub => ir::BinaryOperationKind::Sub,
                        BinOpKind::Equals => ir::BinaryOperationKind::Equals,
                        BinOpKind::Less => ir::BinaryOperationKind::Less,
                        BinOpKind::And => ir::BinaryOperationKind::And,
                    },
                    lhs,
                    rhs,
                })))
            }
            ExpressionKind::LetIn { var, bind, body } => {
                let bind = self.lower_expression(*bind, vars)?;
                let fresh = self.fresh_variable();
                let mut extended_vars = vars.clone();
                extended_vars.insert(var, fresh);
                let body = self.lower_expression(*body, &extended_vars)?;

                Ok(LocalBinding(Box::new(ir::LocalBinding {
                    var: fresh,
                    bind,
                    body,
                })))
            }
            ExpressionKind::IfThenElse {
                condition,
                then_branch,
                else_branch,
            } => Ok(Conditional(Box::new(ir::Conditional {
                condition: self.lower_expression(*condition, vars)?,
                then_branch: self.lower_expression(*then_branch, vars)?,
                else_branch: self.lower_expression(*else_branch, vars)?,
            }))),
            ExpressionKind::Call { function, args } => {
                let mut lowered_args = Vec::with_capacity(args.len());
                for arg in args {
                    lowered_args.push(self.lower_expression(arg, vars)?);
                }

                Ok(FunctionCall {
                    fn_name: function,
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
