use std::collections::HashMap;

use crate::ast::*;
use crate::ir;
use crate::{Type, Variable};

pub fn lower(program: Program) -> Option<ir::Program> {
    let mut lowerer = Lowerer::default();
    lowerer.lower_program(program)
}

#[derive(Debug, Default)]
struct Lowerer {
    fresh_variable: Variable,
}

impl Lowerer {
    fn lower_program(&mut self, program: Program) -> Option<ir::Program> {
        let mut functions = Vec::with_capacity(program.functions.len());
        let mut entry = None;
        for (func, _) in program.functions {
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
                name: func.name.clone().into(),
                parameters: param_vars
                    .into_iter()
                    .map(|(_, var, typ)| (var, typ))
                    .collect(),
                return_type: func.ret_type,
            };

            let body = self.lower_expression(func.body.0, &vars);

            let function = ir::FunctionDefinition { prototype, body };

            if func.name.as_str() == "main" {
                assert!(function.prototype.parameters.is_empty());
                assert_eq!(function.prototype.return_type, Type::Int);
                entry = Some(function.body);
            } else {
                functions.push(function);
            }
        }

        Some(ir::Program {
            entry: entry?,
            functions,
        })
    }

    fn lower_expression(
        &mut self,
        exp: Expression,
        vars: &HashMap<Ident, Variable>,
    ) -> ir::Expression {
        match exp {
            Expression::Int(i) => ir::Expression::Direct(ir::Value::Number(i as i32)),
            Expression::Bool(b) => ir::Expression::Direct(ir::Value::Boolean(b)),
            Expression::Var(v) => match vars.get(&v) {
                Some(var) => ir::Expression::Direct(ir::Value::Variable(*var)),
                None => panic!("Not bound"),
            },
            Expression::UnaOp { inner, .. } => {
                let _inner = self.lower_expression(inner.0, vars);
                todo!()
            }
            Expression::BinOp { kind, lhs, rhs } => {
                let lhs = self.lower_expression(lhs.0, vars);
                let rhs = self.lower_expression(rhs.0, vars);
                ir::Expression::BinaryOperation(Box::new(ir::BinaryOperation {
                    kind: match kind {
                        BinOpKind::Add => ir::BinaryOperationKind::Add,
                        BinOpKind::Sub => ir::BinaryOperationKind::Sub,
                        BinOpKind::Equals => ir::BinaryOperationKind::Equals,
                        BinOpKind::Less => ir::BinaryOperationKind::Less,
                        BinOpKind::And => ir::BinaryOperationKind::And,
                    },
                    lhs,
                    rhs,
                }))
            }
            Expression::LetIn { var, bind, body } => {
                let bind = self.lower_expression(bind.0, vars);
                let fresh = self.fresh_variable();
                let mut extended_vars = vars.clone();
                extended_vars.insert(var, fresh);
                let body = self.lower_expression(body.0, &extended_vars);

                ir::Expression::LocalBinding(Box::new(ir::LocalBinding {
                    var: fresh,
                    bind,
                    body,
                }))
            }
            Expression::IfThenElse {
                condition,
                then_branch,
                else_branch,
            } => ir::Expression::Conditional(Box::new(ir::Conditional {
                condition: self.lower_expression(condition.0, vars),
                then_branch: self.lower_expression(then_branch.0, vars),
                else_branch: self.lower_expression(else_branch.0, vars),
            })),
            Expression::Call { function, args } => {
                let args = args
                    .into_iter()
                    .map(|(arg, _)| self.lower_expression(arg, vars))
                    .collect();

                ir::Expression::FunctionCall {
                    fn_name: function.into(),
                    args,
                }
            }
        }
    }

    fn fresh_variable(&mut self) -> Variable {
        let fresh = self.fresh_variable;
        self.fresh_variable.0 += 1;
        fresh
    }
}
