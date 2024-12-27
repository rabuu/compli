use std::collections::HashMap;

use anyhow::{anyhow, bail, Result};

use crate::ast::*;
use crate::ir;
use crate::{Type, Variable};

pub fn lower(program: Program) -> Result<ir::Program> {
    let mut lowerer = Lowerer::default();
    lowerer.lower_program(program)
}

#[derive(Debug, Default)]
struct Lowerer {
    fresh_variable: Variable,
}

impl Lowerer {
    fn lower_program(&mut self, program: Program) -> Result<ir::Program> {
        let mut vars = HashMap::new();
        // let mut globals = HashMap::new();
        //
        // for (decl, _) in program.declarations {
        //     let e = self.lower_expression(decl.expr.0, &vars);
        //     let var = self.fresh_variable();
        //     if vars.insert(decl.name.clone(), var).is_some() {
        //         bail!("Multiple declaration of name {}", decl.name);
        //     }
        //     globals.insert(var, e);
        // }

        let mut functions = HashMap::new();
        let mut main_function = None;
        for (func, _) in program.functions {
            let param_vars: Vec<(String, Variable, Type)> = func
                .params
                .into_iter()
                .map(|(arg, typ)| (arg, self.fresh_variable(), typ))
                .collect();

            let mut extended_vars = vars.clone();
            extended_vars.extend(
                param_vars
                    .clone()
                    .into_iter()
                    .map(|(name, var, _)| (name, var)),
            );

            let prototype = ir::FunctionPrototype {
                parameters: param_vars
                    .into_iter()
                    .map(|(_, var, typ)| (var, typ))
                    .collect(),
                return_type: func.ret_type,
            };

            let body = self.lower_expression(func.body.0, &extended_vars);

            let function = ir::FunctionDefinition { prototype, body };

            if func.name.as_str() == "main" {
                main_function = Some(function);
            } else {
                functions.insert(func.name, function);
            }
        }

        Ok(ir::Program {
            main_function: main_function.ok_or(anyhow!("No main function provided"))?,
            functions,
        })
    }

    fn lower_expression(
        &mut self,
        exp: Expression,
        vars: &HashMap<String, Variable>,
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
                        BinOpKind::Less => ir::BinaryOperationKind::Cmp,
                        BinOpKind::And => ir::BinaryOperationKind::And,
                        _ => todo!(),
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
            Expression::Call { .. } => unimplemented!(),
        }
    }

    fn fresh_variable(&mut self) -> Variable {
        let fresh = self.fresh_variable;
        self.fresh_variable.0 += 1;
        fresh
    }
}
