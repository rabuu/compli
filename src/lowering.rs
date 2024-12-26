use std::collections::HashMap;

use crate::ast::*;
use crate::ir;
use crate::variable::{Variable, VariableCounter};

#[derive(Debug, Default)]
pub struct Lowerer {
    variable_counter: VariableCounter,
}

impl Lowerer {
    pub fn lower_expression(
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
                let _inner = self.lower_expression(*inner, vars);
                todo!()
            }
            Expression::BinOp { kind, lhs, rhs } => {
                let lhs = self.lower_expression(*lhs, vars);
                let rhs = self.lower_expression(*rhs, vars);
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
                let bind = self.lower_expression(*bind, vars);
                let fresh = self.variable_counter.fresh();
                let mut extended_vars = vars.clone();
                extended_vars.insert(var, fresh);
                let body = self.lower_expression(*body, &extended_vars);

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
                condition: self.lower_expression(*condition, vars),
                then_branch: self.lower_expression(*then_branch, vars),
                else_branch: self.lower_expression(*else_branch, vars),
            })),
            Expression::Call { .. } => unimplemented!(),
        }
    }
}
