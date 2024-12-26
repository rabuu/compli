use std::collections::HashMap;

use crate::ast::*;
use crate::ir;
use crate::Variable;

#[derive(Debug, Default)]
pub struct Lowerer {
    fresh_variable: Variable,
}

impl Lowerer {
    pub fn fresh_variable(&mut self) -> Variable {
        let fresh = self.fresh_variable;
        self.fresh_variable.0 += 1;
        fresh
    }

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
}
