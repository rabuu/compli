use std::collections::HashMap;

use crate::ir;
use crate::variable::{Variable, VariableCounter};

#[derive(Debug)]
pub enum Expression {
    Int(i32),
    Bool(bool),
    Var(String),
    LetIn {
        var: String,
        bind: Box<Expression>,
        body: Box<Expression>,
    },
    Call {
        name: String,
        args: Vec<Expression>,
    },
}

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
            Expression::Int(i) => ir::Expression::Direct(ir::Value::Number(i)),
            Expression::Bool(b) => ir::Expression::Direct(ir::Value::Boolean(b)),
            Expression::Var(v) => match vars.get(&v) {
                Some(var) => ir::Expression::Direct(ir::Value::Variable(*var)),
                None => panic!("Not bound"),
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
            Expression::Call { name, mut args } => match name.as_str() {
                "+" | "<" | "and" => {
                    assert_eq!(args.len(), 2);
                    let lhs = self.lower_expression(args.remove(0), vars);
                    let rhs = self.lower_expression(args.remove(0), vars);
                    ir::Expression::BinaryOperation(Box::new(ir::BinaryOperation {
                        kind: match name.as_str() {
                            "+" => ir::BinaryOperationKind::Add,
                            "<" => ir::BinaryOperationKind::Cmp,
                            "and" => ir::BinaryOperationKind::And,
                            _ => unreachable!(),
                        },
                        lhs,
                        rhs,
                    }))
                }
                "if" => {
                    assert_eq!(args.len(), 3);
                    ir::Expression::Conditional(Box::new(ir::Conditional {
                        condition: self.lower_expression(args.remove(0), vars),
                        then_branch: self.lower_expression(args.remove(0), vars),
                        else_branch: self.lower_expression(args.remove(0), vars),
                    }))
                }
                _ => unimplemented!(),
            },
        }
    }
}
