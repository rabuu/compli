use std::collections::HashMap;

use crate::ir;
use crate::variable::{Variable, VariableCounter};

type Ident = String;

#[derive(Debug)]
pub enum Expression {
    Int(u16),
    Bool(bool),
    Var(String),

    UnaOp(Box<UnaOp>),
    BinOp(Box<BinOp>),

    LetIn(Box<LetIn>),
    IfThenElse(Box<IfThenElse>),

    Call {
        function: Ident,
        args: Vec<Expression>,
    },
}

#[derive(Debug)]
pub struct LetIn {
    pub var: Ident,
    pub bind: Expression,
    pub body: Expression,
}

#[derive(Debug)]
pub struct UnaOp {
    pub kind: UnaOpKind,
    pub inner: Expression,
}

#[derive(Debug, Clone, Copy)]
pub enum UnaOpKind {
    Neg,
    Not,
}

#[derive(Debug)]
pub struct BinOp {
    pub kind: BinOpKind,
    pub lhs: Expression,
    pub rhs: Expression,
}

#[derive(Debug, Clone, Copy)]
pub enum BinOpKind {
    Add,
    Sub,
    Equals,
    Less,
    And,
}

#[derive(Debug)]
pub struct IfThenElse {
    pub condition: Expression,
    pub then_branch: Expression,
    pub else_branch: Expression,
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
            Expression::Int(i) => ir::Expression::Direct(ir::Value::Number(i as i32)),
            Expression::Bool(b) => ir::Expression::Direct(ir::Value::Boolean(b)),
            Expression::Var(v) => match vars.get(&v) {
                Some(var) => ir::Expression::Direct(ir::Value::Variable(*var)),
                None => panic!("Not bound"),
            }
            Expression::UnaOp(unaop) => {
                let _inner = self.lower_expression(unaop.inner, vars);
                todo!()
            }
            Expression::BinOp(binop) => {
                let lhs = self.lower_expression(binop.lhs, vars);
                let rhs = self.lower_expression(binop.rhs, vars);
                ir::Expression::BinaryOperation(Box::new(ir::BinaryOperation {
                    kind: match binop.kind {
                        BinOpKind::Add => ir::BinaryOperationKind::Add,
                        BinOpKind::Less => ir::BinaryOperationKind::Cmp,
                        BinOpKind::And => ir::BinaryOperationKind::And,
                        _ => todo!(),
                    },
                    lhs,
                    rhs,
                }))
            }
            Expression::LetIn(letin) => {
                let bind = self.lower_expression(letin.bind, vars);
                let fresh = self.variable_counter.fresh();
                let mut extended_vars = vars.clone();
                extended_vars.insert(letin.var, fresh);
                let body = self.lower_expression(letin.body, &extended_vars);

                ir::Expression::LocalBinding(Box::new(ir::LocalBinding {
                    var: fresh,
                    bind,
                    body,
                }))
            }
            Expression::IfThenElse(ite) => ir::Expression::Conditional(Box::new(ir::Conditional {
                condition: self.lower_expression(ite.condition, vars),
                then_branch: self.lower_expression(ite.then_branch, vars),
                else_branch: self.lower_expression(ite.else_branch, vars),
            })),
            Expression::Call { .. } => unimplemented!(),
        }
    }
}
