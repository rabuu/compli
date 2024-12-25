use crate::variable::Variable;

#[derive(Debug)]
pub enum Value {
    Number(i32),
    Boolean(bool),
    Variable(Variable),
}

#[derive(Debug)]
pub enum Expression {
    Direct(Value),
    LocalBinding(Box<LocalBinding>),
    BinaryOperation(Box<BinaryOperation>),
    Conditional(Box<Conditional>),
}

#[derive(Debug)]
pub struct LocalBinding {
    pub var: Variable,
    pub bind: Expression,
    pub body: Expression,
}

#[derive(Debug)]
pub struct BinaryOperation {
    pub kind: BinaryOperationKind,
    pub lhs: Expression,
    pub rhs: Expression,
}

#[derive(Debug)]
pub enum BinaryOperationKind {
    Add,
    Cmp,
    And,
}

#[derive(Debug)]
pub struct Conditional {
    pub condition: Expression,
    pub then_branch: Expression,
    pub else_branch: Expression,
}
