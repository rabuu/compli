#[derive(Debug)]
pub enum Value {
    Number(i32),
    Boolean(bool),
}

#[derive(Debug)]
pub enum Expression {
    Direct(Value),
    BinaryOperation(Box<BinaryOperation>),
    Conditional(Box<Conditional>),
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
