#[derive(Debug)]
pub enum Value {
    Number(i32),
    Boolean(bool),
}

#[derive(Debug)]
pub enum Expression {
    Direct(Value),
    Operation {
        kind: OpKind,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    IfThenElse {
        cond: Box<Expression>,
        then_clause: Box<Expression>,
        else_clause: Box<Expression>,
    },
}

#[derive(Debug)]
pub enum OpKind {
    Add,
    And,
    Sma,
}
