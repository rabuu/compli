use crate::{Type, Variable};

#[derive(Debug)]
pub struct Program {
    pub entry: Expression,
    pub functions: Vec<FunctionDefinition>,
}

impl Program {
    pub fn skeleton(&self) -> Vec<FunctionPrototype> {
        self.functions.iter().map(|f| f.prototype.clone()).collect()
    }
}

#[derive(Debug)]
pub struct FunctionDefinition {
    pub prototype: FunctionPrototype,
    pub body: Expression,
}

#[derive(Debug, Clone)]
pub struct FunctionPrototype {
    pub name: String,
    pub parameters: Vec<(Variable, Type)>,
    pub return_type: Type,
}

#[derive(Debug)]
pub enum Value {
    Number(i32),
    Boolean(bool),
    Variable(Variable),
}

#[derive(Debug)]
pub enum Expression {
    Direct(Value),
    FunctionCall {
        fn_name: String,
        args: Vec<Expression>,
    },
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
    Sub,
    Equals,
    Less,
    And,
}

#[derive(Debug)]
pub struct Conditional {
    pub condition: Expression,
    pub then_branch: Expression,
    pub else_branch: Expression,
}
