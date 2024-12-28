use crate::{Spanned, Type};

pub type Ident = String;

#[derive(Debug)]
pub struct Program {
    pub functions: Vec<Spanned<Function>>,
}

#[derive(Debug)]
pub struct Function {
    pub name: Ident,
    pub params: Vec<(Ident, Type)>,
    pub ret_type: Type,
    pub body: Spanned<Expression>,
}

#[derive(Debug)]
pub enum Expression {
    Int(u16),
    Bool(bool),
    Var(Ident),

    UnaOp {
        kind: UnaOpKind,
        inner: Box<Spanned<Expression>>,
    },

    BinOp {
        kind: BinOpKind,
        lhs: Box<Spanned<Expression>>,
        rhs: Box<Spanned<Expression>>,
    },

    LetIn {
        var: Ident,
        bind: Box<Spanned<Expression>>,
        body: Box<Spanned<Expression>>,
    },

    IfThenElse {
        condition: Box<Spanned<Expression>>,
        then_branch: Box<Spanned<Expression>>,
        else_branch: Box<Spanned<Expression>>,
    },

    Call {
        function: Ident,
        args: Vec<Spanned<Expression>>,
    },
}

#[derive(Debug, Clone, Copy)]
pub enum UnaOpKind {
    Neg,
    Not,
}

#[derive(Debug, Clone, Copy)]
pub enum BinOpKind {
    Add,
    Sub,
    Equals,
    Less,
    And,
}
