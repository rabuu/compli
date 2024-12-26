use crate::Spanned;

type Ident = String;

type SpExpression = Spanned<Expression>;

#[derive(Debug)]
pub enum Expression {
    Int(u16),
    Bool(bool),
    Var(String),

    UnaOp {
        kind: UnaOpKind,
        inner: Box<SpExpression>,
    },

    BinOp {
        kind: BinOpKind,
        lhs: Box<SpExpression>,
        rhs: Box<SpExpression>,
    },

    LetIn {
        var: Ident,
        bind: Box<SpExpression>,
        body: Box<SpExpression>,
    },

    IfThenElse {
        condition: Box<SpExpression>,
        then_branch: Box<SpExpression>,
        else_branch: Box<SpExpression>,
    },

    Call {
        function: Ident,
        args: Vec<SpExpression>,
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
