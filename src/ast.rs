type Ident = String;

#[derive(Debug)]
pub enum Expression {
    Int(u16),
    Bool(bool),
    Var(String),

    UnaOp {
        kind: UnaOpKind,
        inner: Box<Expression>,
    },

    BinOp {
        kind: BinOpKind,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },

    LetIn {
        var: Ident,
        bind: Box<Expression>,
        body: Box<Expression>,
    },

    IfThenElse {
        condition: Box<Expression>,
        then_branch: Box<Expression>,
        else_branch: Box<Expression>,
    },

    Call {
        function: Ident,
        args: Vec<Expression>,
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
