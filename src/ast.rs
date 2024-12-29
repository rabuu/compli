use crate::{Spanned, Type};

pub type Ident = String;

#[derive(Debug)]
pub struct Program {
    pub functions: Vec<Spanned<Function>>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Ident,
    pub params: Vec<(Ident, Type)>,
    pub ret_type: Type,
    pub body: Spanned<Expression>,
}

#[derive(Debug, Clone)]
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

use ptree::{print_tree, Style, TreeItem};
use std::borrow::Cow;
use std::{fmt, io};

impl Program {
    pub fn pretty_print(&self) -> io::Result<()> {
        for func in &self.functions {
            print_tree(&func.0)?;
        }
        Ok(())
    }
}

impl fmt::Display for UnaOpKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnaOpKind::Neg => write!(f, "-"),
            UnaOpKind::Not => write!(f, "!"),
        }
    }
}

impl fmt::Display for BinOpKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinOpKind::Add => write!(f, "+"),
            BinOpKind::Sub => write!(f, "-"),
            BinOpKind::Equals => write!(f, "=="),
            BinOpKind::Less => write!(f, "<"),
            BinOpKind::And => write!(f, "&&"),
        }
    }
}

impl TreeItem for Function {
    type Child = Expression;

    fn write_self<W: io::Write>(&self, f: &mut W, style: &Style) -> io::Result<()> {
        let mut fn_string = self.name.clone();
        fn_string.push('(');
        for (i, (param, typ)) in self.params.iter().enumerate() {
            fn_string.push_str(param);
            fn_string.push_str(": ");
            fn_string.push_str(&typ.to_string());

            if i != self.params.len() - 1 {
                fn_string.push_str(", ");
            }
        }
        fn_string.push(')');

        write!(f, "{}", style.paint(fn_string))
    }

    fn children(&self) -> Cow<[Self::Child]> {
        Cow::from(vec![self.body.clone().0])
    }
}

impl TreeItem for Expression {
    type Child = Self;

    fn write_self<W: io::Write>(&self, f: &mut W, style: &Style) -> io::Result<()> {
        match self {
            Expression::Int(i) => write!(f, "{}", style.paint(i)),
            Expression::Bool(b) => write!(f, "{}", style.paint(b)),
            Expression::Var(x) => write!(f, "{}", style.paint(x)),
            Expression::UnaOp { kind, .. } => write!(f, "{}", style.paint(kind)),
            Expression::BinOp { kind, .. } => write!(f, "{}", style.paint(kind)),
            Expression::LetIn { var, .. } => write!(f, "{}", style.paint(format!("LET {var}"))),
            Expression::IfThenElse { .. } => write!(f, "{}", style.paint("IF-THEN-ELSE")),
            Expression::Call { function, .. } => {
                write!(f, "{}", style.paint(format!("CALL {function}")))
            }
        }
    }

    fn children(&self) -> Cow<[Self::Child]> {
        match self {
            Expression::Int(_) | Expression::Bool(_) | Expression::Var(_) => Cow::from(vec![]),
            Expression::UnaOp { inner, .. } => Cow::from(vec![inner.clone().0]),
            Expression::BinOp { lhs, rhs, .. } => Cow::from(vec![lhs.clone().0, rhs.clone().0]),
            Expression::LetIn { bind, body, .. } => Cow::from(vec![bind.clone().0, body.clone().0]),
            Expression::IfThenElse {
                condition,
                then_branch,
                else_branch,
            } => Cow::from(vec![
                condition.clone().0,
                then_branch.clone().0,
                else_branch.clone().0,
            ]),
            Expression::Call { args, .. } => {
                let args: Vec<Self> = args.iter().map(|(arg, _)| arg).cloned().collect();
                Cow::from(args)
            }
        }
    }
}
