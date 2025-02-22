use crate::{Span, Type};

pub type Ident = String;

pub type UntypedProgram = Program<NoContext>;
pub type TypedProgram = Program<Type>;

#[derive(Debug)]
pub struct Program<C>
where
    C: fmt::Display + Clone,
{
    pub functions: Vec<Function<C>>,
}

#[derive(Debug, Clone)]
pub struct Function<C>
where
    C: fmt::Display + Clone,
{
    pub name: Ident,
    pub params: Vec<(Ident, Type)>,
    pub ret_type: Type,
    pub body: Expression<C>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Expression<C>
where
    C: fmt::Display + Clone,
{
    pub kind: ExpressionKind<C>,
    pub span: Span,
    pub type_context: C,
}

impl<C> Expression<C>
where
    C: fmt::Display + Clone,
{
    pub fn new(kind: ExpressionKind<C>, span: Span, context: C) -> Self {
        Self {
            kind,
            span,
            type_context: context,
        }
    }
}

#[derive(Debug, Clone)]
pub enum ExpressionKind<C>
where
    C: fmt::Display + Clone,
{
    Int(u16),
    Bool(bool),
    Var(Ident),

    UnaOp {
        op_kind: UnaOpKind,
        inner: Box<Expression<C>>,
    },

    BinOp {
        op_kind: BinOpKind,
        lhs: Box<Expression<C>>,
        rhs: Box<Expression<C>>,
    },

    LetIn {
        var: Ident,
        bind: Box<Expression<C>>,
        body: Box<Expression<C>>,
    },

    IfThenElse {
        condition: Box<Expression<C>>,
        then_branch: Box<Expression<C>>,
        else_branch: Box<Expression<C>>,
    },

    Call {
        function: Ident,
        args: Vec<Expression<C>>,
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

#[derive(Debug, Clone, Copy)]
pub struct NoContext;

impl fmt::Display for NoContext {
    fn fmt(&self, _: &mut fmt::Formatter<'_>) -> fmt::Result {
        Ok(())
    }
}

use ptree::{print_tree, Style, TreeItem};
use std::borrow::Cow;
use std::{fmt, io};

impl<C> Program<C>
where
    C: fmt::Display + Clone,
{
    pub fn pretty_print(&self) -> io::Result<()> {
        for func in &self.functions {
            print_tree(func)?;
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

impl<C> TreeItem for Function<C>
where
    C: fmt::Display + Clone,
{
    type Child = Expression<C>;

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
        Cow::from(vec![self.body.clone()])
    }
}

impl<C> TreeItem for Expression<C>
where
    C: fmt::Display + Clone,
{
    type Child = Self;

    fn write_self<W: io::Write>(&self, f: &mut W, style: &Style) -> io::Result<()> {
        match &self.kind {
            ExpressionKind::Int(i) => {
                write!(f, "{}", style.paint(format!("{i} {}", self.type_context)))
            }
            ExpressionKind::Bool(b) => {
                write!(f, "{}", style.paint(format!("{b} {}", self.type_context)))
            }
            ExpressionKind::Var(x) => {
                write!(f, "{}", style.paint(format!("{x} {}", self.type_context)))
            }
            ExpressionKind::UnaOp { op_kind: kind, .. } => write!(
                f,
                "{}",
                style.paint(format!("{kind} {}", self.type_context))
            ),
            ExpressionKind::BinOp { op_kind: kind, .. } => write!(
                f,
                "{}",
                style.paint(format!("{kind} {}", self.type_context))
            ),
            ExpressionKind::LetIn { var, .. } => write!(
                f,
                "{}",
                style.paint(format!("LET {var} {}", self.type_context))
            ),
            ExpressionKind::IfThenElse { .. } => write!(
                f,
                "{}",
                style.paint(format!("IF-THEN-ELSE {}", self.type_context))
            ),
            ExpressionKind::Call { function, .. } => {
                write!(
                    f,
                    "{}",
                    style.paint(format!("CALL {function} {}", self.type_context))
                )
            }
        }
    }

    fn children(&self) -> Cow<[Self::Child]> {
        match &self.kind {
            ExpressionKind::Int(_) | ExpressionKind::Bool(_) | ExpressionKind::Var(_) => {
                Cow::from(vec![])
            }
            ExpressionKind::UnaOp { inner, .. } => Cow::from(vec![*inner.clone()]),
            ExpressionKind::BinOp { lhs, rhs, .. } => Cow::from(vec![*lhs.clone(), *rhs.clone()]),
            ExpressionKind::LetIn { bind, body, .. } => {
                Cow::from(vec![*bind.clone(), *body.clone()])
            }
            ExpressionKind::IfThenElse {
                condition,
                then_branch,
                else_branch,
            } => Cow::from(vec![
                *condition.clone(),
                *then_branch.clone(),
                *else_branch.clone(),
            ]),
            ExpressionKind::Call { args, .. } => Cow::from(args),
        }
    }
}
