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
    pub return_type: Type,
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
    pub fn new(kind: ExpressionKind<C>, span: Span, type_context: C) -> Self {
        Self {
            kind,
            span,
            type_context,
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

    Unary {
        op: UnaryOperation,
        inner: Box<Expression<C>>,
    },

    Binary {
        op: BinaryOperation,
        lhs: Box<Expression<C>>,
        rhs: Box<Expression<C>>,
    },

    LetIn {
        binds: Vec<(Ident, Option<Type>, Expression<C>)>,
        body: Box<Expression<C>>,
    },

    IfThenElse {
        condition: Box<Expression<C>>,
        yes: Box<Expression<C>>,
        no: Box<Expression<C>>,
    },

    Call {
        function: Ident,
        args: Vec<Expression<C>>,
    },
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOperation {
    Neg,
    Not,
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOperation {
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

impl fmt::Display for UnaryOperation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnaryOperation::Neg => write!(f, "-"),
            UnaryOperation::Not => write!(f, "!"),
        }
    }
}

impl fmt::Display for BinaryOperation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinaryOperation::Add => write!(f, "+"),
            BinaryOperation::Sub => write!(f, "-"),
            BinaryOperation::Equals => write!(f, "=="),
            BinaryOperation::Less => write!(f, "<"),
            BinaryOperation::And => write!(f, "&&"),
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
            ExpressionKind::Unary { op: kind, .. } => write!(
                f,
                "{}",
                style.paint(format!("{kind} {}", self.type_context))
            ),
            ExpressionKind::Binary { op: kind, .. } => write!(
                f,
                "{}",
                style.paint(format!("{kind} {}", self.type_context))
            ),
            ExpressionKind::LetIn { binds, .. } => {
                let mut var_list = String::new();
                for (i, (var, _, _)) in binds.iter().enumerate() {
                    var_list.push_str(var);
                    if i != binds.len() - 1 {
                        var_list.push_str(", ");
                    }
                }
                write!(
                f,
                "{}",
                style.paint(format!("LET {var_list} {}", self.type_context))
            )}
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
            ExpressionKind::Unary { inner, .. } => Cow::from(vec![*inner.clone()]),
            ExpressionKind::Binary { lhs, rhs, .. } => Cow::from(vec![*lhs.clone(), *rhs.clone()]),
            ExpressionKind::LetIn { binds, body, .. } => {
                let mut children: Vec<_> = binds.iter().map(|(_, _, bind)| bind.clone()).collect();
                children.push(*body.clone());
                Cow::from(children)
            }
            ExpressionKind::IfThenElse { condition, yes, no } => {
                Cow::from(vec![*condition.clone(), *yes.clone(), *no.clone()])
            }
            ExpressionKind::Call { args, .. } => Cow::from(args),
        }
    }
}
