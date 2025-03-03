//! Syntax definitions
//!
//! This file holds the syntax defininitions for the compli language.
//! The AST is the target for the parser, then gets provided with type information by the type
//! checker and finally lowered to compli's intermediate representation.
//!
//! The AST types have a `C` context type parameter that is used to differentiate between the untyped AST
//! that is generated by the parser and the typed AST that the type checker transforms into and is
//! then used by the lowering machinery.

use crate::Span;

/// An identifier is just a string
pub type Ident = String;

/// A program where no type information is attached to the expressions
pub type UntypedProgram = Program<NoContext>;

/// A program where every expression has an attached type
pub type TypedProgram = Program<Type>;

/// Something that can be used to annotate expressions type context in the AST
pub trait TypeContext: Clone + fmt::Display {}

/// The zero-sized type that is used for an untyped AST
///
/// It is basically `()` but is displayed as `""` (important for pretty printing)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct NoContext;

impl fmt::Display for NoContext {
    fn fmt(&self, _: &mut fmt::Formatter<'_>) -> fmt::Result {
        Ok(())
    }
}

impl TypeContext for NoContext {}

/// A data type
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Float,
    Bool,
    Record(Ident),
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Int => write!(f, "int"),
            Type::Float => write!(f, "float"),
            Type::Bool => write!(f, "bool"),
            Type::Record(name) => write!(f, "{name}"),
        }
    }
}

impl TypeContext for Type {}

/// A whole compli program that consists of a number of functions
#[derive(Debug, PartialEq)]
pub struct Program<C: TypeContext> {
    pub records: Vec<Record>,
    pub functions: Vec<Function<C>>,
}

/// A record type that is the product of other types
#[derive(Debug, Clone, PartialEq)]
pub struct Record {
    pub name: Ident,
    pub fields: Vec<(String, Type)>,
    pub name_span: Span,
}

/// A function that consists of a prototype/signature and one body expression
#[derive(Debug, Clone, PartialEq)]
pub struct Function<C: TypeContext> {
    pub name: Ident,
    pub params: Vec<(Ident, Type)>,
    pub return_type: Type,
    pub body: Expression<C>,
    pub name_span: Span,
}

/// An expression is mainly a wrapper for [ExpressionKind] but with attached meta information
/// (a span and maybe some type)
#[derive(Debug, Clone, PartialEq)]
pub struct Expression<C: TypeContext> {
    pub kind: ExpressionKind<C>,
    pub span: Span,
    pub type_context: C,
}

impl<C: TypeContext> Expression<C> {
    pub fn new(kind: ExpressionKind<C>, span: Span, type_context: C) -> Self {
        Self {
            kind,
            span,
            type_context,
        }
    }
}

/// An expression
#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionKind<C: TypeContext> {
    Int(u16),
    Float(f32),
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

    RecordSelector {
        field: Ident,
        expr: Box<Expression<C>>,
    },
}

/// A unary operation
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnaryOperation {
    Neg,
    Not,
}

/// A binary operation
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinaryOperation {
    Add,
    Sub,
    Mul,
    Div,
    Equals,
    Less,
    LessEq,
    Greater,
    GreaterEq,
    And,
    Or,
}

/* PRETTY PRINTING */
// see `ptree` crate

use ptree::{print_tree, Style, TreeItem};
use std::borrow::Cow;
use std::{fmt, io};

impl<C: TypeContext> Program<C> {
    pub fn pretty_print(&self) -> io::Result<()> {
        for rec in &self.records {
            print_tree(rec)?;
        }
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
            BinaryOperation::Mul => write!(f, "*"),
            BinaryOperation::Div => write!(f, "/"),
            BinaryOperation::Equals => write!(f, "=="),
            BinaryOperation::Less => write!(f, "<"),
            BinaryOperation::LessEq => write!(f, "<="),
            BinaryOperation::Greater => write!(f, ">"),
            BinaryOperation::GreaterEq => write!(f, ">="),
            BinaryOperation::And => write!(f, "&&"),
            BinaryOperation::Or => write!(f, "||"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct RecordField {
    name: String,
    typ: Type,
}

impl TreeItem for RecordField {
    type Child = RecordField;

    fn write_self<W: io::Write>(&self, f: &mut W, style: &Style) -> io::Result<()> {
        write!(f, "{}", style.paint(format!("{}: {}", self.name, self.typ)))
    }

    fn children(&self) -> Cow<[Self::Child]> {
        vec![].into()
    }
}

impl TreeItem for Record {
    type Child = RecordField;

    fn write_self<W: io::Write>(&self, f: &mut W, style: &Style) -> io::Result<()> {
        write!(f, "{}", style.paint(format!("DATA {}", self.name.clone())))
    }

    fn children(&self) -> Cow<[Self::Child]> {
        let fields: Vec<RecordField> = self
            .fields
            .clone()
            .into_iter()
            .map(|(name, typ)| RecordField { name, typ })
            .collect();
        Cow::from(fields)
    }
}

impl<C: TypeContext> TreeItem for Function<C> {
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

impl<C: TypeContext> TreeItem for Expression<C> {
    type Child = Self;

    fn write_self<W: io::Write>(&self, f: &mut W, style: &Style) -> io::Result<()> {
        match &self.kind {
            ExpressionKind::Int(i) => {
                write!(f, "{}", style.paint(format!("{i} {}", self.type_context)))
            }
            ExpressionKind::Float(x) => {
                write!(f, "{}", style.paint(format!("{x} {}", self.type_context)))
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
                )
            }
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
            ExpressionKind::RecordSelector { field, .. } => {
                write!(f, "{}", style.paint(format!("SELECT {field} {}", self.type_context)))
            }
        }
    }

    fn children(&self) -> Cow<[Self::Child]> {
        match &self.kind {
            ExpressionKind::Int(_)
            | ExpressionKind::Float(_)
            | ExpressionKind::Bool(_)
            | ExpressionKind::Var(_) => Cow::from(vec![]),
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
            ExpressionKind::RecordSelector { expr, .. } => Cow::from(vec![*expr.clone()]),
        }
    }
}
