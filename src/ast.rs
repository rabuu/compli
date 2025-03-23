//! Syntax definitions
//!
//! This file holds the syntax defininitions for the compli language.
//! The AST is the target for the parser, then gets provided with type information by the type
//! checker and finally lowered to compli's intermediate representation.
//!
//! The AST types have a type context parameter that is used to differentiate between the untyped AST
//! that is generated by the parser and the typed AST that the type checker transforms into and is
//! then used by the lowering machinery.

use crate::{Ident, Span};

/// An AST without any type information
pub type UntypedAst<'src> = Program<'src, NoTypeContext>;

/// An AST with attached types for each expression
pub type TypedAst<'src> = Program<'src, Type<'src>>;

/// Something that can be used to annotate expressions type context in the AST
pub trait TypeContext: Clone {
    fn tree_suffix(&self) -> String;

    fn tree_fmt<W: io::Write, D: fmt::Display>(
        &self,
        expr: D,
        f: &mut W,
        style: &Style,
    ) -> io::Result<()> {
        write!(
            f,
            "{}",
            style.paint(format!("{}{}", expr, self.tree_suffix()))
        )
    }
}

/// A zero-sized type that is used to "annotate" an untyped AST
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct NoTypeContext;

impl TypeContext for NoTypeContext {
    fn tree_suffix(&self) -> String {
        String::new()
    }
}

/// A data type
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Type<'src> {
    Int,
    Float,
    Bool,
    Record(Ident<'src>),
}

impl fmt::Display for Type<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Int => write!(f, "int"),
            Type::Float => write!(f, "float"),
            Type::Bool => write!(f, "bool"),
            Type::Record(name) => write!(f, "{name}"),
        }
    }
}

impl TypeContext for Type<'_> {
    fn tree_suffix(&self) -> String {
        format!(" [{}]", self)
    }
}

/// A whole compli program that consists of a number of functions
#[derive(Debug, PartialEq)]
pub struct Program<'src, C: TypeContext> {
    pub records: Vec<Record<'src>>,
    pub functions: Vec<Function<'src, C>>,
}

/// A record type that is the product of other types
#[derive(Debug, Clone, PartialEq)]
pub struct Record<'src> {
    pub name: Ident<'src>,
    pub fields: Vec<(Ident<'src>, Type<'src>)>,
    pub name_span: Span,
}

/// A function that consists of a prototype/signature and one body expression
#[derive(Debug, Clone, PartialEq)]
pub struct Function<'src, C: TypeContext> {
    pub name: Ident<'src>,
    pub params: Vec<(Ident<'src>, Type<'src>)>,
    pub return_type: Type<'src>,
    pub body: Expression<'src, C>,
    pub name_span: Span,
}

/// An expression is mainly a wrapper for [ExpressionKind] but with attached meta information
/// (a span and maybe some type)
#[derive(Debug, Clone, PartialEq)]
pub struct Expression<'src, C: TypeContext> {
    pub kind: ExpressionKind<'src, C>,
    pub span: Span,
    pub typ: C,
}

impl<'src, C: TypeContext> Expression<'src, C> {
    pub fn new(kind: ExpressionKind<'src, C>, span: Span, typ: C) -> Self {
        Self { kind, span, typ }
    }
}

/// An expression
#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionKind<'src, C: TypeContext> {
    Int(u16),
    Float(f32),
    Bool(bool),
    Var(Ident<'src>),

    Unary {
        op: UnaryOperation,
        inner: Box<Expression<'src, C>>,
    },

    Binary {
        op: BinaryOperation,
        lhs: Box<Expression<'src, C>>,
        rhs: Box<Expression<'src, C>>,
    },

    LetIn {
        binds: Vec<(Ident<'src>, Option<Type<'src>>, Expression<'src, C>)>,
        body: Box<Expression<'src, C>>,
    },

    IfThenElse {
        condition: Box<Expression<'src, C>>,
        yes: Box<Expression<'src, C>>,
        no: Box<Expression<'src, C>>,
    },

    Call {
        function: Ident<'src>,
        args: Vec<Expression<'src, C>>,
    },

    RecordSelector {
        field: Ident<'src>,
        expr: Box<Expression<'src, C>>,
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

impl<C: TypeContext> Program<'_, C> {
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
pub struct RecordField<'src> {
    name: Ident<'src>,
    typ: Type<'src>,
}

impl<'src> TreeItem for RecordField<'src> {
    type Child = RecordField<'src>;

    fn write_self<W: io::Write>(&self, f: &mut W, style: &Style) -> io::Result<()> {
        write!(f, "{}", style.paint(format!("{}: {}", self.name, self.typ)))
    }

    fn children(&self) -> Cow<[Self::Child]> {
        vec![].into()
    }
}

impl<'src> TreeItem for Record<'src> {
    type Child = RecordField<'src>;

    fn write_self<W: io::Write>(&self, f: &mut W, style: &Style) -> io::Result<()> {
        write!(f, "{}", style.paint(format!("REC {}", self.name.clone())))
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

impl<'src, C: TypeContext> TreeItem for Function<'src, C> {
    type Child = Expression<'src, C>;

    fn write_self<W: io::Write>(&self, f: &mut W, style: &Style) -> io::Result<()> {
        let mut fn_string = self.name.to_string();
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

impl<'src, C: TypeContext> TreeItem for Expression<'src, C> {
    type Child = Self;

    fn write_self<W: io::Write>(&self, f: &mut W, style: &Style) -> io::Result<()> {
        match &self.kind {
            ExpressionKind::Int(x) => self.typ.tree_fmt(x.to_string(), f, style),
            ExpressionKind::Float(x) => self.typ.tree_fmt(x.to_string(), f, style),
            ExpressionKind::Bool(x) => self.typ.tree_fmt(x.to_string(), f, style),
            ExpressionKind::Var(x) => self.typ.tree_fmt(x.to_string(), f, style),
            ExpressionKind::Unary { op, .. } => self.typ.tree_fmt(op.to_string(), f, style),
            ExpressionKind::Binary { op, .. } => self.typ.tree_fmt(op.to_string(), f, style),
            ExpressionKind::LetIn { binds, .. } => {
                let mut var_list = String::new();
                for (i, (var, _, _)) in binds.iter().enumerate() {
                    var_list.push_str(var);
                    if i != binds.len() - 1 {
                        var_list.push_str(", ");
                    }
                }
                self.typ.tree_fmt(format!("LET-IN {var_list}"), f, style)
            }
            ExpressionKind::IfThenElse { .. } => self.typ.tree_fmt("IF-THEN-ELSE", f, style),
            ExpressionKind::Call { function, .. } => {
                self.typ.tree_fmt(format!("CALL {function}"), f, style)
            }
            ExpressionKind::RecordSelector { field, .. } => {
                self.typ.tree_fmt(format!("SELECT {field}"), f, style)
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
