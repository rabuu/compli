//! Intermediate representation definitions
//!
//! This module defines the intermediate representation of a compli program.
//! It is the target of the lowering step and is then used to generate LLVM IR from.
//!
//! Since we are using [inkwell] and its recursive builder, our intermediate representation can
//! still be quite high-level and tree-like.

use crate::{Ident, Variable};

#[derive(Debug)]
pub struct Program<'src> {
    pub entry: Expression<'src>,
    pub functions: Vec<FunctionDefinition<'src>>,
}

impl Program<'_> {
    pub fn skeleton(&self) -> Vec<FunctionPrototype> {
        self.functions.iter().map(|f| f.prototype.clone()).collect()
    }
}

#[derive(Debug, Clone)]
pub struct FunctionDefinition<'src> {
    pub prototype: FunctionPrototype<'src>,
    pub body: Expression<'src>,
}

#[derive(Debug, Clone)]
pub struct FunctionPrototype<'src> {
    pub name: Ident<'src>,
    pub parameters: Vec<(Variable, Type)>,
    pub return_type: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Float,
    Bool,
    Record(Vec<Type>),
}

#[derive(Debug, Clone)]
pub enum Value {
    Integer(i32),
    Float(f32),
    Boolean(bool),
    Variable(Variable),
}

#[derive(Debug, Clone)]
pub enum Expression<'src> {
    Direct(Value),
    FunctionCall {
        function_name: Ident<'src>,
        args: Vec<Expression<'src>>,
    },
    RecordConstructor {
        record_fields: Vec<Type>,
        args: Vec<Expression<'src>>,
    },
    RecordSelector {
        record: Box<Expression<'src>>,
        index: usize,
    },
    LocalBinding {
        var: Variable,
        bind: Box<Expression<'src>>,
        body: Box<Expression<'src>>,
    },
    BinaryOperation {
        kind: BinaryOperationKind,
        lhs: Box<Expression<'src>>,
        rhs: Box<Expression<'src>>,
    },
    UnaryOperation {
        kind: UnaryOperationKind,
        inner: Box<Expression<'src>>,
    },
    Conditional {
        condition: Box<Expression<'src>>,
        yes: Box<Expression<'src>>,
        no: Box<Expression<'src>>,
        typ: Type,
    },
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOperationKind {
    AddInt,
    SubInt,
    MulInt,
    DivInt,
    EqualsInt,
    LessInt,
    LessEqInt,
    GreaterInt,
    GreaterEqInt,
    AddFloat,
    SubFloat,
    MulFloat,
    DivFloat,
    EqualsFloat,
    LessFloat,
    LessEqFloat,
    GreaterFloat,
    GreaterEqFloat,
    And,
    Or,
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOperationKind {
    NegInt,
    NegFloat,
    Not,
}

/* PRETTY PRINTING */
// see `ptree` crate

use ptree::{print_tree, TreeItem};
use std::borrow::Cow;
use std::{fmt, io};

impl Program<'_> {
    pub fn pretty_print(&self) -> io::Result<()> {
        println!("ENTRY FUNCTION:");
        print_tree(&self.entry)?;
        println!();
        for function in &self.functions {
            print!("FUNCTION ");
            print_tree(function)?;
        }

        Ok(())
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Int => write!(f, "int"),
            Type::Float => write!(f, "float"),
            Type::Bool => write!(f, "bool"),
            Type::Record(rec) => {
                write!(f, "{{")?;
                for (i, field) in rec.iter().enumerate() {
                    write!(f, "{field}")?;
                    if i != rec.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "}}")
            }
        }
    }
}

impl fmt::Display for FunctionPrototype<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut fn_string = self.name.to_string();
        fn_string.push('(');
        for (i, (param, typ)) in self.parameters.iter().enumerate() {
            fn_string.push_str(&param.to_string());
            fn_string.push_str(": ");
            fn_string.push_str(&typ.to_string());

            if i != self.parameters.len() - 1 {
                fn_string.push_str(", ");
            }
        }
        fn_string.push(')');

        write!(f, "{fn_string}")
    }
}

impl fmt::Display for BinaryOperationKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinaryOperationKind::AddInt => write!(f, "ADD"),
            BinaryOperationKind::SubInt => write!(f, "SUB"),
            BinaryOperationKind::MulInt => write!(f, "MUL"),
            BinaryOperationKind::DivInt => write!(f, "DIV"),
            BinaryOperationKind::EqualsInt => write!(f, "EQUALS"),
            BinaryOperationKind::LessInt => write!(f, "LESS"),
            BinaryOperationKind::LessEqInt => write!(f, "LESSEQ"),
            BinaryOperationKind::GreaterInt => write!(f, "GREATER"),
            BinaryOperationKind::GreaterEqInt => write!(f, "GREATEREQ"),
            BinaryOperationKind::AddFloat => write!(f, "FADD"),
            BinaryOperationKind::SubFloat => write!(f, "FSUB"),
            BinaryOperationKind::MulFloat => write!(f, "FMUL"),
            BinaryOperationKind::DivFloat => write!(f, "FDIV"),
            BinaryOperationKind::EqualsFloat => write!(f, "FEQUALS"),
            BinaryOperationKind::LessFloat => write!(f, "FLESS"),
            BinaryOperationKind::LessEqFloat => write!(f, "FLESSEQ"),
            BinaryOperationKind::GreaterFloat => write!(f, "FGREATER"),
            BinaryOperationKind::GreaterEqFloat => write!(f, "FGREATEREQ"),
            BinaryOperationKind::And => write!(f, "AND"),
            BinaryOperationKind::Or => write!(f, "OR"),
        }
    }
}

impl fmt::Display for UnaryOperationKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnaryOperationKind::NegInt => write!(f, "NEG"),
            UnaryOperationKind::NegFloat => write!(f, "FNEG"),
            UnaryOperationKind::Not => write!(f, "NOT"),
        }
    }
}

impl<'src> TreeItem for FunctionDefinition<'src> {
    type Child = Expression<'src>;

    fn write_self<W: io::Write>(&self, f: &mut W, style: &ptree::Style) -> io::Result<()> {
        write!(f, "{}", style.paint(self.prototype.clone()))
    }

    fn children(&self) -> Cow<[Self::Child]> {
        Cow::from(vec![self.body.clone()])
    }
}

impl TreeItem for Expression<'_> {
    type Child = Self;

    fn write_self<W: io::Write>(&self, f: &mut W, style: &ptree::Style) -> io::Result<()> {
        match self {
            Expression::Direct(value) => match value {
                Value::Integer(n) => write!(f, "{}", style.paint(n)),
                Value::Float(x) => write!(f, "{}", style.paint(x)),
                Value::Boolean(b) => write!(f, "{}", style.paint(b)),
                Value::Variable(v) => write!(f, "{}", style.paint(v)),
            },
            Expression::FunctionCall { function_name, .. } => {
                write!(f, "{}", style.paint(format!("CALL {function_name}")))
            }
            Expression::RecordConstructor { record_fields, .. } => {
                write!(
                    f,
                    "{}",
                    style.paint(format!("CONSTRUCT {}", Type::Record(record_fields.clone())))
                )
            }
            Expression::RecordSelector { index, .. } => {
                write!(f, "{}", style.paint(format!("SELECT {index}")))
            }
            Expression::LocalBinding { var, .. } => {
                write!(f, "{}", style.paint(format!("LOCAL {var}")))
            }
            Expression::BinaryOperation { kind, .. } => write!(f, "{}", style.paint(kind)),
            Expression::UnaryOperation { kind, .. } => write!(f, "{}", style.paint(kind)),
            Expression::Conditional { .. } => write!(f, "{}", style.paint("COND")),
        }
    }

    fn children(&self) -> Cow<[Self::Child]> {
        match self {
            Expression::Direct(_) => Cow::from(vec![]),
            Expression::FunctionCall { args, .. } => Cow::from(args.clone()),
            Expression::RecordConstructor { args, .. } => Cow::from(args.clone()),
            Expression::RecordSelector { record, .. } => Cow::from(vec![*record.clone()]),
            Expression::LocalBinding { bind, body, .. } => {
                Cow::from(vec![*bind.clone(), *body.clone()])
            }
            Expression::BinaryOperation { lhs, rhs, .. } => {
                Cow::from(vec![*lhs.clone(), *rhs.clone()])
            }
            Expression::UnaryOperation { inner, .. } => Cow::from(vec![*inner.clone()]),
            Expression::Conditional {
                condition, yes, no, ..
            } => Cow::from(vec![*condition.clone(), *yes.clone(), *no.clone()]),
        }
    }
}
