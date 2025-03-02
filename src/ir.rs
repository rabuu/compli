//! Intermediate representation defintions
//!
//! This module defines the intermediate representation of a compli program.
//! It is the target of the lowering step and is then used to generate LLVM IR from.
//!
//! Since we are using [inkwell] and its recursive builder, our intermediate representation can
//! still be quite high-level and tree-like.

use crate::Variable;

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

#[derive(Debug, Clone)]
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
pub enum Expression {
    Direct(Value),
    FunctionCall {
        function_name: String,
        args: Vec<Expression>,
    },
    LocalBinding(Box<LocalBinding>),
    BinaryOperation(Box<BinaryOperation>),
    UnaryOperation(Box<UnaryOperation>),
    Conditional(Box<Conditional>),
}

#[derive(Debug, Clone)]
pub struct LocalBinding {
    pub var: Variable,
    pub bind: Expression,
    pub body: Expression,
}

#[derive(Debug, Clone)]
pub struct BinaryOperation {
    pub kind: BinaryOperationKind,
    pub lhs: Expression,
    pub rhs: Expression,
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

#[derive(Debug, Clone)]
pub struct UnaryOperation {
    pub kind: UnaryOperationKind,
    pub inner: Expression,
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOperationKind {
    NegInt,
    NegFloat,
    Not,
}

#[derive(Debug, Clone)]
pub struct Conditional {
    pub condition: Expression,
    pub yes: Expression,
    pub no: Expression,
    pub float_mode: bool,
}

/* PRETTY PRINTING */
// see `ptree` crate

use ptree::{print_tree, TreeItem};
use std::borrow::Cow;
use std::{fmt, io};

impl Program {
    pub fn pretty_print(&self) -> io::Result<()> {
        println!("ENTRY FUNCTION:");
        print_tree(&self.entry)?;
        println!();
        for function in &self.functions {
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

impl fmt::Display for FunctionPrototype {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut fn_string = self.name.clone();
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

impl TreeItem for FunctionDefinition {
    type Child = Expression;

    fn write_self<W: io::Write>(&self, f: &mut W, style: &ptree::Style) -> io::Result<()> {
        write!(f, "{}", style.paint(self.prototype.clone()))
    }

    fn children(&self) -> Cow<[Self::Child]> {
        Cow::from(vec![self.body.clone()])
    }
}

impl TreeItem for Expression {
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
            Expression::LocalBinding(x) => write!(f, "{}", style.paint(format!("LET {}", x.var))),
            Expression::BinaryOperation(x) => write!(f, "{}", style.paint(x.kind)),
            Expression::UnaryOperation(x) => write!(f, "{}", style.paint(x.kind)),
            Expression::Conditional(_) => write!(f, "{}", style.paint("COND")),
        }
    }

    fn children(&self) -> Cow<[Self::Child]> {
        match self {
            Expression::Direct(_) => Cow::from(vec![]),
            Expression::FunctionCall { args, .. } => Cow::from(args.clone()),
            Expression::LocalBinding(x) => Cow::from(vec![x.bind.clone(), x.body.clone()]),
            Expression::BinaryOperation(x) => Cow::from(vec![x.lhs.clone(), x.rhs.clone()]),
            Expression::UnaryOperation(x) => Cow::from(vec![x.inner.clone()]),
            Expression::Conditional(x) => {
                Cow::from(vec![x.condition.clone(), x.yes.clone(), x.no.clone()])
            }
        }
    }
}
