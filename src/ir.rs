use crate::{Type, Variable};

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

#[derive(Debug, Clone)]
pub enum Value {
    Number(i32),
    Boolean(bool),
    Variable(Variable),
}

#[derive(Debug, Clone)]
pub enum Expression {
    Direct(Value),
    FunctionCall {
        fn_name: String,
        args: Vec<Expression>,
    },
    LocalBinding(Box<LocalBinding>),
    BinaryOperation(Box<BinaryOperation>),
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
    Add,
    Sub,
    Equals,
    Less,
    And,
}

#[derive(Debug, Clone)]
pub struct Conditional {
    pub condition: Expression,
    pub then_branch: Expression,
    pub else_branch: Expression,
}

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
            BinaryOperationKind::Add => write!(f, "ADD"),
            BinaryOperationKind::Sub => write!(f, "SUB"),
            BinaryOperationKind::Equals => write!(f, "EQUALS"),
            BinaryOperationKind::Less => write!(f, "LESS"),
            BinaryOperationKind::And => write!(f, "AND"),
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
                Value::Number(n) => write!(f, "{}", style.paint(n)),
                Value::Boolean(b) => write!(f, "{}", style.paint(b)),
                Value::Variable(v) => write!(f, "{}", style.paint(v)),
            },
            Expression::FunctionCall { fn_name, .. } => {
                write!(f, "{}", style.paint(format!("CALL {fn_name}")))
            }
            Expression::LocalBinding(x) => write!(f, "{}", style.paint(format!("LET {}", x.var))),
            Expression::BinaryOperation(x) => write!(f, "{}", style.paint(x.kind)),
            Expression::Conditional(_) => write!(f, "{}", style.paint("COND")),
        }
    }

    fn children(&self) -> Cow<[Self::Child]> {
        match self {
            Expression::Direct(_) => Cow::from(vec![]),
            Expression::FunctionCall { args, .. } => Cow::from(args.clone()),
            Expression::LocalBinding(x) => Cow::from(vec![x.bind.clone(), x.body.clone()]),
            Expression::BinaryOperation(x) => Cow::from(vec![x.lhs.clone(), x.rhs.clone()]),
            Expression::Conditional(x) => Cow::from(vec![
                x.condition.clone(),
                x.then_branch.clone(),
                x.else_branch.clone(),
            ]),
        }
    }
}
