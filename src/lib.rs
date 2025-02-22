mod ast;
mod codegen;
mod ir;
mod lowering;
mod parsing;
mod type_checking;
mod util;

pub use codegen::{compile, CodegenError};
pub use lowering::{lower, LoweringError};
pub use parsing::{parse, ParsingError};
pub use type_checking::{type_check, TypeCheckError};
pub use util::{Span, Variable};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Type {
    Int,
    Bool,
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Int => write!(f, "int"),
            Type::Bool => write!(f, "bool"),
        }
    }
}
