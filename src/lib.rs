mod ast;
mod codegen;
mod ir;
mod lowering;
mod parsing;
mod type_checking;

pub use codegen::{CodegenError, compile};
pub use lowering::{LoweringError, lower};
pub use parsing::{ParsingError, parse};
pub use type_checking::{TypeCheckError, type_check};

type Span = std::ops::Range<usize>;
type Spanned<T> = (T, Span);

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Variable(usize);

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
