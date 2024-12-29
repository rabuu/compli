pub mod ast;
pub mod codegen;
mod ir;
pub mod lowering;
pub mod parsing;
pub mod type_checking;

pub use type_checking::type_check;

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
