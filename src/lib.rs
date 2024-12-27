pub mod ast;
pub mod codegen;
mod ir;
pub mod lowering;
pub mod parsing;

type Span = std::ops::Range<usize>;
type Spanned<T> = (T, Span);

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Variable(usize);

#[derive(Debug, Clone, Copy)]
pub enum Type {
    Int,
    Bool,
}
