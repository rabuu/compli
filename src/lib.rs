pub mod ast;
pub mod codegen;
mod ir;
pub mod lowering;
pub mod parsing;
mod variable;

type Span = std::ops::Range<usize>;
type Spanned<T> = (T, Span);
