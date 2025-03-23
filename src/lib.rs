//! My compiler for PLI
//!
//! The compilation pipeline:
//! 1. [parse] (source code to AST)
//! 2. [type_check] (provide type information in AST)
//! 3. [lower] (AST to IR)
//! 4. [compile] (IR to LLVM IR)

mod ast;
mod builtin;
mod codegen;
mod common;
mod ir;
mod lowering;
mod parsing;
mod type_checking;

pub use ast::{TypedAst, UntypedAst};
pub use codegen::{compile, CodegenError};
pub use ir::Program as IntermediateRepresentation;
pub use lowering::{lower, LoweringError};
pub use parsing::{parse, ParsingError};
pub use type_checking::{type_check, TypeCheckError};

pub(crate) use common::{Ident, Span, Variable};
