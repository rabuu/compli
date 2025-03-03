//! Builtin runtime functions
//!
//! Here are prototype definitions for functions that exist in compli's runtime.

use crate::ir::{FunctionPrototype, Type};
use crate::Variable;

pub fn all_builtins() -> [FunctionPrototype; 6] {
    [
        FunctionPrototype {
            name: String::from("__compli_trace_int"),
            parameters: vec![(Variable::DONT_CARE, Type::Int)],
            return_type: Type::Int,
        },
        FunctionPrototype {
            name: String::from("__compli_trace_float"),
            parameters: vec![(Variable::DONT_CARE, Type::Float)],
            return_type: Type::Float,
        },
        FunctionPrototype {
            name: String::from("__compli_trace_bool"),
            parameters: vec![(Variable::DONT_CARE, Type::Bool)],
            return_type: Type::Bool,
        },
        FunctionPrototype {
            name: String::from("__compli_bool_to_int"),
            parameters: vec![(Variable::DONT_CARE, Type::Bool)],
            return_type: Type::Int,
        },
        FunctionPrototype {
            name: String::from("__compli_float_to_int"),
            parameters: vec![(Variable::DONT_CARE, Type::Float)],
            return_type: Type::Int,
        },
        FunctionPrototype {
            name: String::from("__compli_int_to_float"),
            parameters: vec![(Variable::DONT_CARE, Type::Int)],
            return_type: Type::Float,
        },
    ]
}
