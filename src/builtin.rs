//! Builtin runtime functions
//!
//! Here are prototype definitions for functions that exist in compli's runtime.

use crate::ir::FunctionPrototype;
use crate::{Type, Variable};

pub fn all_builtins() -> [FunctionPrototype; 3] {
    [
        compli_trace_int(),
        compli_trace_float(),
        compli_trace_bool(),
    ]
}

pub fn compli_trace_int() -> FunctionPrototype {
    FunctionPrototype {
        name: String::from("__compli_trace_int"),
        parameters: vec![(Variable::DONT_CARE, Type::Int)],
        return_type: Type::Int,
    }
}

pub fn compli_trace_float() -> FunctionPrototype {
    FunctionPrototype {
        name: String::from("__compli_trace_float"),
        parameters: vec![(Variable::DONT_CARE, Type::Float)],
        return_type: Type::Float,
    }
}

pub fn compli_trace_bool() -> FunctionPrototype {
    FunctionPrototype {
        name: String::from("__compli_trace_bool"),
        parameters: vec![(Variable::DONT_CARE, Type::Bool)],
        return_type: Type::Bool,
    }
}
