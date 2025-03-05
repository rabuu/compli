//! Builtin runtime functions
//!
//! Here are the builtin functions listed and prototype definitions
//! for those builtin functions that exist in compli's runtime.

use crate::ir::{FunctionPrototype, Type};
use crate::Variable;

#[derive(Debug)]
pub enum BuiltinFunction {
    Trace,
    CastInt,
    CastFloat,
    Sqrt,
}

impl BuiltinFunction {
    pub fn from_name(name: &str) -> Option<Self> {
        match name {
            "trace" => Some(Self::Trace),
            "cast_int" => Some(Self::CastInt),
            "cast_float" => Some(Self::CastFloat),
            "sqrt" => Some(Self::Sqrt),
            _ => None,
        }
    }
}

pub fn all_builtin_prototypes() -> [FunctionPrototype; 7] {
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
        FunctionPrototype {
            name: String::from("__compli_sqrt"),
            parameters: vec![(Variable::DONT_CARE, Type::Float)],
            return_type: Type::Float,
        },
    ]
}
