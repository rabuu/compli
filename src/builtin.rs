//! Builtin runtime functions
//!
//! Here are the builtin functions listed and prototype definitions
//! for those builtin functions that exist in compli's runtime.

use crate::ir::{FunctionPrototype, Type};
use crate::{Ident, Variable};

#[derive(Debug)]
pub enum BuiltinFunction {
    Trace,
    CastInt,
    CastFloat,
    Sqrt,
    InputInt,
    InputFloat,
}

impl BuiltinFunction {
    pub fn from_name(name: Ident) -> Option<Self> {
        match name.as_str() {
            "trace" => Some(Self::Trace),
            "cast_int" => Some(Self::CastInt),
            "cast_float" => Some(Self::CastFloat),
            "sqrt" => Some(Self::Sqrt),
            "input_int" => Some(Self::InputInt),
            "input_float" => Some(Self::InputFloat),
            _ => None,
        }
    }

    pub fn parameter_number(&self) -> usize {
        match self {
            BuiltinFunction::Trace => 1,
            BuiltinFunction::CastInt => 1,
            BuiltinFunction::CastFloat => 1,
            BuiltinFunction::Sqrt => 1,
            BuiltinFunction::InputInt => 0,
            BuiltinFunction::InputFloat => 0,
        }
    }
}

pub fn all_builtin_prototypes() -> [FunctionPrototype; 9] {
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
        FunctionPrototype {
            name: String::from("__compli_input_int"),
            parameters: vec![],
            return_type: Type::Int,
        },
        FunctionPrototype {
            name: String::from("__compli_input_float"),
            parameters: vec![],
            return_type: Type::Float,
        },
    ]
}
