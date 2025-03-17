//! Type checking
//!
//! This module is responsible for checking if the types of all expressions in an AST are valid.
//! Also, this type checker transforms the AST into a "typed AST" so that the compiler can leverage
//! all the type information at a later point. The main interface is the [type_check] function.

use std::collections::{HashMap, HashSet};

use miette::Diagnostic;
use thiserror::Error;

use crate::{ast, builtin, Span};

#[derive(Debug, Error, Diagnostic)]
pub enum TypeCheckError {
    #[error("Expected an expression of type `{expected}` but got `{actual}`")]
    UnexpectedType {
        expected: ast::Type,
        actual: ast::Type,

        #[label("here")]
        span: Span,
    },

    #[error("Expected an expression of type `{expected1}` or `{expected2}` but got `{actual}`")]
    UnexpectedTypeOfTwo {
        expected1: ast::Type,
        expected2: ast::Type,
        actual: ast::Type,

        #[label("here")]
        span: Span,
    },

    #[error("The name `{name}` is not bound")]
    NotBound {
        name: ast::Ident,

        #[label("unknown name")]
        span: Span,
    },

    #[error("You cannot call the `main` function")]
    CallToMain {
        #[label("here")]
        span: Span,
    },

    #[error("The name `{name}` can not be user-defined")]
    IllegalFunctionName {
        name: ast::Ident,

        #[label("illegal name")]
        span: Span,
    },

    #[error("There is already a record with the name `{name}`")]
    #[diagnostic(help("The name is reserved for the record constructor"))]
    FunctionSameNameAsRecord {
        name: ast::Ident,

        #[label("function definition")]
        span: Span,
    },

    #[error("There are multiple function definitions with the name `{name}`")]
    MultipleFunctionDefinitions {
        name: ast::Ident,

        #[label("second definition")]
        span: Span,
    },

    #[error("Parameter `{param_name}` has unknown type `{type_name}`")]
    UnknownParameterType {
        param_name: ast::Ident,
        type_name: ast::Ident,

        #[label("in this definition")]
        span: Span,
    },

    #[error("The function `{function_name}` has an unknown return type `{type_name}`")]
    UnknownReturnType {
        function_name: ast::Ident,
        type_name: ast::Ident,

        #[label("in this definition")]
        span: Span,
    },

    #[error("Expected {expected} arguments but got {actual}")]
    WrongNumberOfArguments {
        expected: usize,
        actual: usize,

        #[label("this function call")]
        span: Span,
    },

    #[error("There are multiple record definitions with the name `{name}`")]
    MultipleRecordDefinitions {
        name: ast::Ident,

        #[label("second definition")]
        span: Span,
    },

    #[error("Record field `{field_name}` has unknown type `{type_name}`")]
    #[diagnostic(help(
        "A record must be defined before it can be used in another record definition"
    ))]
    UnknownRecordFieldType {
        field_name: ast::Ident,
        type_name: ast::Ident,

        #[label("in this definition")]
        span: Span,
    },

    #[error("Multiple fields in one record are called `{field_name}`")]
    MultipleFieldNames {
        field_name: ast::Ident,

        #[label("in this definition")]
        span: Span,
    },

    #[error("Tried to select field `{field_name}` but expression is of type `{typ}`")]
    IllegalSelector {
        field_name: ast::Ident,
        typ: ast::Type,

        #[label("here")]
        span: Span,
    },

    #[error("The type `{typ}` cannot be traced")]
    #[diagnostic(help("Only primitives (int, float, bool) can be traced"))]
    UntracableType {
        typ: ast::Type,

        #[label("this expression")]
        span: Span,
    },

    #[error("Cannot cast `{typ}` to `{target}`")]
    ImpossibleCast {
        typ: ast::Type,
        target: ast::Type,

        #[label("here")]
        span: Span,
    },
}

type Result<T> = std::result::Result<T, TypeCheckError>;

/// Check and store the types of all expressions
pub fn type_check(program: ast::UntypedProgram) -> Result<ast::TypedProgram> {
    let prototypes = program
        .functions
        .iter()
        .cloned()
        .map(|f| {
            (
                f.name,
                (
                    f.params.into_iter().map(|(_, typ)| typ).collect(),
                    f.return_type,
                ),
            )
        })
        .collect();

    let defined_records = check_record_definitions(&program.records)?;

    let mut checker = TypeChecker::new(prototypes, defined_records);

    let mut typed_functions = Vec::with_capacity(program.functions.len());
    for function in program.functions {
        typed_functions.push(checker.check_function(function)?);
    }

    Ok(ast::Program {
        functions: typed_functions,
        records: program.records,
    })
}

fn check_record_definitions(
    records: &[ast::Record],
) -> Result<HashMap<ast::Ident, Vec<(ast::Ident, ast::Type)>>> {
    let mut defined_records = HashMap::new();
    for record in records {
        if defined_records.contains_key(&record.name) {
            return Err(TypeCheckError::MultipleRecordDefinitions {
                name: record.name.clone(),
                span: record.name_span,
            });
        }

        let mut field_names = HashSet::new();
        for (field_name, field_type) in &record.fields {
            if field_names.contains(field_name) {
                return Err(TypeCheckError::MultipleFieldNames {
                    field_name: field_name.clone(),
                    span: record.name_span,
                });
            }

            if let ast::Type::Record(name) = field_type {
                if !defined_records.contains_key(name) {
                    return Err(TypeCheckError::UnknownRecordFieldType {
                        field_name: field_name.clone(),
                        type_name: name.clone(),
                        span: record.name_span,
                    });
                }
            }

            field_names.insert(field_name);
        }

        defined_records.insert(record.name.clone(), record.fields.clone());
    }

    Ok(defined_records)
}

/// The main state during type checking
///
/// This state keeps track of function prototypes and which functions were already checked.
struct TypeChecker {
    prototypes: HashMap<ast::Ident, (Vec<ast::Type>, ast::Type)>,
    defined_functions: HashSet<ast::Ident>,

    defined_records: HashMap<ast::Ident, Vec<(ast::Ident, ast::Type)>>,
}

impl TypeChecker {
    fn new(
        prototypes: HashMap<ast::Ident, (Vec<ast::Type>, ast::Type)>,
        defined_records: HashMap<ast::Ident, Vec<(ast::Ident, ast::Type)>>,
    ) -> Self {
        Self {
            prototypes,
            defined_functions: HashSet::new(),
            defined_records,
        }
    }

    fn check_function(
        &mut self,
        function: ast::Function<ast::NoContext>,
    ) -> Result<ast::Function<ast::Type>> {
        let name = function.name.as_str();

        // forbid builtin & runtime names
        let builtin_name = builtin::BuiltinFunction::from_name(name).is_some();
        if builtin_name || name.starts_with("__compli") {
            return Err(TypeCheckError::IllegalFunctionName {
                name: function.name,
                span: function.name_span,
            });
        }

        if self.defined_records.contains_key(&function.name) {
            return Err(TypeCheckError::FunctionSameNameAsRecord {
                name: function.name,
                span: function.name_span,
            });
        }

        if !self.defined_functions.insert(function.name.clone()) {
            return Err(TypeCheckError::MultipleFunctionDefinitions {
                name: function.name,
                span: function.name_span,
            });
        }

        for (param_name, param_type) in &function.params {
            if let ast::Type::Record(name) = param_type {
                if !self.defined_records.contains_key(name) {
                    return Err(TypeCheckError::UnknownParameterType {
                        param_name: param_name.clone(),
                        type_name: name.clone(),
                        span: function.name_span,
                    });
                }
            }
        }

        if let ast::Type::Record(name) = &function.return_type {
            if !self.defined_records.contains_key(name) {
                return Err(TypeCheckError::UnknownReturnType {
                    function_name: function.name,
                    type_name: name.clone(),
                    span: function.name_span,
                });
            }
        }

        let vars = function.params.iter().cloned().collect();
        let typed_body = self.infer_expr(function.body, &vars)?;
        expect_type(
            &function.return_type,
            &typed_body.type_context,
            typed_body.span,
        )?;

        Ok(ast::Function {
            name: function.name,
            params: function.params,
            return_type: function.return_type,
            body: typed_body,
            name_span: function.name_span,
        })
    }

    fn infer_expr(
        &self,
        expr: ast::Expression<ast::NoContext>,
        vars: &HashMap<ast::Ident, ast::Type>,
    ) -> Result<ast::Expression<ast::Type>> {
        match expr.kind {
            ast::ExpressionKind::Int(x) => Ok(ast::Expression {
                kind: ast::ExpressionKind::Int(x),
                span: expr.span,
                type_context: ast::Type::Int,
            }),

            ast::ExpressionKind::Float(x) => Ok(ast::Expression {
                kind: ast::ExpressionKind::Float(x),
                span: expr.span,
                type_context: ast::Type::Float,
            }),

            ast::ExpressionKind::Bool(x) => Ok(ast::Expression {
                kind: ast::ExpressionKind::Bool(x),
                span: expr.span,
                type_context: ast::Type::Bool,
            }),

            ast::ExpressionKind::Var(x) => {
                let typ = vars
                    .get(&x)
                    .cloned()
                    .ok_or_else(|| TypeCheckError::NotBound {
                        name: x.clone(),
                        span: expr.span,
                    })?;

                Ok(ast::Expression {
                    kind: ast::ExpressionKind::Var(x),
                    span: expr.span,
                    type_context: typ,
                })
            }

            ast::ExpressionKind::Unary { op, inner } => {
                let arg = self.infer_expr(*inner, vars)?;
                let typ = match op {
                    ast::UnaryOperation::Neg => {
                        expect_type_of_two(
                            &ast::Type::Int,
                            &ast::Type::Float,
                            &arg.type_context,
                            arg.span,
                        )?;
                        arg.type_context.clone()
                    }
                    ast::UnaryOperation::Not => {
                        expect_type(&ast::Type::Bool, &arg.type_context, arg.span)?;
                        ast::Type::Bool
                    }
                };

                Ok(ast::Expression {
                    kind: ast::ExpressionKind::Unary {
                        op,
                        inner: Box::new(arg),
                    },
                    span: expr.span,
                    type_context: typ,
                })
            }

            ast::ExpressionKind::Binary { op, lhs, rhs } => {
                let typed_lhs = self.infer_expr(*lhs, vars)?;
                let typed_rhs = self.infer_expr(*rhs, vars)?;
                let typ = match op {
                    ast::BinaryOperation::Add
                    | ast::BinaryOperation::Sub
                    | ast::BinaryOperation::Mul
                    | ast::BinaryOperation::Div => {
                        expect_type_of_two(
                            &ast::Type::Int,
                            &ast::Type::Float,
                            &typed_lhs.type_context,
                            typed_lhs.span,
                        )?;
                        expect_type(
                            &typed_lhs.type_context,
                            &typed_rhs.type_context,
                            typed_rhs.span,
                        )?;
                        typed_lhs.type_context.clone()
                    }
                    ast::BinaryOperation::Equals
                    | ast::BinaryOperation::Less
                    | ast::BinaryOperation::LessEq
                    | ast::BinaryOperation::Greater
                    | ast::BinaryOperation::GreaterEq => {
                        expect_type_of_two(
                            &ast::Type::Int,
                            &ast::Type::Float,
                            &typed_lhs.type_context,
                            typed_lhs.span,
                        )?;
                        expect_type(
                            &typed_lhs.type_context,
                            &typed_rhs.type_context,
                            typed_rhs.span,
                        )?;
                        ast::Type::Bool
                    }
                    ast::BinaryOperation::And | ast::BinaryOperation::Or => {
                        expect_type(&ast::Type::Bool, &typed_lhs.type_context, typed_lhs.span)?;
                        expect_type(&ast::Type::Bool, &typed_rhs.type_context, typed_rhs.span)?;
                        ast::Type::Bool
                    }
                };

                Ok(ast::Expression {
                    kind: ast::ExpressionKind::Binary {
                        op,
                        lhs: Box::new(typed_lhs),
                        rhs: Box::new(typed_rhs),
                    },
                    span: expr.span,
                    type_context: typ,
                })
            }

            ast::ExpressionKind::LetIn { binds, body } => {
                let mut extended_vars = vars.clone();

                let mut typed_binds = Vec::with_capacity(binds.len());
                for (var, annotation, bind) in binds {
                    let typed_bind = self.infer_expr(bind, &extended_vars)?;

                    if let Some(ref annotation) = annotation {
                        expect_type(annotation, &typed_bind.type_context, typed_bind.span)?;
                    }

                    extended_vars.insert(var.clone(), typed_bind.type_context.clone());
                    typed_binds.push((var, annotation, typed_bind));
                }

                let typed_body = self.infer_expr(*body, &extended_vars)?;
                let typ = typed_body.type_context.clone();

                Ok(ast::Expression {
                    kind: ast::ExpressionKind::LetIn {
                        binds: typed_binds,
                        body: Box::new(typed_body),
                    },
                    span: expr.span,
                    type_context: typ,
                })
            }

            ast::ExpressionKind::IfThenElse { condition, yes, no } => {
                let typed_condition = self.infer_expr(*condition, vars)?;
                expect_type(
                    &ast::Type::Bool,
                    &typed_condition.type_context,
                    typed_condition.span,
                )?;

                let typed_yes = self.infer_expr(*yes, vars)?;
                let typed_no = self.infer_expr(*no, vars)?;
                expect_type(
                    &typed_yes.type_context,
                    &typed_no.type_context,
                    typed_no.span,
                )?;
                let typ = typed_yes.type_context.clone();

                Ok(ast::Expression {
                    kind: ast::ExpressionKind::IfThenElse {
                        condition: Box::new(typed_condition),
                        yes: Box::new(typed_yes),
                        no: Box::new(typed_no),
                    },
                    span: expr.span,
                    type_context: typ,
                })
            }

            ast::ExpressionKind::Call { function, args } => {
                // forbid calling the main function
                if function.as_str() == "main" {
                    return Err(TypeCheckError::CallToMain { span: expr.span });
                }

                // builtin functions
                if let Some(builtin) = builtin::BuiltinFunction::from_name(function.as_str()) {
                    // at the moment, all builtins are unary
                    if args.len() != 1 {
                        return Err(TypeCheckError::WrongNumberOfArguments {
                            expected: 1,
                            actual: args.len(),
                            span: expr.span,
                        });
                    }
                    let mut args = args;
                    let typed_arg = self.infer_expr(args.swap_remove(0), vars)?;
                    let typ = typed_arg.type_context.clone();

                    match builtin {
                        builtin::BuiltinFunction::Trace if matches!(typ, ast::Type::Record(_)) => {
                            return Err(TypeCheckError::UntracableType {
                                typ,
                                span: expr.span,
                            });
                        }
                        builtin::BuiltinFunction::CastInt
                            if !matches!(typ, ast::Type::Float | ast::Type::Bool) =>
                        {
                            return Err(TypeCheckError::ImpossibleCast {
                                typ,
                                target: ast::Type::Int,
                                span: expr.span,
                            });
                        }
                        builtin::BuiltinFunction::CastFloat if !matches!(typ, ast::Type::Int) => {
                            return Err(TypeCheckError::ImpossibleCast {
                                typ,
                                target: ast::Type::Float,
                                span: expr.span,
                            });
                        }
                        builtin::BuiltinFunction::Sqrt if !matches!(typ, ast::Type::Float) => {
                            return Err(TypeCheckError::UnexpectedType {
                                expected: ast::Type::Float,
                                actual: typ,
                                span: expr.span,
                            });
                        }
                        _ => (),
                    }

                    let return_type = match builtin {
                        builtin::BuiltinFunction::Trace => typ,
                        builtin::BuiltinFunction::CastInt => ast::Type::Int,
                        builtin::BuiltinFunction::CastFloat => ast::Type::Float,
                        builtin::BuiltinFunction::Sqrt => ast::Type::Float,
                    };

                    return Ok(ast::Expression {
                        kind: ast::ExpressionKind::Call {
                            function,
                            args: vec![typed_arg],
                        },
                        span: expr.span,
                        type_context: return_type,
                    });
                }

                let (params, return_type) = match self.defined_records.get(&function) {
                    Some(fields) => (
                        fields.clone().into_iter().map(|(_, typ)| typ).collect(),
                        ast::Type::Record(function.clone()),
                    ),
                    None => self
                        .prototypes
                        .get(&function)
                        .ok_or_else(|| TypeCheckError::NotBound {
                            name: function.clone(),
                            span: expr.span,
                        })?
                        .clone(),
                };

                if args.len() != params.len() {
                    return Err(TypeCheckError::WrongNumberOfArguments {
                        expected: params.len(),
                        actual: args.len(),
                        span: expr.span,
                    });
                }

                let mut typed_args = Vec::with_capacity(args.len());
                for (arg, param) in args.into_iter().zip(params.iter()) {
                    let typed_arg = self.infer_expr(arg, vars)?;
                    expect_type(param, &typed_arg.type_context, typed_arg.span)?;
                    typed_args.push(typed_arg);
                }

                Ok(ast::Expression {
                    kind: ast::ExpressionKind::Call {
                        function,
                        args: typed_args,
                    },
                    span: expr.span,
                    type_context: return_type.clone(),
                })
            }
            ast::ExpressionKind::RecordSelector { expr: inner, field } => {
                let typed_inner = self.infer_expr(*inner, vars)?;
                let inner_typ = typed_inner.type_context.clone();

                let ast::Type::Record(name) = &typed_inner.type_context else {
                    return Err(TypeCheckError::IllegalSelector {
                        field_name: field,
                        typ: inner_typ,
                        span: expr.span,
                    });
                };

                let Some(fields) = self.defined_records.get(name) else {
                    return Err(TypeCheckError::IllegalSelector {
                        field_name: field,
                        typ: inner_typ,
                        span: expr.span,
                    });
                };

                let Some(typ) = fields.iter().find_map(|(field_name, field_type)| {
                    (*field_name == field).then_some(field_type)
                }) else {
                    return Err(TypeCheckError::IllegalSelector {
                        field_name: field,
                        typ: inner_typ,
                        span: expr.span,
                    });
                };

                Ok(ast::Expression {
                    kind: ast::ExpressionKind::RecordSelector {
                        expr: Box::new(typed_inner),
                        field,
                    },
                    span: expr.span,
                    type_context: typ.clone(),
                })
            }
        }
    }
}

/// Assert that two types are equal
fn expect_type(expected: &ast::Type, actual: &ast::Type, span: Span) -> Result<()> {
    if *actual == *expected {
        Ok(())
    } else {
        Err(TypeCheckError::UnexpectedType {
            expected: expected.clone(),
            actual: actual.clone(),
            span,
        })
    }
}

/// Assert that a type is one of a number of others
fn expect_type_of_two(
    expected1: &ast::Type,
    expected2: &ast::Type,
    actual: &ast::Type,
    span: Span,
) -> Result<()> {
    if actual == expected1 || actual == expected2 {
        Ok(())
    } else {
        Err(TypeCheckError::UnexpectedTypeOfTwo {
            expected1: expected1.clone(),
            expected2: expected2.clone(),
            actual: actual.clone(),
            span,
        })
    }
}
