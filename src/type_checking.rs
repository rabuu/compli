//! Type checking
//!
//! This module is responsible for checking if the types of all expressions in an AST are valid.
//! Also, this type checker transforms the AST into a "typed AST" so that the compiler can leverage
//! all the type information at a later point. The main interface is the [type_check] function.

use std::collections::{HashMap, HashSet};

use miette::Diagnostic;
use thiserror::Error;

use crate::{ast, builtin, Ident, Span};

#[derive(Debug, Error, Diagnostic)]
pub enum TypeCheckError {
    #[error("Expected an expression of type `{expected}` but got `{actual}`")]
    UnexpectedType {
        expected: String,
        actual: String,

        #[label("here")]
        span: Span,
    },

    #[error("Expected an expression of type `{expected1}` or `{expected2}` but got `{actual}`")]
    UnexpectedTypeOfTwo {
        expected1: String,
        expected2: String,
        actual: String,

        #[label("here")]
        span: Span,
    },

    #[error("The name `{name}` is not bound")]
    NotBound {
        name: String,

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
        name: String,

        #[label("illegal name")]
        span: Span,
    },

    #[error("There is already a record with the name `{name}`")]
    #[diagnostic(help("The name is reserved for the record constructor"))]
    FunctionSameNameAsRecord {
        name: String,

        #[label("function definition")]
        span: Span,
    },

    #[error("There are multiple function definitions with the name `{name}`")]
    MultipleFunctionDefinitions {
        name: String,

        #[label("second definition")]
        span: Span,
    },

    #[error("Parameter `{param_name}` has unknown type `{type_name}`")]
    UnknownParameterType {
        param_name: String,
        type_name: String,

        #[label("in this definition")]
        span: Span,
    },

    #[error("The function `{function_name}` has an unknown return type `{type_name}`")]
    UnknownReturnType {
        function_name: String,
        type_name: String,

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
        name: String,

        #[label("second definition")]
        span: Span,
    },

    #[error("Record field `{field_name}` has unknown type `{type_name}`")]
    #[diagnostic(help(
        "A record must be defined before it can be used in another record definition"
    ))]
    UnknownRecordFieldType {
        field_name: String,
        type_name: String,

        #[label("in this definition")]
        span: Span,
    },

    #[error("Multiple fields in one record are called `{field_name}`")]
    MultipleFieldNames {
        field_name: String,

        #[label("in this definition")]
        span: Span,
    },

    #[error("Tried to select field `{field_name}` but expression is of type `{typ}`")]
    IllegalSelector {
        field_name: String,
        typ: String,

        #[label("here")]
        span: Span,
    },

    #[error("The type `{typ}` cannot be traced")]
    #[diagnostic(help("Only primitives (int, float, bool) can be traced"))]
    UntracableType {
        typ: String,

        #[label("this expression")]
        span: Span,
    },

    #[error("Cannot cast `{typ}` to `{target}`")]
    ImpossibleCast {
        typ: String,
        target: String,

        #[label("here")]
        span: Span,
    },
}

type Result<T> = std::result::Result<T, TypeCheckError>;

/// Check and store the types of all expressions
pub fn type_check(program: ast::UntypedAst) -> Result<ast::TypedAst> {
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

fn check_record_definitions<'src>(
    records: &[ast::Record<'src>],
) -> Result<HashMap<Ident<'src>, Vec<(Ident<'src>, ast::Type<'src>)>>> {
    let mut defined_records = HashMap::new();
    for record in records {
        if defined_records.contains_key(&record.name) {
            return Err(TypeCheckError::MultipleRecordDefinitions {
                name: record.name.to_string(),
                span: record.name_span,
            });
        }

        let mut field_names = HashSet::new();
        for (field_name, field_type) in &record.fields {
            if field_names.contains(field_name) {
                return Err(TypeCheckError::MultipleFieldNames {
                    field_name: field_name.to_string(),
                    span: record.name_span,
                });
            }

            if let ast::Type::Record(name) = field_type {
                if !defined_records.contains_key(name) {
                    return Err(TypeCheckError::UnknownRecordFieldType {
                        field_name: field_name.to_string(),
                        type_name: name.to_string(),
                        span: record.name_span,
                    });
                }
            }

            field_names.insert(field_name);
        }

        defined_records.insert(record.name, record.fields.clone());
    }

    Ok(defined_records)
}

/// The main state during type checking
///
/// This state keeps track of function prototypes and which functions were already checked.
struct TypeChecker<'src> {
    prototypes: HashMap<Ident<'src>, (Vec<ast::Type<'src>>, ast::Type<'src>)>,
    defined_functions: HashSet<Ident<'src>>,

    defined_records: HashMap<Ident<'src>, Vec<(Ident<'src>, ast::Type<'src>)>>,
}

impl<'src> TypeChecker<'src> {
    fn new(
        prototypes: HashMap<Ident<'src>, (Vec<ast::Type<'src>>, ast::Type<'src>)>,
        defined_records: HashMap<Ident<'src>, Vec<(Ident<'src>, ast::Type<'src>)>>,
    ) -> Self {
        Self {
            prototypes,
            defined_functions: HashSet::new(),
            defined_records,
        }
    }

    fn check_function(
        &mut self,
        function: ast::Function<'src, ast::NoTypeContext>,
    ) -> Result<ast::Function<'src, ast::Type<'src>>> {
        // forbid builtin & runtime names
        let builtin_name = builtin::BuiltinFunction::from_name(function.name).is_some();
        if builtin_name || function.name.as_str().starts_with("__compli") {
            return Err(TypeCheckError::IllegalFunctionName {
                name: function.name.to_string(),
                span: function.name_span,
            });
        }

        if self.defined_records.contains_key(&function.name) {
            return Err(TypeCheckError::FunctionSameNameAsRecord {
                name: function.name.to_string(),
                span: function.name_span,
            });
        }

        if !self.defined_functions.insert(function.name) {
            return Err(TypeCheckError::MultipleFunctionDefinitions {
                name: function.name.to_string(),
                span: function.name_span,
            });
        }

        for (param_name, param_type) in &function.params {
            if let ast::Type::Record(name) = param_type {
                if !self.defined_records.contains_key(name) {
                    return Err(TypeCheckError::UnknownParameterType {
                        param_name: param_name.to_string(),
                        type_name: name.to_string(),
                        span: function.name_span,
                    });
                }
            }
        }

        if let ast::Type::Record(name) = &function.return_type {
            if !self.defined_records.contains_key(name) {
                return Err(TypeCheckError::UnknownReturnType {
                    function_name: function.name.to_string(),
                    type_name: name.to_string(),
                    span: function.name_span,
                });
            }
        }

        let vars = function.params.iter().copied().collect();
        let typed_body = self.infer_expr(function.body, &vars)?;
        expect_type(&function.return_type, &typed_body.typ, typed_body.span)?;

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
        expr: ast::Expression<'src, ast::NoTypeContext>,
        vars: &HashMap<Ident<'src>, ast::Type<'src>>,
    ) -> Result<ast::Expression<'src, ast::Type<'src>>> {
        match expr.kind {
            ast::ExpressionKind::Int(x) => Ok(ast::Expression {
                kind: ast::ExpressionKind::Int(x),
                span: expr.span,
                typ: ast::Type::Int,
            }),

            ast::ExpressionKind::Float(x) => Ok(ast::Expression {
                kind: ast::ExpressionKind::Float(x),
                span: expr.span,
                typ: ast::Type::Float,
            }),

            ast::ExpressionKind::Bool(x) => Ok(ast::Expression {
                kind: ast::ExpressionKind::Bool(x),
                span: expr.span,
                typ: ast::Type::Bool,
            }),

            ast::ExpressionKind::Var(x) => {
                let typ = vars
                    .get(&x)
                    .cloned()
                    .ok_or_else(|| TypeCheckError::NotBound {
                        name: x.to_string(),
                        span: expr.span,
                    })?;

                Ok(ast::Expression {
                    kind: ast::ExpressionKind::Var(x),
                    span: expr.span,
                    typ,
                })
            }

            ast::ExpressionKind::Unary { op, inner } => {
                let arg = self.infer_expr(*inner, vars)?;
                let typ = match op {
                    ast::UnaryOperation::Neg => {
                        expect_type_of_two(&ast::Type::Int, &ast::Type::Float, &arg.typ, arg.span)?;
                        arg.typ
                    }
                    ast::UnaryOperation::Not => {
                        expect_type(&ast::Type::Bool, &arg.typ, arg.span)?;
                        ast::Type::Bool
                    }
                };

                Ok(ast::Expression {
                    kind: ast::ExpressionKind::Unary {
                        op,
                        inner: Box::new(arg),
                    },
                    span: expr.span,
                    typ,
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
                            &typed_lhs.typ,
                            typed_lhs.span,
                        )?;
                        expect_type(&typed_lhs.typ, &typed_rhs.typ, typed_rhs.span)?;
                        typed_lhs.typ
                    }
                    ast::BinaryOperation::Equals
                    | ast::BinaryOperation::Less
                    | ast::BinaryOperation::LessEq
                    | ast::BinaryOperation::Greater
                    | ast::BinaryOperation::GreaterEq => {
                        expect_type_of_two(
                            &ast::Type::Int,
                            &ast::Type::Float,
                            &typed_lhs.typ,
                            typed_lhs.span,
                        )?;
                        expect_type(&typed_lhs.typ, &typed_rhs.typ, typed_rhs.span)?;
                        ast::Type::Bool
                    }
                    ast::BinaryOperation::And | ast::BinaryOperation::Or => {
                        expect_type(&ast::Type::Bool, &typed_lhs.typ, typed_lhs.span)?;
                        expect_type(&ast::Type::Bool, &typed_rhs.typ, typed_rhs.span)?;
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
                    typ,
                })
            }

            ast::ExpressionKind::LetIn { binds, body } => {
                let mut extended_vars = vars.clone();

                let mut typed_binds = Vec::with_capacity(binds.len());
                for (var, annotation, bind) in binds {
                    let typed_bind = self.infer_expr(bind, &extended_vars)?;

                    if let Some(ref annotation) = annotation {
                        expect_type(annotation, &typed_bind.typ, typed_bind.span)?;
                    }

                    extended_vars.insert(var, typed_bind.typ);
                    typed_binds.push((var, annotation, typed_bind));
                }

                let typed_body = self.infer_expr(*body, &extended_vars)?;
                let typ = typed_body.typ;

                Ok(ast::Expression {
                    kind: ast::ExpressionKind::LetIn {
                        binds: typed_binds,
                        body: Box::new(typed_body),
                    },
                    span: expr.span,
                    typ,
                })
            }

            ast::ExpressionKind::IfThenElse { condition, yes, no } => {
                let typed_condition = self.infer_expr(*condition, vars)?;
                expect_type(&ast::Type::Bool, &typed_condition.typ, typed_condition.span)?;

                let typed_yes = self.infer_expr(*yes, vars)?;
                let typed_no = self.infer_expr(*no, vars)?;
                expect_type(&typed_yes.typ, &typed_no.typ, typed_no.span)?;

                let typ = typed_yes.typ;

                Ok(ast::Expression {
                    kind: ast::ExpressionKind::IfThenElse {
                        condition: Box::new(typed_condition),
                        yes: Box::new(typed_yes),
                        no: Box::new(typed_no),
                    },
                    span: expr.span,
                    typ,
                })
            }

            ast::ExpressionKind::Call { function, args } => {
                // forbid calling the main function
                if function.as_str() == "main" {
                    return Err(TypeCheckError::CallToMain { span: expr.span });
                }

                // builtin functions
                if let Some(builtin) = builtin::BuiltinFunction::from_name(function) {
                    if args.len() != builtin.parameter_number() {
                        return Err(TypeCheckError::WrongNumberOfArguments {
                            expected: builtin.parameter_number(),
                            actual: args.len(),
                            span: expr.span,
                        });
                    }

                    let mut args = args;
                    let mut typed_args = Vec::with_capacity(builtin.parameter_number());

                    for _ in 0..builtin.parameter_number() {
                        typed_args.push(self.infer_expr(args.remove(0), vars)?);
                    }

                    if builtin.parameter_number() == 1 {
                        let typ = typed_args[0].typ;
                        match builtin {
                            builtin::BuiltinFunction::Trace
                                if matches!(typ, ast::Type::Record(_)) =>
                            {
                                return Err(TypeCheckError::UntracableType {
                                    typ: typ.to_string(),
                                    span: expr.span,
                                });
                            }
                            builtin::BuiltinFunction::CastInt
                                if !matches!(typ, ast::Type::Float | ast::Type::Bool) =>
                            {
                                return Err(TypeCheckError::ImpossibleCast {
                                    typ: typ.to_string(),
                                    target: ast::Type::Int.to_string(),
                                    span: expr.span,
                                });
                            }
                            builtin::BuiltinFunction::CastFloat
                                if !matches!(typ, ast::Type::Int) =>
                            {
                                return Err(TypeCheckError::ImpossibleCast {
                                    typ: typ.to_string(),
                                    target: ast::Type::Float.to_string(),
                                    span: expr.span,
                                });
                            }
                            builtin::BuiltinFunction::Sqrt if !matches!(typ, ast::Type::Float) => {
                                return Err(TypeCheckError::UnexpectedType {
                                    expected: ast::Type::Float.to_string(),
                                    actual: typ.to_string(),
                                    span: expr.span,
                                });
                            }
                            _ => (),
                        }
                    }

                    let return_type = match builtin {
                        builtin::BuiltinFunction::Trace => typed_args[0].typ,
                        builtin::BuiltinFunction::CastInt => ast::Type::Int,
                        builtin::BuiltinFunction::CastFloat => ast::Type::Float,
                        builtin::BuiltinFunction::Sqrt => ast::Type::Float,
                        builtin::BuiltinFunction::InputInt => ast::Type::Int,
                        builtin::BuiltinFunction::InputFloat => ast::Type::Float,
                    };

                    return Ok(ast::Expression {
                        kind: ast::ExpressionKind::Call {
                            function,
                            args: typed_args,
                        },
                        span: expr.span,
                        typ: return_type,
                    });
                }

                let (params, return_type) = match self.defined_records.get(&function) {
                    Some(fields) => (
                        fields.clone().into_iter().map(|(_, typ)| typ).collect(),
                        ast::Type::Record(function),
                    ),
                    None => self
                        .prototypes
                        .get(&function)
                        .ok_or_else(|| TypeCheckError::NotBound {
                            name: function.to_string(),
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
                    expect_type(param, &typed_arg.typ, typed_arg.span)?;
                    typed_args.push(typed_arg);
                }

                Ok(ast::Expression {
                    kind: ast::ExpressionKind::Call {
                        function,
                        args: typed_args,
                    },
                    span: expr.span,
                    typ: return_type,
                })
            }
            ast::ExpressionKind::RecordSelector { expr: inner, field } => {
                let typed_inner = self.infer_expr(*inner, vars)?;

                let ast::Type::Record(name) = &typed_inner.typ else {
                    return Err(TypeCheckError::IllegalSelector {
                        field_name: field.to_string(),
                        typ: typed_inner.typ.to_string(),
                        span: expr.span,
                    });
                };

                let Some(fields) = self.defined_records.get(name) else {
                    return Err(TypeCheckError::IllegalSelector {
                        field_name: field.to_string(),
                        typ: typed_inner.typ.to_string(),
                        span: expr.span,
                    });
                };

                let Some(typ) = fields.iter().find_map(|(field_name, field_type)| {
                    (*field_name == field).then_some(field_type)
                }) else {
                    return Err(TypeCheckError::IllegalSelector {
                        field_name: field.to_string(),
                        typ: typed_inner.typ.to_string(),
                        span: expr.span,
                    });
                };

                Ok(ast::Expression {
                    kind: ast::ExpressionKind::RecordSelector {
                        expr: Box::new(typed_inner),
                        field,
                    },
                    span: expr.span,
                    typ: *typ,
                })
            }
        }
    }
}

/// Assert that two types are equal
fn expect_type<'src>(
    expected: &ast::Type<'src>,
    actual: &ast::Type<'src>,
    span: Span,
) -> Result<()> {
    if *actual == *expected {
        Ok(())
    } else {
        Err(TypeCheckError::UnexpectedType {
            expected: expected.to_string(),
            actual: actual.to_string(),
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
            expected1: expected1.to_string(),
            expected2: expected2.to_string(),
            actual: actual.to_string(),
            span,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{lex, parse};

    #[test]
    fn unexpected_type() {
        let src = "def foo: int = true";
        let tokens = lex(src).unwrap();
        let ast = parse(&tokens, src.len()).unwrap();
        let err = type_check(ast).unwrap_err();
        assert!(matches!(err, TypeCheckError::UnexpectedType { .. }));
    }

    #[test]
    fn not_bound() {
        let src = "def foo: int = x";
        let tokens = lex(src).unwrap();
        let ast = parse(&tokens, src.len()).unwrap();
        let err = type_check(ast).unwrap_err();
        assert!(matches!(err, TypeCheckError::NotBound { .. }));
    }

    #[test]
    fn call_to_main() {
        let src = "def foo: int = main()";
        let tokens = lex(src).unwrap();
        let ast = parse(&tokens, src.len()).unwrap();
        let err = type_check(ast).unwrap_err();
        assert!(matches!(err, TypeCheckError::CallToMain { .. }));
    }

    #[test]
    fn illegal_function_name() {
        for program in [
            "def trace: int = 0",
            "def __compli_something: int = 0",
            "def cast_int: int = 0",
            "def sqrt: float = 1.0",
        ] {
            let src = program;
            let tokens = lex(src).unwrap();
            let ast = parse(&tokens, src.len()).unwrap();
            let err = type_check(ast).unwrap_err();
            assert!(matches!(err, TypeCheckError::IllegalFunctionName { .. }));
        }
    }

    #[test]
    fn function_same_name_as_record() {
        let src = "rec record = x: int \n def record: int = 1";
        let tokens = lex(src).unwrap();
        let ast = parse(&tokens, src.len()).unwrap();
        let err = type_check(ast).unwrap_err();
        assert!(matches!(
            err,
            TypeCheckError::FunctionSameNameAsRecord { .. }
        ));
    }

    #[test]
    fn multiple_function_definitions() {
        let src = "def foo: int = 1 \n def foo: int = 2";
        let tokens = lex(src).unwrap();
        let ast = parse(&tokens, src.len()).unwrap();
        let err = type_check(ast).unwrap_err();
        assert!(matches!(
            err,
            TypeCheckError::MultipleFunctionDefinitions { .. }
        ));
    }

    #[test]
    fn unknown_parameter_type() {
        let src = "def foo(x: something): int = 0";
        let tokens = lex(src).unwrap();
        let ast = parse(&tokens, src.len()).unwrap();
        let err = type_check(ast).unwrap_err();
        assert!(matches!(err, TypeCheckError::UnknownParameterType { .. }));
    }

    #[test]
    fn unknown_return_type() {
        let src = "def foo: something = something()";
        let tokens = lex(src).unwrap();
        let ast = parse(&tokens, src.len()).unwrap();
        let err = type_check(ast).unwrap_err();
        assert!(matches!(err, TypeCheckError::UnknownReturnType { .. }));
    }

    #[test]
    fn wrong_number_of_arguments() {
        let src = "def foo(x: int, y: int): int = x + y \n def bar: int = foo(1)";
        let tokens = lex(src).unwrap();
        let ast = parse(&tokens, src.len()).unwrap();
        let err = type_check(ast).unwrap_err();
        assert!(matches!(err, TypeCheckError::WrongNumberOfArguments { .. }));
    }

    #[test]
    fn multiple_record_definitions() {
        let src = "rec foo = x: int \n rec foo = x: float";
        let tokens = lex(src).unwrap();
        let ast = parse(&tokens, src.len()).unwrap();
        let err = type_check(ast).unwrap_err();
        assert!(matches!(
            err,
            TypeCheckError::MultipleRecordDefinitions { .. }
        ));
    }

    #[test]
    fn unknown_record_field_type() {
        let src = "rec foo = x: something";
        let tokens = lex(src).unwrap();
        let ast = parse(&tokens, src.len()).unwrap();
        let err = type_check(ast).unwrap_err();
        assert!(matches!(err, TypeCheckError::UnknownRecordFieldType { .. }));

        let src = "rec foo = x: bar \n rec bar = x: int";
        let tokens = lex(src).unwrap();
        let ast = parse(&tokens, src.len()).unwrap();
        let err = type_check(ast).unwrap_err();
        assert!(matches!(err, TypeCheckError::UnknownRecordFieldType { .. }));
    }

    #[test]
    fn multiple_field_names() {
        let src = "rec foo = x: int, x: int";
        let tokens = lex(src).unwrap();
        let ast = parse(&tokens, src.len()).unwrap();
        let err = type_check(ast).unwrap_err();
        assert!(matches!(err, TypeCheckError::MultipleFieldNames { .. }));
    }

    #[test]
    fn illegal_selector() {
        let src = "rec foo = x: int \n def bar(foo: foo): int = foo.y";
        let tokens = lex(src).unwrap();
        let ast = parse(&tokens, src.len()).unwrap();
        let err = type_check(ast).unwrap_err();
        assert!(matches!(err, TypeCheckError::IllegalSelector { .. }));
    }

    #[test]
    fn untracable_type() {
        let src = "rec foo = x: int \n def bar: foo = trace(foo(1))";
        let tokens = lex(src).unwrap();
        let ast = parse(&tokens, src.len()).unwrap();
        let err = type_check(ast).unwrap_err();
        assert!(matches!(err, TypeCheckError::UntracableType { .. }));
    }

    #[test]
    fn impossible_cast() {
        let src = "rec foo = x: int \n def bar: int = cast_int(foo(1))";
        let tokens = lex(src).unwrap();
        let ast = parse(&tokens, src.len()).unwrap();
        let err = type_check(ast).unwrap_err();
        assert!(matches!(err, TypeCheckError::ImpossibleCast { .. }));
    }
}
