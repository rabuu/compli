//! Lowering
//!
//! This module is responsible for lowering the AST (with type checkers type information) down to
//! our intermediate representation ([ir]). The main interface is the [lower] function.

use std::collections::HashMap;

use miette::Diagnostic;
use thiserror::Error;

use crate::{ast, builtin, ir, Ident, Span, Variable};

#[derive(Debug, Error, Diagnostic)]
pub enum LoweringError {
    #[error("No main function defined as entry point for the program")]
    #[diagnostic(help("An entry function must be defined with `func main(): int`"))]
    NoEntryFunction,

    #[error("The main function is malformed")]
    MalformedEntryFunction {
        #[help]
        context: String,

        #[label]
        span: Span,
    },

    #[error("The variable {variable} is not bound")]
    VariableNotBound {
        variable: String,

        #[label("this variable")]
        span: Span,
    },
}

type Result<T> = std::result::Result<T, LoweringError>;

/// Turn the AST (with type information) into IR
pub fn lower(ast::TypedAst { records, functions }: ast::TypedAst) -> Result<ir::Program> {
    let mut lowerer = Lowerer::new(records);
    lowerer.lower_program(functions)
}

/// The main state during lowering
///
/// This state keeps track of which variables can be used as fresh.
#[derive(Debug)]
struct Lowerer<'src> {
    fresh_variable: Variable,
    record_definitions: HashMap<Ident<'src>, Vec<(Ident<'src>, ir::Type)>>,
}

impl<'src> Lowerer<'src> {
    fn new(records: Vec<ast::Record<'src>>) -> Self {
        let mut lowerer = Self {
            fresh_variable: Variable::default(),
            record_definitions: HashMap::default(),
        };

        for ast::Record { name, fields, .. } in records {
            let lowered_fields = fields
                .into_iter()
                .map(|(field_name, typ)| (field_name, lowerer.lower_type(typ)))
                .collect();

            lowerer.record_definitions.insert(name, lowered_fields);
        }

        lowerer
    }

    fn lower_program(
        &mut self,
        functions: Vec<ast::Function<'src, ast::Type>>,
    ) -> Result<ir::Program<'src>> {
        let mut lowered_functions = Vec::with_capacity(functions.len());
        let mut entry = None;
        for func in functions {
            let func_is_main = func.name.as_str() == "main";
            let name_span = func.name_span;

            let function = self.lower_function(func)?;

            // check if entry/main function is valid
            if func_is_main {
                if !function.prototype.parameters.is_empty() {
                    return Err(LoweringError::MalformedEntryFunction {
                        context: String::from("The main function must not take any arguments"),
                        span: name_span,
                    });
                }

                if function.prototype.return_type != ir::Type::Int {
                    return Err(LoweringError::MalformedEntryFunction {
                        context: String::from("The main function must always return the type int"),
                        span: name_span,
                    });
                }

                entry = Some(function.body);
            } else {
                lowered_functions.push(function);
            }
        }

        Ok(ir::Program {
            entry: entry.ok_or(LoweringError::NoEntryFunction)?,
            functions: lowered_functions,
        })
    }

    fn lower_function(
        &mut self,
        func: ast::Function<'src, ast::Type>,
    ) -> Result<ir::FunctionDefinition<'src>> {
        let param_vars: Vec<(Ident, Variable, ast::Type)> = func
            .params
            .into_iter()
            .map(|(arg, typ)| (arg, self.fresh_variable(), typ))
            .collect();

        let vars = param_vars
            .clone()
            .into_iter()
            .map(|(name, var, _)| (name, var))
            .collect();

        let prototype = ir::FunctionPrototype {
            name: func.name,
            parameters: param_vars
                .into_iter()
                .map(|(_, var, typ)| (var, self.lower_type(typ)))
                .collect(),
            return_type: self.lower_type(func.return_type),
        };

        let body = self.lower_expression(func.body, &vars)?;

        Ok(ir::FunctionDefinition { prototype, body })
    }

    fn lower_expression(
        &mut self,
        expr: ast::Expression<'src, ast::Type>,
        vars: &HashMap<Ident<'src>, Variable>,
    ) -> Result<ir::Expression<'src>> {
        match expr.kind {
            ast::ExpressionKind::Int(i) => Ok(ir::Expression::Direct(ir::Value::Integer(i as i32))),
            ast::ExpressionKind::Float(f) => Ok(ir::Expression::Direct(ir::Value::Float(f))),
            ast::ExpressionKind::Bool(b) => Ok(ir::Expression::Direct(ir::Value::Boolean(b))),
            ast::ExpressionKind::Var(v) => vars
                .get(&v)
                .copied()
                .map(|v| ir::Expression::Direct(ir::Value::Variable(v)))
                .ok_or(LoweringError::VariableNotBound {
                    variable: v.to_string(),
                    span: expr.span,
                }),
            ast::ExpressionKind::Unary { op, inner } => {
                let float_mode = inner.typ == ast::Type::Float;

                let arg = self.lower_expression(*inner, vars)?;
                Ok(ir::Expression::UnaryOperation {
                    kind: match op {
                        ast::UnaryOperation::Neg if float_mode => ir::UnaryOperationKind::NegFloat,
                        ast::UnaryOperation::Neg => ir::UnaryOperationKind::NegInt,
                        ast::UnaryOperation::Not => ir::UnaryOperationKind::Not,
                    },
                    inner: Box::new(arg),
                })
            }
            ast::ExpressionKind::Binary { op: kind, lhs, rhs } => {
                let f = lhs.typ == ast::Type::Float;

                let lhs = self.lower_expression(*lhs, vars)?;
                let rhs = self.lower_expression(*rhs, vars)?;

                let kind = match kind {
                    ast::BinaryOperation::Add if f => ir::BinaryOperationKind::AddFloat,
                    ast::BinaryOperation::Sub if f => ir::BinaryOperationKind::SubFloat,
                    ast::BinaryOperation::Mul if f => ir::BinaryOperationKind::MulFloat,
                    ast::BinaryOperation::Div if f => ir::BinaryOperationKind::DivFloat,
                    ast::BinaryOperation::Equals if f => ir::BinaryOperationKind::EqualsFloat,
                    ast::BinaryOperation::Less if f => ir::BinaryOperationKind::LessFloat,
                    ast::BinaryOperation::LessEq if f => ir::BinaryOperationKind::LessEqFloat,
                    ast::BinaryOperation::Greater if f => ir::BinaryOperationKind::GreaterFloat,
                    ast::BinaryOperation::GreaterEq if f => ir::BinaryOperationKind::GreaterEqFloat,
                    ast::BinaryOperation::Add => ir::BinaryOperationKind::AddInt,
                    ast::BinaryOperation::Sub => ir::BinaryOperationKind::SubInt,
                    ast::BinaryOperation::Mul => ir::BinaryOperationKind::MulInt,
                    ast::BinaryOperation::Div => ir::BinaryOperationKind::DivInt,
                    ast::BinaryOperation::Equals => ir::BinaryOperationKind::EqualsInt,
                    ast::BinaryOperation::Less => ir::BinaryOperationKind::LessInt,
                    ast::BinaryOperation::LessEq => ir::BinaryOperationKind::LessEqInt,
                    ast::BinaryOperation::Greater => ir::BinaryOperationKind::GreaterInt,
                    ast::BinaryOperation::GreaterEq => ir::BinaryOperationKind::GreaterEqInt,
                    ast::BinaryOperation::And => ir::BinaryOperationKind::And,
                    ast::BinaryOperation::Or => ir::BinaryOperationKind::Or,
                };

                Ok(ir::Expression::BinaryOperation {
                    kind,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                })
            }
            ast::ExpressionKind::LetIn { mut binds, body } => {
                let mut extended_vars = vars.clone();

                if binds.len() <= 1 {
                    let (var, _, bind) = binds.pop().expect("at least 1");
                    let bind = self.lower_expression(bind, vars)?;

                    let fresh = self.fresh_variable();
                    extended_vars.insert(var, fresh);

                    let body = self.lower_expression(*body, &extended_vars)?;

                    Ok(ir::Expression::LocalBinding {
                        var: fresh,
                        bind: Box::new(bind),
                        body: Box::new(body),
                    })
                } else {
                    let (last_var, _, last_expr) = binds.remove(0);
                    let bind = self.lower_expression(last_expr, vars)?;

                    let fresh = self.fresh_variable();
                    extended_vars.insert(last_var, fresh);

                    let rest = ast::Expression {
                        kind: ast::ExpressionKind::LetIn { binds, body },
                        span: expr.span,
                        typ: expr.typ,
                    };
                    let rest = self.lower_expression(rest, &extended_vars)?;

                    Ok(ir::Expression::LocalBinding {
                        var: fresh,
                        bind: Box::new(bind),
                        body: Box::new(rest),
                    })
                }
            }
            ast::ExpressionKind::IfThenElse { condition, yes, no } => {
                let typ = self.lower_type(yes.typ);

                Ok(ir::Expression::Conditional {
                    condition: Box::new(self.lower_expression(*condition, vars)?),
                    yes: Box::new(self.lower_expression(*yes, vars)?),
                    no: Box::new(self.lower_expression(*no, vars)?),
                    typ,
                })
            }
            ast::ExpressionKind::Call { function, args } => {
                // builtin functions
                if let Some(builtin) = builtin::BuiltinFunction::from_name(function) {
                    let function_name = match builtin {
                        builtin::BuiltinFunction::Trace => match args[0].typ {
                            ast::Type::Int => "__compli_trace_int",
                            ast::Type::Float => "__compli_trace_float",
                            ast::Type::Bool => "__compli_trace_bool",
                            ast::Type::Record(_) => unreachable!("ensured by type checker"),
                        },
                        builtin::BuiltinFunction::CastInt => match args[0].typ {
                            ast::Type::Bool => "__compli_bool_to_int",
                            ast::Type::Float => "__compli_float_to_int",
                            _ => unreachable!("ensured by type checker"),
                        },
                        builtin::BuiltinFunction::CastFloat => match args[0].typ {
                            ast::Type::Int => "__compli_int_to_float",
                            _ => unreachable!("ensured by type checker"),
                        },
                        builtin::BuiltinFunction::Sqrt => "__compli_sqrt",
                        builtin::BuiltinFunction::InputInt => "__compli_input_int",
                        builtin::BuiltinFunction::InputFloat => "__compli_input_float",
                    }
                    .into();

                    let mut args = args;
                    let mut lowered_args = Vec::with_capacity(builtin.parameter_number());
                    for _ in 0..builtin.parameter_number() {
                        lowered_args.push(self.lower_expression(args.remove(0), vars)?);
                    }

                    return Ok(ir::Expression::FunctionCall {
                        function_name,
                        args: lowered_args,
                    });
                }

                let mut lowered_args = Vec::with_capacity(args.len());
                for arg in args {
                    lowered_args.push(self.lower_expression(arg, vars)?);
                }

                if let Some(fields) = self.record_definitions.get(&function) {
                    return Ok(ir::Expression::RecordConstructor {
                        record_fields: fields.clone().into_iter().map(|(_, typ)| typ).collect(),
                        args: lowered_args,
                    });
                }

                Ok(ir::Expression::FunctionCall {
                    function_name: function,
                    args: lowered_args,
                })
            }
            ast::ExpressionKind::RecordSelector { expr: inner, field } => {
                let ast::Type::Record(record_name) = &inner.typ else {
                    unreachable!("ensured by type checker")
                };

                let fields = self
                    .record_definitions
                    .get(record_name)
                    .expect("ensured by type checker");

                let index = fields
                    .iter()
                    .enumerate()
                    .find_map(|(i, (field_name, _))| (*field_name == field).then_some(i))
                    .expect("ensured by type checker");

                let record = self.lower_expression(*inner, vars)?;

                Ok(ir::Expression::RecordSelector {
                    record: Box::new(record),
                    index,
                })
            }
        }
    }

    fn lower_type(&self, typ: ast::Type<'src>) -> ir::Type {
        match typ {
            ast::Type::Int => ir::Type::Int,
            ast::Type::Float => ir::Type::Float,
            ast::Type::Bool => ir::Type::Bool,
            ast::Type::Record(name) => ir::Type::Record(
                self.record_definitions
                    .get(&name)
                    .expect("ensured by type checker")
                    .clone()
                    .into_iter()
                    .map(|(_, typ)| typ)
                    .collect(),
            ),
        }
    }

    fn fresh_variable(&mut self) -> Variable {
        let fresh = self.fresh_variable;
        self.fresh_variable.advance();
        fresh
    }
}
