//! Code generation
//!
//! This module is responsible for generating LLVM IR from compli's own intermediate
//! representation ([ir]). Its main interface is the [compile] function.
//!
//! This module heavily relies on the [inkwell] crate.

use std::collections::HashMap;

use miette::Diagnostic;
use thiserror::Error;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, StructType};
use inkwell::values::{AggregateValue, BasicValue, BasicValueEnum, FunctionValue};

use crate::{builtin, ir, Variable};

#[derive(Debug, Error, Diagnostic)]
pub enum CodegenError {
    #[error("The LLVM builder failed")]
    BuilderError(#[from] inkwell::builder::BuilderError),

    #[error("The function {0} is unknown and cannot be built")]
    UnknownFunction(String),
}

type Result<T> = std::result::Result<T, CodegenError>;

/// Compile an [inkwell]/LLVM module from a compli program
pub fn compile(context: &Context, program: ir::Program) -> Result<Module> {
    let mut codegen = Codegen::new(context, &program.skeleton(), &program.entry)?;

    for function in &program.functions {
        codegen.compile_function(function)?;
    }

    Ok(codegen.module)
}

/// The main state of the code generation phase
struct Codegen<'ctx> {
    context: &'ctx Context,
    builder: Builder<'ctx>,
    module: Module<'ctx>,
    function: FunctionValue<'ctx>,
}

impl<'ctx> Codegen<'ctx> {
    /// Initialize [inkwell]'s codegen machinery, compile all prototypes and the entry function
    fn new(
        context: &'ctx Context,
        skeleton: &[ir::FunctionPrototype],
        entry_expr: &ir::Expression,
    ) -> Result<Self> {
        let builder = context.create_builder();
        let module = context.create_module("compliModule");

        // The "entry function" will get called from the runtimes main function
        // It has the type "() -> int" and has the name "__compli_entry".
        let entry_fn_type = context.i32_type().fn_type(&[], false);
        let entry_fn = module.add_function("__compli_entry", entry_fn_type, None);

        let codegen = Self {
            context,
            builder,
            module,
            function: entry_fn,
        };

        let builtins = builtin::all_builtins();
        for prototype in skeleton.iter().chain(builtins.iter()) {
            codegen.compile_prototype(prototype)?;
        }

        let entry_bb = codegen.context.append_basic_block(entry_fn, "entry");
        codegen.builder.position_at_end(entry_bb);

        let body = codegen.compile_expression(entry_expr, &HashMap::new())?;
        codegen.builder.build_return(Some(&body))?;

        assert!(entry_fn.verify(true));

        Ok(codegen)
    }

    fn compile_prototype(&self, prototype: &ir::FunctionPrototype) -> Result<FunctionValue<'ctx>> {
        let ret_type = self.compile_type(&prototype.return_type);
        let param_types: Vec<BasicMetadataTypeEnum> = prototype
            .parameters
            .iter()
            .map(|(_, typ)| self.compile_type(typ).into())
            .collect();

        let fn_type = ret_type.fn_type(&param_types, false);
        Ok(self.module.add_function(&prototype.name, fn_type, None))
    }

    fn compile_function(
        &mut self,
        function: &ir::FunctionDefinition,
    ) -> Result<FunctionValue<'ctx>> {
        self.function = self
            .module
            .get_function(&function.prototype.name)
            .expect("Prototype missing");

        let mut bindings = HashMap::new();
        for (var, value) in function
            .prototype
            .parameters
            .iter()
            .map(|(var, _)| *var)
            .zip(self.function.get_param_iter())
        {
            bindings.insert(var, value.as_basic_value_enum());
        }

        let entry = self.context.append_basic_block(self.function, "entry");
        self.builder.position_at_end(entry);

        let body = self.compile_expression(&function.body, &bindings)?;
        self.builder.build_return(Some(&body))?;

        assert!(self.function.verify(true));
        Ok(self.function)
    }

    fn compile_type(&self, typ: &ir::Type) -> BasicTypeEnum<'ctx> {
        match typ {
            ir::Type::Int => self.context.i32_type().as_basic_type_enum(),
            ir::Type::Float => self.context.f32_type().as_basic_type_enum(),
            ir::Type::Bool => self.context.bool_type().as_basic_type_enum(),
            ir::Type::Record(fields) => self.compile_record(fields).as_basic_type_enum(),
        }
    }

    fn compile_record(&self, fields: &[ir::Type]) -> StructType<'ctx> {
        let fields: Vec<_> = fields.iter().map(|typ| self.compile_type(typ)).collect();
        self.context.struct_type(&fields, false)
    }

    fn compile_expression(
        &self,
        expression: &ir::Expression,
        bindings: &HashMap<Variable, BasicValueEnum<'ctx>>,
    ) -> Result<BasicValueEnum<'ctx>> {
        match expression {
            ir::Expression::Direct(value) => Ok(self.compile_value(value, bindings)),
            ir::Expression::LocalBinding(local) => {
                let bind = self.compile_expression(&local.bind, bindings)?;
                let mut extended_bindings = bindings.clone();
                extended_bindings.insert(local.var, bind);
                self.compile_expression(&local.body, &extended_bindings)
            }
            ir::Expression::UnaryOperation(unaop) => {
                let arg = self.compile_expression(&unaop.inner, bindings)?;
                match unaop.kind {
                    ir::UnaryOperationKind::NegInt => Ok(self
                        .builder
                        .build_int_neg(arg.into_int_value(), "neg")?
                        .as_basic_value_enum()),
                    ir::UnaryOperationKind::NegFloat => Ok(self
                        .builder
                        .build_float_neg(arg.into_float_value(), "neg")?
                        .as_basic_value_enum()),
                    ir::UnaryOperationKind::Not => Ok(self
                        .builder
                        .build_not(arg.into_int_value(), "not")?
                        .as_basic_value_enum()),
                }
            }
            ir::Expression::BinaryOperation(binop) => {
                let lhs = self.compile_expression(&binop.lhs, bindings)?;
                let rhs = self.compile_expression(&binop.rhs, bindings)?;
                match binop.kind {
                    ir::BinaryOperationKind::AddInt => Ok(self
                        .builder
                        .build_int_add(lhs.into_int_value(), rhs.into_int_value(), "add")?
                        .as_basic_value_enum()),
                    ir::BinaryOperationKind::SubInt => Ok(self
                        .builder
                        .build_int_sub(lhs.into_int_value(), rhs.into_int_value(), "sub")?
                        .as_basic_value_enum()),
                    ir::BinaryOperationKind::MulInt => Ok(self
                        .builder
                        .build_int_mul(lhs.into_int_value(), rhs.into_int_value(), "mul")?
                        .as_basic_value_enum()),
                    ir::BinaryOperationKind::DivInt => Ok(self
                        .builder
                        .build_int_signed_div(lhs.into_int_value(), rhs.into_int_value(), "div")?
                        .as_basic_value_enum()),
                    ir::BinaryOperationKind::EqualsInt => Ok(self
                        .builder
                        .build_int_compare(
                            inkwell::IntPredicate::EQ,
                            lhs.into_int_value(),
                            rhs.into_int_value(),
                            "equ",
                        )?
                        .as_basic_value_enum()),
                    ir::BinaryOperationKind::LessInt => Ok(self
                        .builder
                        .build_int_compare(
                            inkwell::IntPredicate::SLT,
                            lhs.into_int_value(),
                            rhs.into_int_value(),
                            "lt",
                        )?
                        .as_basic_value_enum()),
                    ir::BinaryOperationKind::LessEqInt => Ok(self
                        .builder
                        .build_int_compare(
                            inkwell::IntPredicate::SLE,
                            lhs.into_int_value(),
                            rhs.into_int_value(),
                            "le",
                        )?
                        .as_basic_value_enum()),
                    ir::BinaryOperationKind::GreaterInt => Ok(self
                        .builder
                        .build_int_compare(
                            inkwell::IntPredicate::SGT,
                            lhs.into_int_value(),
                            rhs.into_int_value(),
                            "gt",
                        )?
                        .as_basic_value_enum()),
                    ir::BinaryOperationKind::GreaterEqInt => Ok(self
                        .builder
                        .build_int_compare(
                            inkwell::IntPredicate::SGE,
                            lhs.into_int_value(),
                            rhs.into_int_value(),
                            "ge",
                        )?
                        .as_basic_value_enum()),
                    ir::BinaryOperationKind::AddFloat => Ok(self
                        .builder
                        .build_float_add(lhs.into_float_value(), rhs.into_float_value(), "fadd")?
                        .as_basic_value_enum()),
                    ir::BinaryOperationKind::SubFloat => Ok(self
                        .builder
                        .build_float_sub(lhs.into_float_value(), rhs.into_float_value(), "fsub")?
                        .as_basic_value_enum()),
                    ir::BinaryOperationKind::MulFloat => Ok(self
                        .builder
                        .build_float_mul(lhs.into_float_value(), rhs.into_float_value(), "fmul")?
                        .as_basic_value_enum()),
                    ir::BinaryOperationKind::DivFloat => Ok(self
                        .builder
                        .build_float_div(lhs.into_float_value(), rhs.into_float_value(), "fdiv")?
                        .as_basic_value_enum()),
                    ir::BinaryOperationKind::EqualsFloat => Ok(self
                        .builder
                        .build_float_compare(
                            inkwell::FloatPredicate::OEQ,
                            lhs.into_float_value(),
                            rhs.into_float_value(),
                            "fequ",
                        )?
                        .as_basic_value_enum()),
                    ir::BinaryOperationKind::LessFloat => Ok(self
                        .builder
                        .build_float_compare(
                            inkwell::FloatPredicate::OLT,
                            lhs.into_float_value(),
                            rhs.into_float_value(),
                            "flt",
                        )?
                        .as_basic_value_enum()),
                    ir::BinaryOperationKind::LessEqFloat => Ok(self
                        .builder
                        .build_float_compare(
                            inkwell::FloatPredicate::OLE,
                            lhs.into_float_value(),
                            rhs.into_float_value(),
                            "fle",
                        )?
                        .as_basic_value_enum()),
                    ir::BinaryOperationKind::GreaterFloat => Ok(self
                        .builder
                        .build_float_compare(
                            inkwell::FloatPredicate::OGT,
                            lhs.into_float_value(),
                            rhs.into_float_value(),
                            "fgt",
                        )?
                        .as_basic_value_enum()),
                    ir::BinaryOperationKind::GreaterEqFloat => Ok(self
                        .builder
                        .build_float_compare(
                            inkwell::FloatPredicate::OGE,
                            lhs.into_float_value(),
                            rhs.into_float_value(),
                            "fge",
                        )?
                        .as_basic_value_enum()),
                    ir::BinaryOperationKind::And => Ok(self
                        .builder
                        .build_and(lhs.into_int_value(), rhs.into_int_value(), "and")?
                        .as_basic_value_enum()),
                    ir::BinaryOperationKind::Or => Ok(self
                        .builder
                        .build_or(lhs.into_int_value(), rhs.into_int_value(), "or")?
                        .as_basic_value_enum()),
                }
            }
            ir::Expression::Conditional(c) => {
                let condition = self.compile_expression(&c.condition, bindings)?;

                let then_bb = self.context.append_basic_block(self.function, "then");
                let else_bb = self.context.append_basic_block(self.function, "else");
                let cont_bb = self.context.append_basic_block(self.function, "cont");

                self.builder.build_conditional_branch(
                    condition.into_int_value(),
                    then_bb,
                    else_bb,
                )?;

                self.builder.position_at_end(then_bb);
                let then_value = self.compile_expression(&c.yes, bindings)?;
                self.builder.build_unconditional_branch(cont_bb)?;

                // NOTE: Important! Update bb for phi merge because the expression may change it
                let updated_then_bb = self.builder.get_insert_block().unwrap();

                self.builder.position_at_end(else_bb);
                let else_value = self.compile_expression(&c.no, bindings)?;
                self.builder.build_unconditional_branch(cont_bb)?;

                // NOTE: Important! Update bb for phi merge because the expression may change it
                let updated_else_bb = self.builder.get_insert_block().unwrap();

                self.builder.position_at_end(cont_bb);

                let phi_type = if c.float_mode {
                    self.context.f32_type().as_basic_type_enum()
                } else {
                    self.context.i32_type().as_basic_type_enum()
                };

                let phi = self.builder.build_phi(phi_type, "cond-phi")?;
                phi.add_incoming(&[
                    (&then_value, updated_then_bb),
                    (&else_value, updated_else_bb),
                ]);

                Ok(phi.as_basic_value())
            }
            ir::Expression::FunctionCall {
                function_name,
                args,
            } => match self.module.get_function(function_name.as_str()) {
                Some(func) => {
                    let mut compiled_args = Vec::with_capacity(args.len());
                    for arg in args {
                        compiled_args.push(self.compile_expression(arg, bindings)?.into());
                    }

                    let result = self.builder.build_call(func, &compiled_args, "call")?;
                    Ok(result.try_as_basic_value().unwrap_left())
                }
                None => Err(CodegenError::UnknownFunction(function_name.clone())),
            },
            ir::Expression::RecordConstructor {
                record_fields,
                args,
            } => {
                let mut compiled_args = Vec::with_capacity(args.len());
                for arg in args {
                    compiled_args.push(self.compile_expression(arg, bindings)?);
                }

                let struct_type = self.compile_record(record_fields);
                let mut struct_value = struct_type.const_zero().as_aggregate_value_enum();

                for (i, arg) in compiled_args.into_iter().enumerate() {
                    struct_value = self.builder.build_insert_value(
                        struct_value,
                        arg,
                        i as u32,
                        "construct",
                    )?;
                }

                Ok(struct_value.as_basic_value_enum())
            }
        }
    }

    fn compile_value(
        &self,
        value: &ir::Value,
        bindings: &HashMap<Variable, BasicValueEnum<'ctx>>,
    ) -> BasicValueEnum<'ctx> {
        match value {
            ir::Value::Integer(n) => self
                .context
                .i32_type()
                .const_int(*n as u64, true)
                .as_basic_value_enum(),
            ir::Value::Float(f) => self
                .context
                .f32_type()
                .const_float(*f as f64)
                .as_basic_value_enum(),
            ir::Value::Boolean(false) => self
                .context
                .bool_type()
                .const_int(0, false)
                .as_basic_value_enum(),
            ir::Value::Boolean(true) => self
                .context
                .bool_type()
                .const_int(1, false)
                .as_basic_value_enum(),
            ir::Value::Variable(v) => bindings
                .get(v)
                .expect("Already checked")
                .as_basic_value_enum(),
        }
    }
}
