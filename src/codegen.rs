use std::collections::HashMap;

use miette::Diagnostic;
use thiserror::Error;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicMetadataTypeEnum, IntType};
use inkwell::values::{AnyValue, FunctionValue, IntValue};

use crate::{ir, Type, Variable};

#[derive(Debug, Error, Diagnostic)]
pub enum CodegenError {
    #[error("The LLVM builder failed")]
    BuilderError(#[from] inkwell::builder::BuilderError),

    #[error("The function {0} is unknown and cannot be built")]
    UnknownFunction(String),
}

type Result<T> = std::result::Result<T, CodegenError>;

pub fn compile(context: &Context, program: ir::Program) -> Result<Module> {
    let mut codegen = Codegen::new(context, &program.skeleton(), &program.entry)?;

    for function in &program.functions {
        codegen.compile_function(function)?;
    }

    Ok(codegen.module)
}

struct Codegen<'ctx> {
    context: &'ctx Context,
    builder: Builder<'ctx>,
    module: Module<'ctx>,
    function: FunctionValue<'ctx>,
}

impl<'ctx> Codegen<'ctx> {
    fn new(
        context: &'ctx Context,
        skeleton: &[ir::FunctionPrototype],
        entry_expr: &ir::Expression,
    ) -> Result<Self> {
        let builder = context.create_builder();
        let module = context.create_module("compliModule");

        let entry_fn_type = context.i32_type().fn_type(&[], false);
        let entry_fn = module.add_function("__compli_entry", entry_fn_type, None);

        let codegen = Self {
            context,
            builder,
            module,
            function: entry_fn,
        };

        for prototype in skeleton {
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
            bindings.insert(var, value.as_any_value_enum().into_int_value());
        }

        let entry = self.context.append_basic_block(self.function, "entry");
        self.builder.position_at_end(entry);

        let body = self.compile_expression(&function.body, &bindings)?;
        self.builder.build_return(Some(&body))?;

        assert!(self.function.verify(true));
        Ok(self.function)
    }

    fn compile_type(&self, typ: &Type) -> IntType<'ctx> {
        match typ {
            Type::Int => self.context.i32_type(),
            Type::Bool => self.context.bool_type(),
        }
    }

    fn compile_expression(
        &self,
        expression: &ir::Expression,
        bindings: &HashMap<Variable, IntValue<'ctx>>,
    ) -> Result<IntValue<'ctx>> {
        match expression {
            ir::Expression::Direct(value) => Ok(self.compile_value(value, bindings)),
            ir::Expression::LocalBinding(local) => {
                let bind = self.compile_expression(&local.bind, bindings)?;
                let mut extended_bindings = bindings.clone();
                extended_bindings.insert(local.var, bind);
                self.compile_expression(&local.body, &extended_bindings)
            }
            ir::Expression::BinaryOperation(binop) => {
                let lhs = self.compile_expression(&binop.lhs, bindings)?;
                let rhs = self.compile_expression(&binop.rhs, bindings)?;
                match binop.kind {
                    ir::BinaryOperationKind::Add => {
                        Ok(self.builder.build_int_add(lhs, rhs, "add")?)
                    }
                    ir::BinaryOperationKind::Sub => {
                        Ok(self.builder.build_int_sub(lhs, rhs, "sub")?)
                    }
                    ir::BinaryOperationKind::Equals => Ok(self.builder.build_int_compare(
                        inkwell::IntPredicate::EQ,
                        lhs,
                        rhs,
                        "equ",
                    )?),
                    ir::BinaryOperationKind::Less => Ok(self.builder.build_int_compare(
                        inkwell::IntPredicate::SLT,
                        lhs,
                        rhs,
                        "lt",
                    )?),
                    ir::BinaryOperationKind::And => Ok(self.builder.build_and(lhs, rhs, "and")?),
                }
            }
            ir::Expression::Conditional(c) => {
                let condition = self.compile_expression(&c.condition, bindings)?;

                let then_bb = self.context.append_basic_block(self.function, "then");
                let else_bb = self.context.append_basic_block(self.function, "else");
                let cont_bb = self.context.append_basic_block(self.function, "cont");

                self.builder
                    .build_conditional_branch(condition, then_bb, else_bb)?;

                self.builder.position_at_end(then_bb);
                let then_value = self.compile_expression(&c.then_branch, bindings)?;
                self.builder.build_unconditional_branch(cont_bb)?;

                // NOTE: Important! Update bb for phi merge because the expression may change it
                let updated_then_bb = self.builder.get_insert_block().unwrap();

                self.builder.position_at_end(else_bb);
                let else_value = self.compile_expression(&c.else_branch, bindings)?;
                self.builder.build_unconditional_branch(cont_bb)?;

                // NOTE: Important! Update bb for phi merge because the expression may change it
                let updated_else_bb = self.builder.get_insert_block().unwrap();

                self.builder.position_at_end(cont_bb);
                let phi = self
                    .builder
                    .build_phi(self.context.i32_type(), "cond-phi")?;
                phi.add_incoming(&[
                    (&then_value, updated_then_bb),
                    (&else_value, updated_else_bb),
                ]);

                Ok(phi.as_any_value_enum().into_int_value())
            }
            ir::Expression::FunctionCall { fn_name, args } => {
                match self.module.get_function(fn_name.as_str()) {
                    Some(func) => {
                        let mut compiled_args = Vec::with_capacity(args.len());
                        for arg in args {
                            compiled_args.push(self.compile_expression(arg, bindings)?.into());
                        }

                        let result = self.builder.build_call(func, &compiled_args, "call")?;
                        Ok(result.as_any_value_enum().into_int_value())
                    }
                    None => Err(CodegenError::UnknownFunction(fn_name.clone())),
                }
            }
        }
    }

    fn compile_value(
        &self,
        value: &ir::Value,
        bindings: &HashMap<Variable, IntValue<'ctx>>,
    ) -> IntValue<'ctx> {
        match value {
            ir::Value::Number(n) => self.context.i32_type().const_int(*n as u64, true),
            ir::Value::Boolean(false) => self.context.bool_type().const_int(0, false),
            ir::Value::Boolean(true) => self.context.bool_type().const_int(1, false),
            ir::Value::Variable(v) => *bindings.get(v).expect("Already checked"),
        }
    }
}
