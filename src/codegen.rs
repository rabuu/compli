use std::collections::HashMap;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicMetadataTypeEnum, IntType};
use inkwell::values::{AnyValue, BasicMetadataValueEnum, FunctionValue, IntValue};

use crate::Variable;
use crate::{ir, Type};

pub fn compile(context: &Context, program: ir::Program) -> Module {
    let mut codegen = Codegen::new(context, &program.skeleton());

    let entry_fn_proto = ir::FunctionPrototype {
        name: String::from("__compli_entry"),
        parameters: vec![],
        return_type: Type::Int,
    };

    codegen.compile_prototype(&entry_fn_proto);

    let entry_fn = ir::FunctionDefinition {
        prototype: entry_fn_proto,
        body: program.entry,
    };

    codegen.compile_function(&entry_fn);

    for function in &program.functions {
        codegen.compile_function(function);
    }

    codegen.module
}

struct Codegen<'ctx> {
    context: &'ctx Context,
    builder: Builder<'ctx>,
    module: Module<'ctx>,
    function: Option<FunctionValue<'ctx>>,
}

impl<'ctx> Codegen<'ctx> {
    fn new(context: &'ctx Context, skeleton: &[ir::FunctionPrototype]) -> Self {
        let builder = context.create_builder();
        let module = context.create_module("compliModule");

        let codegen = Self {
            context,
            builder,
            module,
            function: None,
        };

        for prototype in skeleton {
            codegen.compile_prototype(prototype);
        }

        codegen
    }

    fn function(&self) -> FunctionValue<'ctx> {
        self.function.expect("No function set")
    }

    fn int_type(&self) -> IntType<'ctx> {
        self.context.i32_type()
    }

    fn compile_prototype(&self, prototype: &ir::FunctionPrototype) {
        let ret_type = self.compile_type(&prototype.return_type);
        let param_types: Vec<BasicMetadataTypeEnum> = prototype
            .parameters
            .iter()
            .map(|(_, typ)| self.compile_type(typ).into())
            .collect();

        let fn_type = ret_type.fn_type(&param_types, false);
        self.module.add_function(&prototype.name, fn_type, None);
    }

    fn compile_function(&mut self, function: &ir::FunctionDefinition) -> FunctionValue<'ctx> {
        let func = self
            .module
            .get_function(&function.prototype.name)
            .expect("Prototype missing");
        self.function = Some(func);

        let mut bindings = HashMap::new();
        for (var, value) in function
            .prototype
            .parameters
            .iter()
            .map(|(var, _)| *var)
            .zip(self.function().get_param_iter())
        {
            bindings.insert(var, value.as_any_value_enum().into_int_value());
        }

        let entry = self.context.append_basic_block(self.function(), "entry");
        self.builder.position_at_end(entry);

        let body = self.compile_expression(&function.body, &bindings);
        self.builder.build_return(Some(&body)).unwrap();

        assert!(self.function().verify(true));
        self.function()
    }

    fn compile_type(&self, typ: &Type) -> IntType<'ctx> {
        match typ {
            Type::Int => self.int_type(),
            Type::Bool => self.context.bool_type(),
        }
    }

    fn compile_expression(
        &mut self,
        expression: &ir::Expression,
        bindings: &HashMap<Variable, IntValue<'ctx>>,
    ) -> IntValue<'ctx> {
        match expression {
            ir::Expression::Direct(value) => self.compile_value(value, bindings),
            ir::Expression::LocalBinding(local) => {
                let bind = self.compile_expression(&local.bind, bindings);
                let mut extended_bindings = bindings.clone();
                extended_bindings.insert(local.var, bind);
                self.compile_expression(&local.body, &extended_bindings)
            }
            ir::Expression::BinaryOperation(binop) => {
                let lhs = self.compile_expression(&binop.lhs, bindings);
                let rhs = self.compile_expression(&binop.rhs, bindings);
                match binop.kind {
                    ir::BinaryOperationKind::Add => {
                        self.builder.build_int_add(lhs, rhs, "add").unwrap()
                    }
                    ir::BinaryOperationKind::Cmp => self
                        .builder
                        .build_int_compare(inkwell::IntPredicate::SLT, lhs, rhs, "and")
                        .unwrap(),
                    ir::BinaryOperationKind::And => {
                        self.builder.build_and(lhs, rhs, "and").unwrap()
                    }
                }
            }
            ir::Expression::Conditional(c) => {
                let condition = self.compile_expression(&c.condition, bindings);

                let then_bb = self.context.append_basic_block(self.function(), "then");
                let else_bb = self.context.append_basic_block(self.function(), "else");
                let cont_bb = self.context.append_basic_block(self.function(), "cont");

                self.builder
                    .build_conditional_branch(condition, then_bb, else_bb)
                    .unwrap();

                self.builder.position_at_end(then_bb);
                let then_value = self.compile_expression(&c.then_branch, bindings);
                self.builder.build_unconditional_branch(cont_bb).unwrap();

                self.builder.position_at_end(else_bb);
                let else_value = self.compile_expression(&c.else_branch, bindings);
                self.builder.build_unconditional_branch(cont_bb).unwrap();

                self.builder.position_at_end(cont_bb);
                let phi = self.builder.build_phi(self.int_type(), "cond-phi").unwrap();
                phi.add_incoming(&[(&then_value, then_bb), (&else_value, else_bb)]);

                phi.as_any_value_enum().into_int_value()
            }
            ir::Expression::FunctionCall { fn_name, args } => {
                match self.module.get_function(fn_name.as_str()) {
                    Some(func) => {
                        let compiled_args: Vec<BasicMetadataValueEnum> = args
                            .iter()
                            .map(|arg| self.compile_expression(arg, bindings).into())
                            .collect();

                        let result = self.builder.build_call(func, &compiled_args, "call");
                        result.unwrap().as_any_value_enum().into_int_value()
                    }
                    None => panic!("No known function: `{}`", fn_name.as_str()),
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
            ir::Value::Number(n) => self.int_type().const_int(*n as u64, true),
            ir::Value::Boolean(false) => self.context.bool_type().const_int(0, false),
            ir::Value::Boolean(true) => self.context.bool_type().const_int(1, false),
            ir::Value::Variable(v) => *bindings.get(v).unwrap(),
        }
    }
}
