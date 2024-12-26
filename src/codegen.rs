use std::collections::HashMap;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicMetadataTypeEnum, IntType};
use inkwell::values::{AnyValue, FunctionValue, IntValue};

use crate::ir;
use crate::Variable;

pub fn compile<'a>(context: &'a Context, program: &ir::Program) -> Module<'a> {
    let mut codegen = Codegen::new(context);

    let bindings = HashMap::new();
    codegen.compile_function("main", &program.main_function, &bindings);
    for (name, function) in &program.functions {
        codegen.compile_function(name, function, &bindings);
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
    fn new(context: &'ctx Context) -> Self {
        let builder = context.create_builder();
        let module = context.create_module("compliModule");

        Self {
            context,
            builder,
            module,
            function: None,
        }
    }

    fn int_type(&self) -> IntType<'ctx> {
        self.context.i32_type()
    }

    fn function(&self) -> FunctionValue<'ctx> {
        self.function.unwrap()
    }

    fn compile_function(
        &mut self,
        name: &str,
        function: &ir::FunctionDefinition,
        bindings: &HashMap<Variable, IntValue<'ctx>>,
    ) -> FunctionValue<'ctx> {
        let fn_val = self.compile_prototype(name, &function.prototype);
        self.function = Some(fn_val);

        let entry = self.context.append_basic_block(fn_val, "entry");
        self.builder.position_at_end(entry);

        let body = self.compile_expression(&function.body, bindings);
        self.builder.build_return(Some(&body)).unwrap();

        assert!(fn_val.verify(true));
        fn_val
    }

    fn compile_prototype(&self, name: &str, prototype: &ir::FunctionPrototype) -> FunctionValue<'ctx> {
        let ret_type = self.compile_type(&prototype.return_type);
        let param_types: Vec<BasicMetadataTypeEnum> = prototype
            .parameters
            .iter()
            .map(|(_, typ)| self.compile_type(typ).into())
            .collect();

        let fn_type = ret_type.fn_type(&param_types, false);
        let fn_val = self.module.add_function(name, fn_type, None);

        fn_val
    }

    fn compile_type(&self, typ: &ir::Type) -> IntType<'ctx> {
        match typ {
            ir::Type::Int => self.int_type(),
            ir::Type::Bool => self.context.bool_type(),
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
