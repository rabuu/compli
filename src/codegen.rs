use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::IntType;
use inkwell::values::{AnyValue, FunctionValue, IntValue};

use crate::ir;

pub struct Codegen<'ctx> {
    context: &'ctx Context,
    builder: Builder<'ctx>,
    module: Module<'ctx>,
    function: FunctionValue<'ctx>,
}

impl<'ctx> Codegen<'ctx> {
    fn basic_type(&self) -> IntType<'ctx> {
        self.context.i32_type()
    }

    pub fn new(context: &'ctx Context) -> Self {
        let builder = context.create_builder();
        let module = context.create_module("myModule");

        let int_type = context.i32_type();
        let fn_type = int_type.fn_type(&[], false);

        let function = module.add_function("myMain", fn_type, None);
        let entry = context.append_basic_block(function, "myEntrypoint");
        builder.position_at_end(entry);

        Self {
            context,
            builder,
            module,
            function,
        }
    }

    pub fn compile(mut self, expression: ir::Expression) -> Module<'ctx> {
        let result = self.compile_expression(&expression);
        self.builder.build_return(Some(&result)).unwrap();
        self.module
    }

    fn compile_expression(&mut self, expression: &ir::Expression) -> IntValue<'ctx> {
        match expression {
            ir::Expression::Direct(value) => self.compile_value(value),
            ir::Expression::Operation { kind, lhs, rhs } => {
                let lhs = self.compile_expression(lhs);
                let rhs = self.compile_expression(rhs);
                match kind {
                    ir::OpKind::Add => self.builder.build_int_add(lhs, rhs, "add").unwrap(),
                    ir::OpKind::And => self.builder.build_and(lhs, rhs, "and").unwrap(),
                    ir::OpKind::Sma => self.builder.build_int_compare(inkwell::IntPredicate::SLT, lhs, rhs, "and").unwrap(),
                }
            }
            ir::Expression::IfThenElse { cond, then_clause, else_clause } => {
                let cond = self.compile_expression(cond);

                let then_bb = self.context.append_basic_block(self.function, "then");
                let else_bb = self.context.append_basic_block(self.function, "else");
                let cont_bb = self.context.append_basic_block(self.function, "cont");

                self.builder.build_conditional_branch(cond, then_bb, else_bb).unwrap();

                self.builder.position_at_end(then_bb);
                let then_value = self.compile_expression(then_clause);
                self.builder.build_unconditional_branch(cont_bb).unwrap();

                self.builder.position_at_end(else_bb);
                let else_value = self.compile_expression(else_clause);
                self.builder.build_unconditional_branch(cont_bb).unwrap();

                self.builder.position_at_end(cont_bb);
                let phi = self.builder.build_phi(self.basic_type(), "iftmp").unwrap();
                phi.add_incoming(&[
                    (&then_value, then_bb),
                    (&else_value, else_bb),
                ]);

                phi.as_any_value_enum().into_int_value()
            }
        }
    }

    fn compile_value(&self, value: &ir::Value) -> IntValue<'ctx> {
        match value {
            ir::Value::Number(n) => self.basic_type().const_int(*n as u64, true),
            ir::Value::Boolean(false) => self.context.bool_type().const_int(0, false),
            ir::Value::Boolean(true) => self.context.bool_type().const_int(1, false),
        }
    }
}
