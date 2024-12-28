use std::collections::HashMap;

use crate::{ast, Type};

pub fn type_check(program: &ast::Program) -> Option<()> {
    let functions = program
        .functions
        .iter()
        .map(|(f, _)| {
            (
                f.name.clone(),
                (f.params.iter().map(|(_, typ)| *typ).collect(), f.ret_type),
            )
        })
        .collect();

    let checker = TypeChecker { functions };
    for function in &program.functions {
        checker.check_function(&function.0)?;
    }

    Some(())
}

struct TypeChecker {
    functions: HashMap<ast::Ident, (Vec<Type>, Type)>,
}

impl TypeChecker {
    fn check_function(&self, function: &ast::Function) -> Option<()> {
        let vars = function.params.iter().cloned().collect();
        let ret = self.infer_expr(&function.body.0, &vars)?;
        assert_type_equal(ret, function.ret_type)
    }

    fn infer_expr(&self, expr: &ast::Expression, vars: &HashMap<ast::Ident, Type>) -> Option<Type> {
        match expr {
            ast::Expression::Int(_) => Some(Type::Int),
            ast::Expression::Bool(_) => Some(Type::Bool),
            ast::Expression::Var(x) => vars.get(x).copied(),
            ast::Expression::UnaOp { kind, inner } => {
                let arg = self.infer_expr(&inner.0, vars)?;
                match kind {
                    ast::UnaOpKind::Neg => {
                        assert_type_equal(arg, Type::Int)?;
                        Some(Type::Int)
                    }
                    ast::UnaOpKind::Not => {
                        assert_type_equal(arg, Type::Bool)?;
                        Some(Type::Bool)
                    }
                }
            }
            ast::Expression::BinOp { kind, lhs, rhs } => {
                let lhs = self.infer_expr(&lhs.0, vars)?;
                let rhs = self.infer_expr(&rhs.0, vars)?;
                match kind {
                    ast::BinOpKind::Add | ast::BinOpKind::Sub => {
                        assert_type_equal(lhs, Type::Int)?;
                        assert_type_equal(rhs, Type::Int)?;
                        Some(Type::Int)
                    }
                    ast::BinOpKind::Equals | ast::BinOpKind::Less => {
                        assert_type_equal(lhs, Type::Int)?;
                        assert_type_equal(rhs, Type::Int)?;
                        Some(Type::Bool)
                    }
                    ast::BinOpKind::And => {
                        assert_type_equal(lhs, Type::Bool)?;
                        assert_type_equal(rhs, Type::Bool)?;
                        Some(Type::Bool)
                    }
                }
            }
            ast::Expression::LetIn { var, bind, body } => {
                let bind = self.infer_expr(&bind.0, vars)?;

                let mut extended_vars = vars.clone();
                extended_vars.insert(var.clone(), bind);

                self.infer_expr(&body.0, &extended_vars)
            }
            ast::Expression::IfThenElse {
                condition,
                then_branch,
                else_branch,
            } => {
                let condition = self.infer_expr(&condition.0, vars)?;
                assert_type_equal(condition, Type::Bool)?;

                let then_branch = self.infer_expr(&then_branch.0, vars)?;
                let else_branch = self.infer_expr(&else_branch.0, vars)?;
                assert_type_equal(then_branch, else_branch)?;

                Some(then_branch)
            }
            ast::Expression::Call { function, args } => {
                let (params, ret) = self.functions.get(function)?;

                if args.len() != params.len() {
                    return None;
                }

                for (arg, &param) in args.iter().map(|(arg, _)| arg).zip(params.iter()) {
                    let arg = self.infer_expr(arg, vars)?;
                    assert_type_equal(arg, param)?;
                }

                Some(*ret)
            }
        }
    }
}

fn assert_type_equal(typ1: Type, typ2: Type) -> Option<()> {
    Some(typ1 == typ2).filter(|x| *x).map(|_| ())
}
