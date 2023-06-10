use std::collections::HashMap;

use crate::{
    ast::{Expression, FnDef, Statement, VarDef},
    DErr, Sp, Span,
};

use self::value::Value;

mod value;

#[derive(Default)]
pub struct Interpreter {
    variables: HashMap<String, Sp<Value>>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
        }
    }

    pub fn interpret_stmt(&mut self, stmt: Sp<Statement>) -> Result<(), DErr> {
        let (stmt, _) = stmt.into_parts();
        match stmt {
            Statement::Expr(expr) => {
                let val = self.interpret_expr(expr.as_ref())?;
                self.define_variable("it".to_owned(), Sp::new(val, expr.span()));
            }
            Statement::VarDef(var_stmt) => self.interpret_var_def(var_stmt.into_parts().0)?,
            Statement::FnDef(fn_def) => self.interpret_fn_def(fn_def)?,
        };

        Ok(())
    }

    fn interpret_var_def(&mut self, var_def: VarDef) -> Result<(), DErr> {
        let value = self.interpret_expr(var_def.expr.as_ref())?;

        self.define_variable(
            var_def.name.into_parts().0,
            Sp::new(value, var_def.expr.span()),
        );

        Ok(())
    }

    fn interpret_fn_def(&mut self, fn_def: Sp<FnDef>) -> Result<(), DErr> {
        let (fn_def, span) = fn_def.into_parts();
        self.define_variable(
            fn_def.name.into_parts().0,
            Sp::new(
                Value::Function(
                    fn_def.args.into_iter().map(|(expr, _)| expr).collect(),
                    fn_def.expr,
                ),
                span,
            ),
        );

        Ok(())
    }

    fn interpret_expr(&self, expr: Sp<&Expression>) -> Result<Value, DErr> {
        let (expr, span) = expr.as_parts();
        Ok(match expr {
            Expression::Paren { expr, .. } => self.interpret_expr(expr.unbox())?,
            Expression::BinaryOp { lhs, rhs, op } => {
                let lhv = self.interpret_expr(lhs.unbox())?;
                let rhv = self.interpret_expr(rhs.unbox())?;

                Value::eval_binop(Sp::new(lhv, lhs.span()), Sp::new(rhv, rhs.span()), *op)?
            }
            Expression::UnaryOp { expr, op } => {
                let expr_val = self.interpret_expr(expr.unbox())?;

                Value::eval_unop(Sp::new(expr_val, expr.span()), *op)?
            }
            Expression::Literal(lit) => (*lit).into(),
            Expression::Variable(var) => self.lookup_variable(span, var)?,
            Expression::FunctionCall { expr, args, .. } => {
                let call_expr = self.interpret_expr(expr.unbox())?;

                let _ = args
                    .iter()
                    .map(|(arg, _)| self.interpret_expr(arg.as_ref()))
                    .collect::<Result<Vec<_>, DErr>>()?;

                return Err(DErr::new_err(
                    format!("could not call function"),
                    expr.span(),
                ));
            }
        })
    }

    fn lookup_variable(&self, span: Span, var: &str) -> Result<Value, DErr> {
        self.variables
            .get(var)
            .map(|val| val.inner())
            .cloned()
            .ok_or_else(|| DErr::new_err(format!("unknown variable `{var}`"), span))
    }

    fn define_variable(&mut self, name: String, val: Sp<Value>) {
        println!("let {name}: {} = {val}", val.inner().type_of());
        self.variables.insert(name, val);
    }
}
