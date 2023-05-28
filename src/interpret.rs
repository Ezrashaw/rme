use std::collections::HashMap;

use crate::{
    ast::{Expression, Statement, VarDef},
    DErr, Sp, Span, SubDiag, SubDiagLevel,
};

use self::value::Value;

mod value;

#[derive(Default)]
pub struct Interpreter {
    variables: HashMap<String, (Span, Value)>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
        }
    }

    pub fn interpret_stmt(&mut self, stmt: Sp<Statement>) -> Result<Value, DErr> {
        let (stmt, _) = stmt.into_parts();
        match stmt {
            Statement::Expr(expr) => {
                // let is_print = expr.is_print_expr();
                let val = self.interpret_expr(expr.as_ref())?;
                Ok(val)
            }
            Statement::VarDef(var_stmt) => self
                .interpret_var_def(var_stmt.into_parts().0)
                .map(|_| Value::Unit),
        }
    }

    fn interpret_var_def(&mut self, var_def: VarDef) -> Result<(), DErr> {
        let (var_name, var_span) = var_def.name.into_parts();
        let var_value = self.variables.get(&var_name).cloned();
        let value = self.interpret_expr(var_def.expr.as_ref())?;

        match var_value {
            // correct variable definition
            None => {
                self.variables.insert(var_name, (var_span, value));

                Ok(())
            }

            // variable already defined
            Some((existing_def, var_val)) => {
                let mut err = DErr::new_err(
                    format!("cannot redefine variable `{var_name}`"),
                    Span::merge(var_def.let_kw, var_span),
                );

                err.add_subdiag(SubDiag::new(
                    SubDiagLevel::Info,
                    format!("variable already defined here with value `{var_val}`"),
                    existing_def,
                ));

                Err(err)
            }
        }
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
                let Expression::Variable(ref name) = **(expr.inner()) else {
                    return Err(DErr::new_err("arbitrary function expressions not supported", expr.span()));
                };

                let evaled_args = args
                    .iter()
                    .map(|(arg, _)| self.interpret_expr(arg.as_ref()))
                    .collect::<Result<Vec<_>, DErr>>()?;

                if let [Value::Float(arg)] = evaled_args[..] {
                    if name == "print" {
                        println!("{arg}");
                        return Ok(Value::Unit);
                    }

                    if let Some(function) = Self::lookup_simple_float_function(name) {
                        return Ok(Value::Float(function(arg)));
                    }
                }

                return Err(DErr::new_err(
                    format!("unknown function `{name}`"),
                    expr.span(),
                ));
            }
        })
    }

    fn lookup_variable(&self, span: Span, var: &str) -> Result<Value, DErr> {
        self.variables
            .get(var)
            .map(|(_, val)| val)
            .cloned()
            .ok_or_else(|| DErr::new_err(format!("unknown variable `{var}`"), span))
    }

    fn lookup_simple_float_function(name: &str) -> Option<fn(f32) -> f32> {
        Some(match name {
            "abs" => f32::abs,
            "trunc" => f32::trunc,
            "rad" | "to_radians" => f32::to_radians,
            "deg" | "to_degrees" => f32::to_degrees,
            "tan" => f32::tan,
            "sqrt" => f32::sqrt,
            "sin" => f32::sin,
            "round" => f32::round,
            "log2" => f32::log2,
            "log10" => f32::log10,
            "cos" => f32::cos,
            _ => return None,
        })
    }
}
