use std::collections::HashMap;

use crate::{
    ast::{BinOperator, Expression, Statement, VarDef, UnOperator},
    DiagEmitter, DiagLevel, Sp, Span,
};

pub struct Interpreter {
    variables: HashMap<String, (Span, f32)>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
        }
    }

    pub fn interpret_stmt(&mut self, emitter: &DiagEmitter, stmt: Sp<Statement>) -> Option<()> {
        let (stmt, span) = stmt.into_parts();
        match stmt {
            Statement::Expr(expr) => {
                let val = self.interpret_expr(emitter, Sp::new(expr, span))?;
                emitter.print_diag(
                    DiagLevel::Info,
                    format!("evalulated expression to be `{val}`"),
                    span,
                );

                Some(())
            }
            Statement::VarDef(var_stmt) => self.interpret_var_def(emitter, var_stmt),
        }
    }

    fn interpret_var_def(&mut self, emitter: &DiagEmitter, var_def: VarDef) -> Option<()> {
        let (var_name, var_span) = var_def.variable.into_parts();
        let var_value = self.variables.get(&var_name).copied();
        let value = self.interpret_expr(emitter, var_def.expr)?;

        match var_value {
            // correct variable definition
            None => {
                self.variables.insert(var_name, (var_span, value));

                Some(())
            }

            // variable already defined
            Some((existing_def, var_val)) => {
                emitter.print_diag(
                    DiagLevel::Error,
                    format!("cannot redefine variable `{var_name}`"),
                    Span::merge(var_def.let_kw, var_span),
                );

                emitter.print_diag(
                    DiagLevel::Info,
                    format!("variable already defined here with value `{var_val}`"),
                    existing_def,
                );

                None
            }
        }
    }

    fn interpret_expr(&self, emitter: &DiagEmitter, expr: Sp<Expression>) -> Option<f32> {
        let (expr, span) = expr.into_parts();
        Some(match expr {
            Expression::Paren { expr, .. } => self.interpret_expr(emitter, expr.unbox())?,
            Expression::BinOp { lhs, rhs, op } => {
                let lhs = self.interpret_expr(emitter, lhs.unbox())?;
                let rhs = self.interpret_expr(emitter, rhs.unbox())?;

                match op.inner() {
                    BinOperator::Add => lhs + rhs,
                    BinOperator::Sub => lhs - rhs,
                    BinOperator::Mul => lhs * rhs,
                    BinOperator::Div => lhs / rhs,
                }
            }
            Expression::UnaryOp { expr, op } => {
                let expr = self.interpret_expr(emitter, expr.unbox())?;

                match op.inner() {
                    UnOperator::Negation => -expr,
                }
            },
            Expression::Literal(x) => x,
            Expression::Variable(var) => self.lookup_variable(emitter, span, &var)?,
            Expression::FunctionCall { name, args, .. } => {
                let args = args
                    .into_iter()
                    .map(|(arg, _)| self.interpret_expr(emitter, arg))
                    .collect::<Option<Vec<_>>>()?;

                let Some(function) = Self::lookup_one_arg_function(name.inner()) else {
                    emitter.print_diag(DiagLevel::Error, format!("unknown built-in function `{}`", name.inner()), name.span());
                    
                    return None;
                };

                let arg = args[0];
                function(arg)
            }
        })
    }

    fn lookup_variable(&self, emitter: &DiagEmitter, span: Span, var: &str) -> Option<f32> {
        self.variables.get(var).map(|(_, val)| val).copied().or_else(|| {
            emitter.print_diag(DiagLevel::Error, format!("unknown variable `{var}`"), span);

            None
        })
    }

    fn lookup_one_arg_function(name: &str) -> Option<fn(f32) -> f32> {
        match name {
            "abs" => Some(f32::abs),
            "trunc" => Some(f32::trunc),
            "to_radians" => Some(f32::to_radians),
            "to_degrees" => Some(f32::to_degrees),
            "tan" => Some(f32::tan),
            "sqrt" => Some(f32::sqrt),
            "sin" => Some(f32::sin),
            "round" => Some(f32::round),
            "log2" => Some(f32::log2),
            "log10" => Some(f32::log10),
            "cos" => Some(f32::cos),
            _ => None,
        }
    }
}
