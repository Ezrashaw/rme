use std::collections::HashMap;

use crate::{
    ast::{BinOperator, Expression, Statement, UnOperator, VarDef},
    diag::{Diag, DiagEmitter},
    Sp, Span,
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

    pub fn interpret_stmt<'em>(
        &'em mut self,
        emitter: &DiagEmitter,
        stmt: Sp<Statement>,
    ) -> Result<(), Diag> {
        let (stmt, span) = stmt.into_parts();
        match stmt {
            Statement::Expr(expr) => {
                let val = self.interpret_expr(emitter, Sp::new(expr, span))?;

                Ok(())
            }
            Statement::VarDef(var_stmt) => self.interpret_var_def(emitter, var_stmt),
        }
    }

    fn interpret_var_def<'em>(
        &'em mut self,
        emitter: &'em DiagEmitter,
        var_def: VarDef,
    ) -> Result<(), Diag> {
        let (var_name, var_span) = var_def.variable.into_parts();
        let var_value = self.variables.get(&var_name).copied();
        let value = self.interpret_expr(emitter, var_def.expr)?;

        match var_value {
            // correct variable definition
            None => {
                self.variables.insert(var_name, (var_span, value));

                Ok(())
            }

            // variable already defined
            Some((existing_def, var_val)) => {
                Err(emitter.create_err(
                    format!("cannot redefine variable `{var_name}`"),
                    Span::merge(var_def.let_kw, var_span),
                ))

                // FIXME: no sub-diags; so we
                // emitter.print_diag(
                //     DiagLevel::Info,
                //     format!("variable already defined here with value `{var_val}`"),
                //     existing_def,
                // );
            }
        }
    }

    fn interpret_expr<'em>(
        &'em self,
        emitter: &'em DiagEmitter,
        expr: Sp<Expression>,
    ) -> Result<f32, Diag> {
        let (expr, span) = expr.into_parts();
        Ok(match expr {
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
            }
            Expression::Literal(x) => x,
            Expression::Variable(var) => self.lookup_variable(emitter, span, &var)?,
            Expression::FunctionCall { name, args, .. } => {
                let evaled_args = args
                    .iter()
                    .map(|(arg, _)| self.interpret_expr(emitter, arg.clone()))
                    .collect::<Result<Vec<_>, Diag>>()?;

                // if name.inner() == "print" {
                //     let [arg] = evaled_args[..] else {
                //         return None;
                //     };

                //     emitter.print_diag(
                //         DiagLevel::Info,
                //         format!("evalulated expression to be `{arg}`"),
                //         args[0].0.span(),
                //     );

                //     return Some(arg);
                // }

                let Some(function) = Self::lookup_one_arg_function(name.inner()) else {
                    return Err(emitter.create_err(format!("unknown built-in function `{}`", name.inner()), name.span()));
                };

                let arg = evaled_args[0];
                function(arg)
            }
        })
    }

    fn lookup_variable<'em>(
        &'em self,
        emitter: &'em DiagEmitter,
        span: Span,
        var: &str,
    ) -> Result<f32, Diag> {
        self.variables
            .get(var)
            .map(|(_, val)| val)
            .copied()
            .ok_or(emitter.create_err(format!("unknown variable `{var}`"), span))
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
            "print" => Some(|x| {
                print!("{x}");
                x
            }),
            _ => None,
        }
    }
}
