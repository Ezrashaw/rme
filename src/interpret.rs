use std::collections::HashMap;

use crate::{
    ast::{BinOperator, Expression, Statement, UnOperator, VarDef},
    DErr, Sp, Span, SubDiag, SubDiagLevel,
};

#[derive(Default)]
pub struct Interpreter {
    variables: HashMap<String, (Span, f32)>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
        }
    }

    pub fn interpret_stmt(&mut self, stmt: Sp<Statement>) -> Result<Option<SubDiag>, DErr> {
        let (stmt, span) = stmt.into_parts();
        match stmt {
            Statement::Expr(expr) => {
                let is_print = expr.is_print_expr();
                let val = self.interpret_expr(Sp::new(expr, span))?;

                if is_print {
                    Ok(None)
                } else {
                    let mut info_diag = SubDiag::new(
                        SubDiagLevel::Info,
                        format!("evalulated standalone expression to be `{val}`"),
                        span,
                    );

                    info_diag.add_subdiag(SubDiag::without_span(
                        SubDiagLevel::Help,
                        "consider using the `print` function",
                    ));

                    Ok(Some(info_diag))
                }
            }
            Statement::VarDef(var_stmt) => self.interpret_var_def(var_stmt).map(|_| None),
        }
    }

    fn interpret_var_def(&mut self, var_def: VarDef) -> Result<(), DErr> {
        let (var_name, var_span) = var_def.name.into_parts();
        let var_value = self.variables.get(&var_name).copied();
        let value = self.interpret_expr(var_def.expr)?;

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

    fn interpret_expr(&self, expr: Sp<Expression>) -> Result<f32, DErr> {
        let (expr, span) = expr.into_parts();
        Ok(match expr {
            Expression::Paren { expr, .. } => self.interpret_expr(expr.unbox())?,
            Expression::BinOp { lhs, rhs, op } => {
                let lhs = self.interpret_expr(lhs.unbox())?;
                let rhs = self.interpret_expr(rhs.unbox())?;

                match op.inner() {
                    BinOperator::Add => lhs + rhs,
                    BinOperator::Sub => lhs - rhs,
                    BinOperator::Mul => lhs * rhs,
                    BinOperator::Div => lhs / rhs,
                }
            }
            Expression::UnaryOp { expr, op } => {
                let expr = self.interpret_expr(expr.unbox())?;

                match op.inner() {
                    UnOperator::Negation => -expr,
                }
            }
            Expression::Literal(x) => x,
            Expression::Variable(var) => self.lookup_variable(span, &var)?,
            Expression::FunctionCall { name, args, .. } => {
                let evaled_args = args
                    .iter()
                    .map(|(arg, _)| self.interpret_expr(arg.clone()))
                    .collect::<Result<Vec<_>, DErr>>()?;

                let Some(function) = self.lookup_one_arg_function(&name) else {
                    return Err(DErr::new_err(format!("unknown built-in function `{name}`"), name.span()));
                };

                let arg = evaled_args[0];
                function(arg)
            }
        })
    }

    fn lookup_variable(&self, span: Span, var: &str) -> Result<f32, DErr> {
        self.variables
            .get(var)
            .map(|(_, val)| val)
            .copied()
            .ok_or_else(|| DErr::new_err(format!("unknown variable `{var}`"), span))
    }

    fn lookup_one_arg_function(&self, name: &str) -> Option<fn(f32) -> f32> {
        Some(match name {
            "abs" => f32::abs,
            "trunc" => f32::trunc,
            "to_radians" => f32::to_radians,
            "to_degrees" => f32::to_degrees,
            "tan" => f32::tan,
            "sqrt" => f32::sqrt,
            "sin" => f32::sin,
            "round" => f32::round,
            "log2" => f32::log2,
            "log10" => f32::log10,
            "cos" => f32::cos,
            "print" => |x| {
                println!("{x}");

                // FIXME: with types, this returns (for example) the unit type
                0.
            },
            _ => return None,
        })
    }
}
