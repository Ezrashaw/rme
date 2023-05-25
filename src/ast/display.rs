use std::fmt::{self, Display};

use super::{BinOperator, Expression, Statement, UnOperator, VarDef};

impl Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Expr(expr) => expr.fmt(f),
            Self::VarDef(def) => def.fmt(f),
        }
    }
}

impl Display for VarDef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "let {} = ", self.name)?;
        self.expr.fmt(f)
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            display_expr_prec(self, false, f)
        } else {
            display_expr(self, f)
        }
    }
}

fn display_expr(expr: &Expression, f: &mut fmt::Formatter) -> fmt::Result {
    match expr {
        Expression::Paren { expr, .. } => write!(f, "({expr})"),
        Expression::BinOp { lhs, rhs, op } => write!(f, "{lhs} {} {rhs}", *op),
        Expression::UnaryOp { expr, op } => match op.inner() {
            UnOperator::Negation => write!(f, "{}{}", op, expr),
            UnOperator::Factorial => write!(f, "{}{}", expr, op),
        },
        Expression::Literal(val) => write!(f, "{val}"),
        Expression::Variable(var) => write!(f, "{var}"),
        Expression::FunctionCall { name, args, .. } => {
            write!(f, "{name}(")?;
            for (idx, arg) in args.iter().enumerate() {
                write!(f, "{}", arg.0)?;
                if idx < args.len() - 1 {
                    write!(f, ", ")?;
                }
            }

            write!(f, ")")
        }
    }
}

fn display_expr_prec(expr: &Expression, no_show: bool, f: &mut fmt::Formatter) -> fmt::Result {
    let show_parens =
        matches!(expr, Expression::UnaryOp { .. } | Expression::BinOp { .. }) && !no_show;
    if show_parens {
        write!(f, "(")?;
    }

    match expr {
        Expression::Paren { expr: inner, .. } => {
            write!(f, "(")?;
            display_expr_prec(inner, true, f)?;
            write!(f, ")")
        }
        Expression::BinOp { lhs, rhs, op } => write!(f, "{lhs:#} {} {rhs:#}", *op),
        Expression::UnaryOp { expr, op } => match op.inner() {
            UnOperator::Negation => write!(f, "{}{:#}", op, expr),
            UnOperator::Factorial => write!(f, "{:#}{}", expr, op),
        },
        Expression::Literal(val) => write!(f, "{val}"),
        Expression::Variable(var) => write!(f, "{var}"),
        Expression::FunctionCall { name, args, .. } => {
            write!(f, "{name}(")?;
            for (idx, arg) in args.iter().enumerate() {
                write!(f, "{:#}", arg.0)?;
                if idx < args.len() - 1 {
                    write!(f, ", ")?;
                }
            }

            write!(f, ")")
        }
    }?;

    if show_parens {
        write!(f, ")")
    } else {
        Ok(())
    }
}

impl Display for BinOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Add => write!(f, "+"),
            Self::Sub => write!(f, "-"),
            Self::Mul => write!(f, "*"),
            Self::Div => write!(f, "/"),
        }
    }
}

impl Display for UnOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Negation => write!(f, "-"),
            Self::Factorial => write!(f, "!"),
        }
    }
}
