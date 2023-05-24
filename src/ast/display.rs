use std::fmt::{self, Display};

use crate::Sp;

use super::{BinOperator, Expression, Statement, UnOperator, VarDef};

impl<T: Display> Display for Sp<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        (**self).fmt(f)
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Expr(expr) => write!(f, "{expr}"),
            Self::VarDef(def) => write!(f, "{def}"),
        }
    }
}

impl Display for VarDef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "let {} = {}", self.name, self.expr)
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Paren { expr, .. } => write!(f, "({expr})"),
            Self::BinOp { lhs, rhs, op } => {
                write!(f, "{lhs} {} {rhs}", *op)
            }
            Self::UnaryOp { expr, op } => {
                write!(f, "{}{expr}", *op)
            }
            Self::Literal(val) => write!(f, "{val}"),
            Self::Variable(var) => write!(f, "{var}"),
            Self::FunctionCall { name, args, .. } => {
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
        }
    }
}
