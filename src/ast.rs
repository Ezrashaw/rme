use std::fmt::{self, Display};

use crate::{Sp, SpBox, Span};

#[derive(Debug, Clone)]
pub enum Statement {
    Expr(Expression),
    VarDef(VarDef),
}

impl Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Expr(expr) => write!(f, "{expr}"),
            Self::VarDef(var) => write!(f, "{var}"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct VarDef {
    pub let_kw: Span,
    pub variable: Sp<String>,
    pub equals: Span,
    pub expr: Sp<Expression>,
}

impl Display for VarDef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "let {} = {}", *self.variable, *self.expr)
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    Paren {
        open: Span,
        close: Span,
        expr: SpBox<Expression>,
    },
    BinOp {
        lhs: SpBox<Expression>,
        rhs: SpBox<Expression>,
        op: Sp<BinOperator>,
    },
    UnaryOp {
        expr: SpBox<Expression>,
        op: Sp<UnOperator>,
    },
    Literal(f32),
    Variable(String),
    FunctionCall {
        name: Sp<String>,
        open: Span,
        args: Vec<(Sp<Expression>, Option<Span>)>,
        close: Span,
    },
}
impl Expression {
    pub fn is_print_expr(&self) -> bool {
        match self {
            Expression::FunctionCall { name, .. } => **name == "print",
            _ => false,
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Paren { expr, .. } => write!(f, "({})", **expr),
            Self::BinOp { lhs, rhs, op } => {
                write!(f, "{} {} {}", **lhs, **op, **rhs)
            }
            Self::UnaryOp { expr, op } => {
                write!(f, "{}{}", **op, **expr)
            }
            Self::Literal(val) => write!(f, "{val}"),
            Self::Variable(var) => write!(f, "{var}"),
            Self::FunctionCall { name, args, .. } => {
                write!(f, "{}(", **name)?;
                for (idx, arg) in args.iter().enumerate() {
                    write!(f, "{}", arg.0.inner())?;
                    if idx < args.len() - 1 {
                        write!(f, ", ")?;
                    }
                }

                write!(f, ")")
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum BinOperator {
    Add,
    Sub,
    Mul,
    Div,
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

#[derive(Debug, Clone)]
pub enum UnOperator {
    Negation,
}

impl Display for UnOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Negation => write!(f, "-"),
        }
    }
}
