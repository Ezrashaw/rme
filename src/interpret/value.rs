use std::fmt::Display;

use crate::{
    ast::{BinOperator, Expression, UnOperator},
    DErr, Diag, Literal, Sp, SubDiag, SubDiagLevel,
};

#[derive(Debug, Clone)]
pub enum Type {
    Float,
    Bool,
    Function,
    Unit,
}

impl Type {
    pub fn operator_error(op: Sp<String>, operands: Vec<(Sp<Self>, &str)>) -> DErr {
        let (span_tag, op_span) = op.into_parts();

        let mut err = Diag::new_err("type error", op_span);
        err.span_tag(span_tag);

        for (value, name) in operands {
            err.add_subdiag(SubDiag::new(
                SubDiagLevel::Info,
                format!("{name} has type `{}`", value.inner()),
                value.span(),
            ));
        }

        err
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Float => write!(f, "float"),
            Self::Bool => write!(f, "bool"),
            Self::Function => write!(f, "fn(..)"),
            Self::Unit => write!(f, "()"),
        }
    }
}

#[derive(Clone)]
pub enum Value {
    Float(f32),
    Bool(bool),
    Function(Vec<Sp<String>>, Sp<Expression>),
    Unit,
}

impl From<Literal> for Value {
    fn from(value: Literal) -> Self {
        match value {
            Literal::Float(x) => Self::Float(x),
            Literal::Bool(x) => Self::Bool(x),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Float(x) => write!(f, "{x}"),
            Self::Bool(x) => write!(f, "{x}"),
            Self::Function(_, _) => write!(f, "fn(..)"),
            Self::Unit => write!(f, "()"),
        }
    }
}

impl Value {
    pub const fn type_of(&self) -> Type {
        match self {
            Self::Float(_) => Type::Float,
            Self::Bool(_) => Type::Bool,
            Self::Function(_, _) => Type::Function,
            Self::Unit => Type::Unit,
        }
    }

    pub fn eval_binop(lhs: Sp<Self>, rhs: Sp<Self>, op: Sp<BinOperator>) -> Result<Self, DErr> {
        let val = match (lhs.inner(), rhs.inner()) {
            (Self::Float(lhs), Self::Float(rhs)) => {
                let val = match op.inner() {
                    BinOperator::Add => lhs + rhs,
                    BinOperator::Sub => lhs - rhs,
                    BinOperator::Mul => lhs * rhs,
                    BinOperator::Div => lhs / rhs,
                    BinOperator::Eq => return Ok(Self::Bool(lhs == rhs)),
                };

                Self::Float(val)
            }
            _ => {
                let err = Type::operator_error(
                    op.map_inner(|op| format!("`{op:?}` cannot be used on these types")),
                    vec![
                        (lhs.map_inner(|x| x.type_of()), "LHS"),
                        (rhs.map_inner(|x| x.type_of()), "RHS"),
                    ],
                );

                return Err(err);
            }
        };

        Ok(val)
    }

    pub fn eval_unop(expr: Sp<Self>, op: Sp<UnOperator>) -> Result<Self, DErr> {
        let val = match expr.inner() {
            Self::Float(expr) => match op.inner() {
                UnOperator::Negation => Self::Float(-expr),
                UnOperator::Factorial => {
                    let expr = *expr as u128;
                    let mut val = 1u128;
                    for i in 1..=expr {
                        val = val.saturating_mul(i);
                    }

                    Self::Float(val as f32)
                }
            },
            _ => {
                let expr_ty = expr.map_inner(|x| x.type_of());
                let err = Type::operator_error(
                    op.map_inner(|op| format!("`{op:?}` cannot be used on type `{}`", expr_ty)),
                    vec![(expr_ty, "operand")],
                );

                return Err(err);
            }
        };

        Ok(val)
    }
}
