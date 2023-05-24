use crate::{Sp, SpBox, Span};

mod display;

#[derive(Debug, Clone)]
pub enum Statement {
    Expr(Expression),
    VarDef(VarDef),
}

#[derive(Debug, Clone)]
pub struct VarDef {
    pub let_kw: Span,
    pub name: Sp<String>,
    pub equals: Span,
    pub expr: Sp<Expression>,
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
        args: Vec<(Sp<Expression>, Option<Span>)>,
        open: Span,
        close: Span,
    },
}

impl Expression {
    pub fn is_print_expr(&self) -> bool {
        match self {
            Self::FunctionCall { name, .. } => **name == "print",
            _ => false,
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

#[derive(Debug, Clone)]
pub enum UnOperator {
    Negation,
}
