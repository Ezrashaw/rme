use crate::{lexer, Sp, SpBox, Span};

#[cfg(feature = "ast-debug")]
pub mod debug;

mod display;

pub struct Ast {
    pub statements: Vec<(Sp<Statement>, Span)>,
}

#[derive(Clone)]
pub enum Statement {
    Expr(Sp<Expression>),
    VarDef(Sp<VarDef>),
}

#[derive(Clone)]
pub struct VarDef {
    pub let_kw: Span,
    pub name: Sp<String>,
    pub equals: Span,
    pub expr: Sp<Expression>,
}

#[derive(Clone)]
pub enum Expression {
    Paren {
        open: Span,
        close: Span,
        expr: SpBox<Expression>,
    },
    BinaryOp {
        op: Sp<BinOperator>,
        lhs: SpBox<Expression>,
        rhs: SpBox<Expression>,
    },
    UnaryOp {
        op: Sp<UnOperator>,
        expr: SpBox<Expression>,
    },
    Literal(lexer::Literal),
    Variable(String),
    FunctionCall {
        expr: SpBox<Expression>,
        args: Vec<(Sp<Expression>, Option<Span>)>,
        open: Span,
        close: Span,
    },
}

impl Expression {
    pub fn new_binop(op: Sp<BinOperator>, lhs: Sp<Self>, rhs: Sp<Self>) -> Sp<Self> {
        let span = Span::merge(lhs.span(), rhs.span());
        Sp::new(
            Self::BinaryOp {
                op,
                lhs: lhs.map_inner(Box::new),
                rhs: rhs.map_inner(Box::new),
            },
            span,
        )
    }

    pub fn new_unop(op: Sp<UnOperator>, expr: Sp<Self>) -> Sp<Self> {
        let span = Span::merge(op.span(), expr.span());
        Sp::new(
            Self::UnaryOp {
                op,
                expr: expr.map_inner(Box::new),
            },
            span,
        )
    }

    pub fn is_print_expr(&self) -> bool {
        match self {
            Self::FunctionCall { expr, .. } => {
                if let Self::Variable(name) = expr.unbox().inner() {
                    name == "print"
                } else {
                    false
                }
            }
            _ => false,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum BinOperator {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Clone, Copy)]
pub enum UnOperator {
    Negation,
    Factorial,
}
