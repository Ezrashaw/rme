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
    pub fn new_binop(
        op: Sp<BinOperator>,
        lhs: Sp<Expression>,
        rhs: Sp<Expression>,
    ) -> Sp<Expression> {
        let span = Span::merge(lhs.span(), rhs.span());
        Sp::new(
            Self::BinOp {
                lhs: lhs.map_inner(Box::new),
                rhs: rhs.map_inner(Box::new),
                op,
            },
            span,
        )
    }

    pub fn new_unop(op: Sp<UnOperator>, expr: Sp<Expression>) -> Sp<Expression> {
        let span = Span::merge(op.span(), expr.span());
        Sp::new(
            Self::UnaryOp {
                expr: expr.map_inner(Box::new),
                op,
            },
            span,
        )
    }

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
    Factorial,
}
