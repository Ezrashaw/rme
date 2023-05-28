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

impl Statement {
    pub fn spanify(self) -> Sp<Self> {
        let span = match &self {
            Statement::Expr(expr) => expr.span(),
            Statement::VarDef(var_def) => var_def.span(),
        };

        Sp::new(self, span)
    }
}

#[derive(Clone)]
pub struct VarDef {
    pub let_kw: Span,
    pub name: Sp<String>,
    pub equals: Span,
    pub expr: Sp<Expression>,
}

impl VarDef {
    pub fn spanify(self) -> Sp<Self> {
        Sp::new(self, Span::merge(self.let_kw, self.expr.span()))
    }
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
    pub fn spanify(self) -> Sp<Self> {
        let span = match &self {
            Expression::Paren { open, close, .. } => Span::merge(*open, *close),
            Expression::BinaryOp { lhs, rhs, .. } => Span::merge(lhs.span(), rhs.span()),
            Expression::UnaryOp { op, expr } => Span::merge(op.span(), expr.span()),
            Expression::FunctionCall { expr, close, .. } => Span::merge(expr.span(), *close),
            Expression::Literal(_) | Expression::Variable(_) => {
                panic!("cannot infer span for `{self}`")
            }
        };

        Sp::new(self, span)
    }

    pub fn new_binop(op: Sp<BinOperator>, lhs: Sp<Self>, rhs: Sp<Self>) -> Sp<Self> {
        let expr = Self::BinaryOp {
            op,
            lhs: lhs.map_inner(Box::new),
            rhs: rhs.map_inner(Box::new),
        };

        expr.spanify()
    }

    pub fn new_unop(op: Sp<UnOperator>, expr: Sp<Self>) -> Sp<Self> {
        let expr = Self::UnaryOp {
            op,
            expr: expr.map_inner(Box::new),
        };

        expr.spanify()
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
