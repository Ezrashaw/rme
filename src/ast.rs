use crate::{token, Sp, SpBox, Span};

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
    FnDef(Sp<FnDef>),
}

impl Statement {
    pub fn spanify(self) -> Sp<Self> {
        let span = match &self {
            Statement::Expr(expr) => expr.span(),
            Statement::VarDef(var_def) => var_def.span(),
            Statement::FnDef(fn_def) => fn_def.span(),
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
        let span = Span::merge(self.let_kw, self.expr.span());
        Sp::new(self, span)
    }
}

#[derive(Clone)]
pub struct FnDef {
    pub fn_kw: Span,
    pub name: Sp<String>,
    pub args_open: Span,
    pub args: Vec<(Sp<String>, Option<Span>)>,
    pub args_close: Span,
    pub equals: Span,
    pub expr: Sp<Expression>,
}

impl FnDef {
    pub fn spanify(self) -> Sp<Self> {
        let span = Span::merge(self.fn_kw, self.expr.span());
        Sp::new(self, span)
    }
}

pub type FnCallArg = (Sp<Expression>, Option<Span>);

#[derive(Clone)]
pub enum Expression {
    Paren {
        open: Span,
        expr: SpBox<Expression>,
        close: Span,
    },
    BinaryOp {
        lhs: SpBox<Expression>,
        op: Sp<BinOperator>,
        rhs: SpBox<Expression>,
    },
    UnaryOp {
        op: Sp<UnOperator>,
        expr: SpBox<Expression>,
    },
    Literal(token::Literal),
    Variable(String),
    FunctionCall {
        expr: SpBox<Expression>,
        args: Vec<FnCallArg>,
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
}

#[derive(Debug, Clone, Copy)]
pub enum BinOperator {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
}

/// A unary operator.
///
/// That is, a operator with a single operand. The operator may either be
/// before the operand (prefix position, more common) or after the operand
/// (postfix position, less common).
#[derive(Debug, Clone, Copy)]
pub enum UnOperator {
    /// Arithmetic negation.
    ///
    /// Note that negative literals are encoded with this; you may only ever
    /// have a positive float literal.
    ///
    /// e.g. `-1`
    Negation,

    /// "Factorial" operator.
    ///
    /// Computes the product of each value in the range `1..value`.
    Factorial,
}

impl UnOperator {
    /// Returns a value indicating whether the [`UnOperator`] is valid in the
    /// prefix or postfix position.
    pub fn is_postfix(self) -> bool {
        matches!(self, Self::Factorial)
    }
}
