use crate::{token, ty::Type, Sp, SpBox, Span};

#[cfg(feature = "ast-debug")]
pub mod debug;

mod display;

pub struct Ast {
    pub statements: Vec<(Sp<Statement>, Span)>,
}

// FIXME: remove, this is so typeck regression tests work
pub struct TypedStmt<'a>(pub &'a Statement, pub &'a Type);

#[derive(Clone)]
pub enum Statement {
    Expr(Sp<Expression>),
    VarDef(Sp<VarDef>),
    FnDef(Sp<FnDef>),
    Return(Sp<Return>)
}

#[derive(Clone)]
pub struct Return {
    pub rtn_kw: Span,
    pub expr: Sp<Expression>
}

#[derive(Clone)]
pub struct VarDef {
    pub let_kw: Span,
    pub name: Sp<String>,
    pub equals: Span,
    pub expr: Sp<Expression>,
}

#[derive(Clone)]
pub struct FnDef {
    pub fn_kw: Span,
    pub name: Sp<String>,
    pub parens: (Span, Span),
    pub args: Vec<(Sp<String>, Option<Span>)>,
    pub equals: Span,
    pub expr: Sp<Expression>,
}

pub type FnCallArg = (Sp<Expression>, Option<Span>);

#[derive(Clone)]
pub enum Expression {
    Paren {
        parens: (Span, Span),
        expr: SpBox<Expression>,
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
        name: SpBox<Expression>,
        parens: (Span, Span),
        args: Vec<FnCallArg>,
    },
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
