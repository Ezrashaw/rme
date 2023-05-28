use std::{fmt::Display, io};

use crate::Sp;

use super::{Ast, Expression, Statement, VarDef};

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct AstIndent(usize);

impl AstIndent {
    pub fn new() -> Self {
        Self(0)
    }

    fn mv(self) -> Self {
        Self(self.0 + 1)
    }
}

impl Default for AstIndent {
    fn default() -> Self {
        Self::new()
    }
}

impl Display for AstIndent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for _ in 0..self.0 {
            write!(f, "| ")?;
        }

        Ok(())
    }
}

pub fn dbg_ast(ast: &Ast, w: &mut impl io::Write, indent: AstIndent) -> io::Result<()> {
    for (idx, (stmt, semi)) in ast.statements.iter().enumerate() {
        writeln!(w, "{indent}---AST Statement (semi: {semi:?}) #{idx}---")?;

        write!(w, "{indent}")?;
        dbg_stmt(stmt, w, indent.mv())?;
        writeln!(w, "-------")?;
        if idx < ast.statements.len() - 1 {
            writeln!(w)?;
        }
    }

    Ok(())
}

fn dbg_stmt(stmt: &Sp<Statement>, w: &mut impl io::Write, indent: AstIndent) -> io::Result<()> {
    match stmt.inner() {
        Statement::Expr(expr) => {
            writeln!(w, "Statement(Expression)@{:?}", stmt.span())?;

            write!(w, "{indent}")?;
            dbg_expr(expr.as_ref(), w, indent.mv())
        }
        Statement::VarDef(var_def) => {
            writeln!(w, "Statement(VarDef)@{:?}", stmt.span())?;

            write!(w, "{indent}")?;
            dbg_var_def(var_def, w, indent.mv())
        }
    }
}

pub fn dbg_var_def(
    var_def: &Sp<VarDef>,
    w: &mut impl io::Write,
    indent: AstIndent,
) -> io::Result<()> {
    let (var_def, span) = var_def.as_parts();
    writeln!(w, "VarDef@{span:?}")?;

    writeln!(w, "{indent}let_kw: {:?}", var_def.let_kw)?;
    writeln!(w, "{indent}name: {:?}", var_def.name)?;
    writeln!(w, "{indent}equals: {:?}", var_def.equals)?;

    write!(w, "{indent}expr: ")?;
    dbg_expr(var_def.expr.as_ref(), w, indent.mv())
}

pub fn dbg_expr(
    self_: Sp<&Expression>,
    w: &mut impl io::Write,
    indent: AstIndent,
) -> io::Result<()> {
    match self_.inner() {
        Expression::Paren { open, close, expr } => {
            writeln!(w, "Paren@{:?}", self_.span())?;

            writeln!(w, "{indent}open: {open:?}")?;
            writeln!(w, "{indent}close: {close:?}")?;

            write!(w, "{indent}expr: ")?;
            dbg_expr(expr.unbox(), w, indent.mv())
        }
        Expression::UnaryOp { op, expr } => {
            writeln!(w, "UnaryOp@{:?}", self_.span())?;

            writeln!(w, "{indent}op: {op:?}")?;

            write!(w, "{indent}expr: ")?;
            dbg_expr(expr.unbox(), w, indent.mv())
        }
        Expression::Literal(lit) => {
            writeln!(w, "{lit:?}@{:?}", self_.span())
        }
        Expression::BinaryOp { op, lhs, rhs } => {
            writeln!(w, "BinaryOp@{:?}", self_.span())?;

            writeln!(w, "{indent}op: {op:?}")?;

            write!(w, "{indent}lhs: ")?;
            dbg_expr(lhs.unbox(), w, indent.mv())?;

            write!(w, "{indent}rhs: ")?;
            dbg_expr(rhs.unbox(), w, indent.mv())
        }
        Expression::Variable(var) => {
            writeln!(w, "Variable({var:?})@{:?}", self_.span())
        }
        Expression::FunctionCall {
            expr,
            args,
            open,
            close,
        } => {
            writeln!(w, "FunctionCall@{:?}", self_.span())?;

            write!(w, "{indent}expr: ")?;
            dbg_expr(expr.unbox(), w, indent.mv())?;
            writeln!(w, "{indent}open: {open:?}")?;
            writeln!(w, "{indent}close: {close:?}")?;

            for (idx, (arg, comma)) in args.iter().enumerate() {
                writeln!(w, "{indent}arg #{idx} (comma): {comma:?}")?;

                write!(w, "{indent}arg #{idx}: ")?;
                dbg_expr(arg.as_ref(), w, indent.mv())?;
            }

            Ok(())
        }
    }
}
