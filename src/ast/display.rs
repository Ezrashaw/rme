use std::fmt::{self, Display};

use crate::{
    ansi::{Colour, Style, WriteStyle},
    ast::{Ast, BinOperator, Expression, FnDef, Statement, TypedStmt, UnOperator, VarDef},
    token::{Keyword, Literal},
    ty::Type,
};

use super::Return;

impl fmt::Display for Ast {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (stmt, _) in &self.statements {
            stmt.fmt(f)?;
            f.write_punct(";")?;
            writeln!(f)?;
        }

        Ok(())
    }
}

fn display_stmt(stmt: &Statement, ty: Option<&Type>, f: &mut fmt::Formatter) -> fmt::Result {
    match stmt {
        Statement::Expr(expr) => {
            expr.fmt(f)?;
            f.write_ty_annotation(ty)
        }
        Statement::VarDef(def) => display_var_def(def.inner(), ty, f),
        Statement::FnDef(def) => display_fn_def(def.inner(), ty, f),
        Statement::Return(rtn) => display_rtn(rtn.inner(), ty, f),
    }
}

impl fmt::Display for TypedStmt<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        display_stmt(self.0, Some(self.1), f)
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        display_stmt(self, None, f)
    }
}

fn display_var_def(var_def: &VarDef, ty: Option<&Type>, f: &mut fmt::Formatter) -> fmt::Result {
    f.write_kw(Keyword::Let)?;
    var_def.name.fmt(f)?;
    f.write_ty_annotation(ty)?;
    f.write_punct(" = ")?;
    var_def.expr.fmt(f)
}

impl fmt::Display for VarDef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        display_var_def(self, None, f)
    }
}

fn display_rtn(rtn: &Return, ty: Option<&Type>, f: &mut fmt::Formatter) -> fmt::Result {
    f.write_kw(Keyword::Return)?;
    f.write_ty_annotation(ty)?;
    rtn.expr.fmt(f)
}

impl fmt::Display for Return {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        display_rtn(self, None, f)
    }
}

fn display_fn_def(fn_def: &FnDef, ty: Option<&Type>, f: &mut fmt::Formatter) -> fmt::Result {
    f.write_kw(Keyword::Fn)?;
    fn_def.name.fmt(f)?;
    f.write_delimiter("(")?;

    for (idx, arg) in fn_def.args.iter().enumerate() {
        arg.0.fmt(f)?;
        if idx < fn_def.args.len() - 1 {
            f.write_punct(", ")?;
        }
    }

    f.write_delimiter(")")?;
    f.write_ty_annotation(ty)?;
    f.write_punct(" = ")?;
    fn_def.expr.fmt(f)
}

impl fmt::Display for FnDef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        display_fn_def(self, None, f)
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Paren { expr, .. } => {
                f.write_delimiter("(")?;
                expr.fmt(f)?;
                f.write_delimiter(")")
            }
            Self::BinaryOp { lhs, rhs, op } => write!(f, "{lhs} {} {rhs}", *op),
            Self::UnaryOp { expr, op } => match op.inner() {
                UnOperator::Negation => write!(f, "{op}{expr}"),
                UnOperator::Factorial => write!(f, "{expr}{op}"),
            },
            Self::Literal(lit) => f.write_lit(*lit),
            Self::Variable(var) => write!(f, "{var}"),
            Self::FunctionCall { name, args, .. } => {
                name.fmt(f)?;
                f.write_delimiter("(")?;
                for (idx, arg) in args.iter().enumerate() {
                    arg.0.fmt(f)?;
                    if idx < args.len() - 1 {
                        f.write_punct(", ")?;
                    }
                }

                f.write_delimiter(")")
            }
        }
    }
}

impl fmt::Display for BinOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Add => f.write_op("+"),
            Self::Sub => f.write_op("-"),
            Self::Mul => f.write_op("*"),
            Self::Div => f.write_op("/"),
            Self::Eq => f.write_op("=="),
        }
    }
}

impl fmt::Display for UnOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Negation => f.write_op("-"),
            Self::Factorial => f.write_op("!"),
        }
    }
}

const KW_STYLE: WriteStyle = Style::fg(Colour::Cyan).bold();
const LIT_STYLE: WriteStyle = Style::fg(Colour::Yellow).italic();
const PUNCT_STYLE: WriteStyle = Style::fg(Colour::BrightBlack).bold();
const DELIM_STYLE: WriteStyle = Style::fg(Colour::BrightMagenta);

trait AstDisplayExt {
    fn write_kw(&mut self, kw: Keyword) -> fmt::Result;
    fn write_lit(&mut self, lit: Literal) -> fmt::Result;
    fn write_punct(&mut self, punct: &'static str) -> fmt::Result;
    fn write_op(&mut self, op: &'static str) -> fmt::Result;
    fn write_delimiter(&mut self, op: &'static str) -> fmt::Result;
    fn write_ty_annotation(&mut self, ty: Option<&Type>) -> fmt::Result;
}

impl AstDisplayExt for fmt::Formatter<'_> {
    fn write_kw(&mut self, kw: Keyword) -> fmt::Result {
        write!(self, "{}{}{} ", KW_STYLE, kw.user_str(), Style::reset())
    }

    fn write_lit(&mut self, lit: Literal) -> fmt::Result {
        let style = if matches!(lit, Literal::Bool(_)) {
            KW_STYLE
        } else {
            LIT_STYLE
        };
        write!(self, "{}{lit}{}", style, Style::reset())
    }

    fn write_punct(&mut self, punct: &'static str) -> fmt::Result {
        write!(self, "{}{punct}{}", PUNCT_STYLE, Style::reset())
    }

    fn write_op(&mut self, op: &'static str) -> fmt::Result {
        write!(self, "{op}")
    }

    fn write_delimiter(&mut self, delim: &'static str) -> fmt::Result {
        write!(self, "{}{delim}{}", DELIM_STYLE, Style::reset())
    }

    fn write_ty_annotation(&mut self, ty: Option<&Type>) -> fmt::Result {
        if let Some(ty) = ty {
            self.write_punct(": ")?;
            ty.fmt(self)?;
        }

        Ok(())
    }
}
