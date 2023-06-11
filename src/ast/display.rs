use std::fmt;

use crate::{
    ansi::{Colour, Style, WriteStyle},
    token::{Keyword, Literal},
};

use super::{Ast, BinOperator, Expression, FnDef, Statement, UnOperator, VarDef};

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

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Expr(expr) => expr.fmt(f),
            Self::VarDef(def) => def.fmt(f),
            Self::FnDef(def) => def.fmt(f),
        }
    }
}

impl fmt::Display for VarDef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_kw(Keyword::Let)?;
        self.name.fmt(f)?;
        f.write_punct(" = ")?;
        self.expr.fmt(f)
    }
}

impl fmt::Display for FnDef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_kw(Keyword::Fn)?;
        self.name.fmt(f)?;
        f.write_delimiter("(")?;

        for (idx, arg) in self.args.iter().enumerate() {
            arg.0.fmt(f)?;
            if idx < self.args.len() - 1 {
                f.write_punct(", ")?;
            }
        }

        f.write_delimiter(")")?;
        f.write_punct(" = ")?;
        self.expr.fmt(f)
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
            Self::FunctionCall { expr, args, .. } => {
                expr.fmt(f)?;
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
}

impl AstDisplayExt for fmt::Formatter<'_> {
    fn write_kw(&mut self, kw: Keyword) -> fmt::Result {
        write!(self, "{}{}{} ", KW_STYLE, kw.diag_str(), Style::reset())
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
}
