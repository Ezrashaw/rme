use std::fmt;

use crate::token::Literal;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Primitive(PrimType),
    Function(Vec<Type>, Box<Type>),
    Var(TypeVar),
}

impl Type {
    pub fn walk_mut(&mut self, mut f: impl FnMut(&mut Type)) {
        f(self);
        match self {
            Type::Primitive(_) => {}
            Type::Var(_) => {}
            Type::Function(args, ret) => {
                ret.walk_mut(&mut f);
                args.iter_mut().for_each(|ty| ty.walk_mut(&mut f));
            }
        }
    }

    pub fn walk_vars_mut(&mut self, f: impl FnMut()) {}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PrimType {
    Float,
    Bool,
}

#[derive(Hash, Debug, Clone, Copy, PartialEq, Eq)]
pub struct TypeVar(u32);

impl TypeVar {
    pub fn value(&self) -> u32 {
        self.0
    }

    pub(crate) fn new(val: u32) -> Self {
        Self(val)
    }
}

impl From<PrimType> for Type {
    fn from(value: PrimType) -> Self {
        Self::Primitive(value)
    }
}

impl PrimType {
    pub fn from_lit(value: Literal) -> Self {
        match value {
            Literal::Float(_) => Self::Float,
            Literal::Bool(_) => Self::Bool,
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Primitive(prim) => write!(f, "{prim}"),
            Self::Function(args, ret) => {
                write!(f, "fn(")?;
                for (idx, arg) in args.iter().enumerate() {
                    write!(f, "{arg}")?;
                    if idx < args.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ") -> {ret}")
            }
            Self::Var(var) => write!(f, "{var}"),
        }
    }
}

impl fmt::Display for PrimType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Float => write!(f, "float"),
            Self::Bool => write!(f, "bool"),
        }
    }
}

impl fmt::Display for TypeVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "?{}", self.0)
    }
}
