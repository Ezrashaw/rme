use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Primitive(PrimType),
    Function(Vec<Type>, Box<Type>),
    Var(u32),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PrimType {
    Float,
    Bool,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Primitive(prim) => write!(f, "{prim}"),
            Self::Function(args, ret) => {
                write!(f, "fn(")?;
                for (idx, arg) in args.iter().enumerate() {
                    write!(f, "{arg}")?;
                    if idx < args.len() {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ") -> {ret}")
            }
            Self::Var(var) => write!(f, "?{var}"),
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
