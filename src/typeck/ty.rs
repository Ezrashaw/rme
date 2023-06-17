use std::fmt;

use crate::token::Literal;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Primitive(PrimType),
    Function(Vec<Type>, Box<Type>),
    Var(TypeVar),
}

impl Type {
    pub fn any_var(&self, mut f: impl FnMut(&TypeVar) -> bool) -> bool {
        let mut val = false;
        // FIXME: this isn't that efficient, we should stop when we see `true`
        self.walk_(&mut |ty| {
            if let Type::Var(v) = ty && f(v) {
                val = true;
            }
        });

        val
    }

    pub fn walk(&self, mut f: impl FnMut(&Type)) {
        self.walk_(&mut |ty| f(ty))
    }

    pub fn replace_vars(&mut self, mut f: impl FnMut(TypeVar) -> Option<Type>) {
        self.walk_mut_(&mut |ty| {
            if let Type::Var(v) = ty {
                if let Some(new_ty) = f(*v) {
                    *ty = new_ty;
                }
            }
        })
    }

    pub fn walk_mut(&mut self, mut f: impl FnMut(&mut Type)) {
        self.walk_mut_(&mut f)
    }

    fn walk_(&self, f: &mut impl FnMut(&Type)) {
        f(self);
        match self {
            Type::Primitive(_) | Type::Var(_) => {}
            Type::Function(args, ret) => {
                ret.walk_(f);
                args.iter().for_each(|ty| ty.walk_(f));
            }
        }
    }

    fn walk_mut_(&mut self, f: &mut impl FnMut(&mut Type)) {
        f(self);
        match self {
            Type::Primitive(_) | Type::Var(_) => {}
            Type::Function(args, ret) => {
                ret.walk_mut_(f);
                args.iter_mut().for_each(|ty| ty.walk_mut_(f));
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PrimType {
    Float,
    Bool,
}

#[derive(Hash, Debug, Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct TypeVar(u32);

impl TypeVar {
    pub fn value(&self) -> u32 {
        self.0
    }

    pub(super) const fn from_u32(value: u32) -> Self {
        Self(value)
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
        write!(
            f,
            "?{}",
            (TryInto::<u8>::try_into(self.0).unwrap() + b'a') as char
        )
    }
}
