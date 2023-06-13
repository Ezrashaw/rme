use std::fmt;

use crate::ty::Type;

pub struct PolyType {
    bound_variables: Vec<u32>,
    ty: Type,
}

impl PolyType {
    pub fn new(bound_variables: Vec<u32>, ty: Type) -> Self {
        Self {
            bound_variables,
            ty,
        }
    }

    // pub fn instantiate(self) -> Type {

    // }
}

impl fmt::Display for PolyType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "∀")?;

        for (idx, var) in self.bound_variables.iter().enumerate() {
            write!(f, "?{var}")?;
            if idx < self.bound_variables.len() - 1 {
                write!(f, ", ")?;
            }
        }

        write!(f, ". {}", self.ty)
    }
}
