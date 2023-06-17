use crate::ty::{Type, TypeVar};

pub struct TypeVarGen(pub u32);

impl Default for TypeVarGen {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeVarGen {
    #[must_use]
    pub fn new() -> Self {
        Self(0)
    }

    pub fn fresh_var(&mut self) -> TypeVar {
        let val = self.0;
        self.0 += 1;

        TypeVar::from_u32(val)
    }

    pub fn fresh_ty(&mut self) -> Type {
        Type::Var(self.fresh_var())
    }
}
