use crate::ty::{Type, TypeVar};

pub struct TypeVarGen(pub u32);

impl TypeVarGen {
    pub fn new() -> Self {
        Self(0)
    }

    pub fn next(&mut self) -> TypeVar {
        let val = self.0;
        self.0 += 1;

        TypeVar::from_u32(val)
    }

    pub fn next_ty(&mut self) -> Type {
        Type::Var(self.next())
    }
}
