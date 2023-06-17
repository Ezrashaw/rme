use std::fmt;

use crate::{
    ty::{Type, TypeVar},
    typeck::utils::TypeVarGen,
};

#[derive(Debug)]
pub(super) struct PolyType {
    // FIXME: replace this with a `HashSet`
    bound_variables: Vec<TypeVar>,
    ty: Type,
}

impl PolyType {
    pub fn new(bound_variables: Vec<TypeVar>, ty: Type) -> Self {
        Self {
            bound_variables,
            ty,
        }
    }

    pub fn fresh_var(vg: &mut TypeVarGen) -> Self {
        Self {
            bound_variables: Vec::new(),
            ty: vg.fresh_ty(),
        }
    }

    pub fn instantiate(&self, vg: &mut TypeVarGen) -> Type {
        let mut ty = self.ty.clone();

        if self.bound_variables.is_empty() {
            return ty;
        }

        // assumption: all bound variables occur in the type
        let first_fresh_var = vg.0;
        vg.0 += self.bound_variables.len() as u32;

        ty.replace_vars(|var| {
            self.bound_variables
                .iter()
                .position(|&v| v == var)
                .map(|idx| idx as u32)
                .map(|idx| Type::Var(TypeVar::from_u32(first_fresh_var + idx)))
        });

        ty
    }

    pub const fn ty(&self) -> &Type {
        &self.ty
    }

    pub fn into_inner(self) -> (Vec<TypeVar>, Type) {
        (self.bound_variables, self.ty)
    }
}

impl fmt::Display for PolyType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "∀")?;

        for (idx, var) in self.bound_variables.iter().enumerate() {
            write!(f, "{var}")?;
            if idx < self.bound_variables.len() - 1 {
                write!(f, ", ")?;
            }
        }

        write!(f, ". {}", self.ty)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ty::{Type, TypeVar},
        typeck::utils::TypeVarGen,
    };

    use super::PolyType;

    #[test]
    fn polytype_instantiation1() {
        let mut var_gen = TypeVarGen::new();
        let ty_var = var_gen.fresh_var();
        let tv = Type::Var(ty_var);

        let polytype = PolyType::new(vec![ty_var], Type::Function(vec![tv.clone()], Box::new(tv)));

        let fresh_var = Type::Var(TypeVar::from_u32(1));
        assert_eq!(
            polytype.instantiate(&mut var_gen),
            Type::Function(vec![fresh_var.clone()], Box::new(fresh_var))
        );
    }

    #[test]
    fn polytype_instantiation2() {
        let mut var_gen = TypeVarGen::new();
        let ty_var1 = var_gen.fresh_var();
        let ty_var2 = var_gen.fresh_var();
        let tv1 = Type::Var(ty_var1);
        let tv2 = Type::Var(ty_var2);

        let polytype = PolyType::new(
            vec![ty_var1, ty_var2],
            Type::Function(vec![tv1], Box::new(tv2)),
        );

        let fresh_var1 = Type::Var(TypeVar::from_u32(2));
        let fresh_var2 = Type::Var(TypeVar::from_u32(3));
        assert_eq!(
            polytype.instantiate(&mut var_gen),
            Type::Function(vec![fresh_var1], Box::new(fresh_var2))
        );
    }
}
