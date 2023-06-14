use std::{collections::HashSet, fmt};

use crate::ty::{Type, TypeVar};

use super::utils::TypeVarGen;

#[derive(Debug)]
pub(super) struct PolyType {
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
            ty: vg.next_ty(),
        }
    }

    pub fn instantiate(&self, vg: &mut TypeVarGen) -> Type {
        let mut ty = self.ty.clone();

        if self.bound_variables.is_empty() {
            return ty;
        }

        let first_fresh_var = self.bound_variables.len();
        vg.0 += first_fresh_var as u32;

        ty.replace_vars(|var| {
            self.bound_variables
                .iter()
                .position(|&v| v == var)
                .map(|idx| Type::Var(TypeVar::from_u32((first_fresh_var + idx) as u32)))
        });

        ty
    }

    pub fn ty(&self) -> &Type {
        &self.ty
    }

    pub fn bound_vars(&self) -> &[TypeVar] {
        &self.bound_variables
    }

    pub fn into_inner(self) -> (Vec<TypeVar>, Type) {
        (self.bound_variables, self.ty)
    }
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
        let ty_var = var_gen.next();
        let tv = Type::Var(ty_var);

        let polytype = PolyType::new(vec![ty_var], Type::Function(vec![tv.clone()], Box::new(tv)));

        let fresh_var = Type::Var(TypeVar::from_u32(1));
        assert_eq!(
            polytype.instantiate(&mut var_gen),
            Type::Function(vec![fresh_var.clone()], Box::new(fresh_var))
        )
    }

    #[test]
    fn polytype_instantiation2() {
        let mut var_gen = TypeVarGen::new();
        let ty_var1 = var_gen.next();
        let ty_var2 = var_gen.next();
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
        )
    }
}
