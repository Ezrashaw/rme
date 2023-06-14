use std::collections::{HashMap, HashSet};

use crate::{
    ast::{Expression, FnDef, Statement},
    ty::Type,
    typeck::ty::PrimType,
    Sp,
};

use super::{polytype::PolyType, unify::Subst, utils::TypeVarGen};

pub fn infer_stmt(stmt: &Statement) -> (Type, Subst) {
    match stmt {
        Statement::Expr(expr) => {
            infer_expr(&mut TypeVarGen::new(), &mut TypeEnv::empty(), expr.inner())
        }
        Statement::VarDef(_) => todo!(),
        Statement::FnDef(_) => todo!(),
    }
}

fn infer_expr(vg: &mut TypeVarGen, env: &mut TypeEnv, expr: &Expression) -> (Type, Subst) {
    let x = match expr {
        Expression::Literal(lit) => (PrimType::from_lit(*lit).into(), Subst::empty()),
        Expression::Variable(var) => (
            env.lookup(var).expect("unknown variable").instantiate(vg),
            Subst::empty(),
        ),
        // Expression::FunctionCall {
        //     expr,
        //     args,
        //     open,
        //     close,
        // } => todo!(),
        // Expression::Paren { expr, .. } => infer_expr(vg, env, expr.inner()),
        // Expression::BinaryOp { lhs, op, rhs } => todo!(),
        // Expression::UnaryOp { op, expr } => todo!(),
        _ => todo!(),
    };

    x
}

fn infer_fn_def(vg: &mut TypeVarGen, env: &mut TypeEnv, fn_def: &FnDef) -> (Type, Subst) {
    let args = fn_def.args.iter().map(|a| a.0.inner()).collect::<Vec<_>>();

    let mut arg_tys = args.iter().map(|_| vg.next_ty()).collect::<Vec<_>>();

    // add types to env

    let (ret_ty, subst) = infer_expr(vg, env, fn_def.expr.inner());

    for arg_ty in &mut arg_tys {
        subst.subst_shallow(arg_ty);
    }

    let fn_ty = Type::Function(arg_tys, Box::new(ret_ty));
    (fn_ty, subst)
}

struct TypeEnv<'a> {
    variables: HashMap<&'a str, PolyType>,
}

impl<'a> TypeEnv<'a> {
    fn empty() -> Self {
        Self {
            variables: HashMap::new(),
        }
    }

    pub fn generalize_ty(mut self, ty: Type) -> PolyType {
        // FIXME: this should be inherent on `Type` and should be more efficient
        // when writing comments: note that there are likely to be less
        // variables in the type than the environment, so this goes first
        let mut vars_in_ty = Vec::new();
        ty.walk(|ty| {
            if let Type::Var(var) = ty {
                if !vars_in_ty.contains(var) {
                    vars_in_ty.push(*var);
                }
            }
        });

        // FIXME: this is sloooow
        // FIXME: put hashsets everywhere
        // FIXME: I suppose we could skip this if `ty` is a monotype (`vars_in_ty` is empty)
        for (_, var) in self.variables.iter() {
            var.ty().walk(|ty| {
                if let Type::Var(var) = ty && let Some(idx) = vars_in_ty.iter().position(|v| v == var) {
                    // FIXME: when not debugging, this can be swap_remove
                    vars_in_ty.remove(idx);
                }
            })
        }

        PolyType::new(vars_in_ty, ty)
    }

    fn lookup(&self, var: &str) -> Option<&PolyType> {
        self.variables.get(var)
    }

    fn with_fresh_vars<T: AsRef<str>>(
        &mut self,
        vg: &mut TypeVarGen,
        var_names: &[T],
        mut f: impl FnOnce(&mut Self),
    ) -> Vec<Type> {
        let mut shadowed_vars = Vec::with_capacity(var_names.len());

        self.variables.reserve(var_names.len());
        for var in var_names {
            let shadowed = self.variables.insert(var.as_ref(), PolyType::fresh_var(vg));

            if let Some(shadowed_ty) = shadowed {
                shadowed_vars.push((var.as_ref(), shadowed_ty));
            }
        }

        f(self);

        for var in vars {
            let var = var.as_ref();
            self.variables.remove(var.as_ref());
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::{
        ty::{Type, TypeVar},
        typeck::{infer::TypeEnv, polytype::PolyType},
    };

    const TY_VAR: TypeVar = TypeVar::from_u32(0);
    const VAR_TY: Type = Type::Var(TY_VAR);

    fn generalize(env: TypeEnv, ty: Type, expected_bound_vars: &[TypeVar]) {
        let polytype = env.generalize_ty(ty);

        assert_eq!(polytype.bound_vars(), expected_bound_vars);
    }

    #[test]
    fn test_generalization1() {
        let env = TypeEnv::empty();

        generalize(env, VAR_TY, &[TY_VAR]);
    }

    #[test]
    fn test_generalization2() {
        // FIXME: create a `TypeEnv::new` fn
        let env = TypeEnv {
            variables: HashMap::from([("", PolyType::new(vec![], VAR_TY))]),
        };

        generalize(env, VAR_TY, &[]);
    }

    #[test]
    fn test_generalization3() {
        let env = TypeEnv {
            variables: HashMap::from([("", PolyType::new(vec![], VAR_TY))]),
        };

        let ty_var2 = TypeVar::from_u32(1);
        let var_ty2 = Type::Var(ty_var2);

        let ty = Type::Function(vec![var_ty2], Box::new(VAR_TY));

        generalize(env, ty, &[ty_var2])
    }
}
