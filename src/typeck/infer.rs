use std::{
    collections::HashMap,
    fmt::{self, Display},
};

use crate::{
    ast::{Expression, FnDef, Statement},
    ty::Type,
    typeck::ty::PrimType,
    Sp,
};

use super::{
    polytype::PolyType,
    unify::{unify, Subst, TypeError},
    utils::TypeVarGen,
};

pub fn infer<'a>(
    env: &mut TypeEnv<'a>,
    stmts: impl Iterator<Item = &'a Statement>,
) -> Result<Vec<Type>, TypeError> {
    let mut types = Vec::new();

    for stmt in stmts {
        let ty = infer_stmt(env, stmt)?;
        types.push(ty);
    }

    Ok(types)
}

fn infer_stmt<'a>(env: &mut TypeEnv<'a>, stmt: &'a Statement) -> Result<Type, TypeError> {
    let mut vg = &mut TypeVarGen::new();
    let mut subst = &mut Subst::empty();

    match stmt {
        Statement::Expr(expr) => infer_expr(env, vg, subst, expr.inner()),
        Statement::VarDef(_) => todo!(),
        Statement::FnDef(fn_def) => infer_fn_def(env, vg, subst, fn_def.inner()),
    }
}

fn infer_expr(
    env: &mut TypeEnv,
    vg: &mut TypeVarGen,
    subst: &mut Subst,
    expr: &Expression,
) -> Result<Type, TypeError> {
    match expr {
        Expression::Literal(lit) => Ok(PrimType::from_lit(*lit).into()),
        Expression::Paren { expr, .. } => infer_expr(env, vg, subst, expr.inner()),
        Expression::Variable(var) => Ok(env.lookup(var).expect("unknown variable").instantiate(vg)),
        Expression::FunctionCall { name, args, .. } => {
            let fn_type = infer_expr(env, vg, subst, name.inner())?;

            let arg_tys = args
                .iter()
                .map(|arg| infer_expr(env, vg, subst, arg.0.inner()))
                .collect::<Result<Vec<_>, _>>()?;

            let mut ret_type = vg.fresh_ty();
            unify(
                subst,
                fn_type,
                Type::Function(arg_tys, Box::new(ret_type.clone())),
            )?;

            subst.subst_shallow(&mut ret_type);

            Ok(ret_type)
        }
        // Expression::BinaryOp { lhs, op, rhs } => todo!(),
        // Expression::UnaryOp { op, expr } => todo!(),
        _ => todo!(),
    }
}

fn infer_fn_def<'a>(
    env: &mut TypeEnv<'a>,
    vg: &mut TypeVarGen,
    subst: &mut Subst,
    fn_def: &'a FnDef,
) -> Result<Type, TypeError> {
    let args = fn_def
        .args
        .iter()
        .map(|(a, _)| a.inner().as_str())
        .collect::<Vec<_>>();

    let (ret_ty, mut arg_tys) = env.with_fresh_vars(vg, subst, &args, |env, vg, subst| {
        infer_expr(env, vg, subst, fn_def.expr.inner())
    });

    let ret_ty = ret_ty?;

    for arg_ty in &mut arg_tys {
        subst.subst_shallow(arg_ty);
    }

    let fn_type = Type::Function(arg_tys, Box::new(ret_ty));
    // FIXME: this is basically a specialized `let` definition, not a
    //        anonymous function abstraction expression.
    env.push(fn_def.name.inner(), &fn_type);

    Ok(fn_type)
}

pub struct TypeEnv<'a> {
    variables: HashMap<&'a str, PolyType>,
}

impl<'a> TypeEnv<'a> {
    #[must_use]
    pub fn empty() -> Self {
        Self {
            variables: HashMap::new(),
        }
    }

    pub(super) fn generalize_ty(&self, ty: Type) -> PolyType {
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

        // FIXME: this is sloooow; put hashsets everywhere
        for var in self.variables.values() {
            if vars_in_ty.is_empty() {
                break;
            }

            var.ty().walk(|ty| {
                if let Type::Var(var) = ty && let Some(idx) = vars_in_ty.iter().position(|v| v == var) {
                    // FIXME: when not debugging, this can be swap_remove
                    vars_in_ty.remove(idx);
                }
            });
        }

        PolyType::new(vars_in_ty, ty)
    }

    fn push(&mut self, name: &'a str, ty: &Type) {
        let ty = self.generalize_ty(ty.clone());
        self.variables.insert(name, ty);
    }

    fn lookup(&self, var: &str) -> Option<&PolyType> {
        self.variables.get(var)
    }

    // FIXME: refactor this into a generic "extend env" fn
    pub fn with_fresh_vars<T>(
        &mut self,
        vg: &mut TypeVarGen,
        subst: &mut Subst,
        var_names: &[&'a str],
        mut f: impl FnOnce(&mut Self, &mut TypeVarGen, &mut Subst) -> T,
    ) -> (T, Vec<Type>) {
        let mut shadowed_vars = HashMap::<&'a str, PolyType>::with_capacity(var_names.len());

        self.variables.reserve(var_names.len());
        for var in var_names {
            let shadowed = self.variables.insert(var, PolyType::fresh_var(vg));

            if let Some(shadowed_ty) = shadowed {
                shadowed_vars.insert(var, shadowed_ty);
            }
        }

        let t = f(self, vg, subst);

        let mut types = Vec::new();
        for var in var_names {
            let ty = self.variables.remove(var).unwrap();
            types.push(ty.into_inner().1);

            if let Some(shadowed) = shadowed_vars.remove(var) {
                self.variables.insert(var, shadowed);
            }
        }

        (t, types)
    }
}

impl fmt::Display for TypeEnv<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "---Type Environment---")?;
        for (var, polytype) in &self.variables {
            writeln!(f, "{var}: {polytype}")?;
        }

        write!(f, "----------------------");

        Ok(())
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

        generalize(env, ty, &[ty_var2]);
    }
}
