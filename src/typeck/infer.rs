use std::{collections::HashSet, fmt};

use crate::{
    ast::{Expression, FnDef, Statement, VarDef},
    typeck::{
        polytype::PolyType,
        ty::{PrimType, Type},
        unify::{unify, Subst, TypeError},
        utils::TypeVarGen,
    },
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
    let vg = &mut TypeVarGen::new();
    let subst = &mut Subst::empty();

    match stmt {
        Statement::Expr(expr) => infer_expr(env, vg, subst, expr.inner()),
        Statement::VarDef(var_def) => infer_var_def(env, vg, subst, var_def.inner()),
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
    env.push(fn_def.name.inner(), fn_type.clone());

    Ok(fn_type)
}

fn infer_var_def<'a>(
    env: &mut TypeEnv<'a>,
    vg: &mut TypeVarGen,
    subst: &mut Subst,
    var_def: &'a VarDef,
) -> Result<Type, TypeError> {
    let expr_ty = infer_expr(env, vg, subst, var_def.expr.inner())?;
    env.push(var_def.name.inner(), expr_ty.clone());

    Ok(expr_ty)
}

pub struct TypeEnv<'a> {
    variables: Vec<(&'a str, PolyType)>,
}

impl<'a> TypeEnv<'a> {
    #[must_use]
    pub fn empty() -> Self {
        Self {
            variables: Vec::new(),
        }
    }

    pub(super) fn generalize_ty(&self, ty: Type) -> PolyType {
        // when writing comments: note that there are likely to be less
        // variables in the type than the environment, so this goes first
        let mut vars_in_ty = HashSet::new();
        ty.walk_vars(|var| {
            vars_in_ty.insert(var);
        });

        if !vars_in_ty.is_empty() {
            for var in self.variables.iter().map(|(_, ty)| ty) {
                var.ty().walk_vars(|var| {
                    vars_in_ty.remove(&var);
                });
            }
        }

        PolyType::new(Vec::from_iter(vars_in_ty), ty)
    }

    fn push(&mut self, name: &'a str, ty: Type) {
        let ty = self.generalize_ty(ty);
        self.variables.push((name, ty));
    }

    pub(super) fn lookup(&self, var: &str) -> Option<&PolyType> {
        self.variables
            .iter()
            .rev()
            .find_map(|(name, pt)| (*name == var).then_some(pt))
    }

    pub(super) fn with_new_scope<T>(
        &mut self,
        vg: &mut TypeVarGen,
        subst: &mut Subst,
        vars: impl IntoIterator<Item = (&'a str, PolyType)>,
        f: impl FnOnce(&mut Self, &mut TypeVarGen, &mut Subst) -> T,
    ) -> (T, Vec<Type>) {
        let mut count = 0;
        for var in vars {
            self.variables.push(var);
            count += 1;
        }

        let t = f(self, vg, subst);

        let vars = self
            .variables
            .drain((self.variables.len() - count)..)
            .map(|(_, pt)| pt.into_inner().1)
            .collect();
        (t, vars)
    }

    // FIXME: shorten function arg-lists by creating an `InferCtx`
    //        (not stolen from rustc lol)
    pub fn with_fresh_vars<T>(
        &mut self,
        vg: &mut TypeVarGen,
        subst: &mut Subst,
        var_names: &[&'a str],
        f: impl FnOnce(&mut Self, &mut TypeVarGen, &mut Subst) -> T,
    ) -> (T, Vec<Type>) {
        // FIXME: uneeded allocation here
        let vars = var_names
            .iter()
            .map(|&n| (n, PolyType::fresh_var(vg)))
            .collect::<Vec<_>>();

        self.with_new_scope(vg, subst, vars, f)
    }
}

impl fmt::Display for TypeEnv<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "---Type Environment---")?;
        for (var, polytype) in &self.variables {
            writeln!(f, "{var}: {polytype}")?;
        }

        write!(f, "----------------------")?;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ty::{Type, TypeVar},
        typeck::{infer::TypeEnv, polytype::PolyType},
    };

    const TY_VAR: TypeVar = TypeVar::from_u32(0);
    const VAR_TY: Type = Type::Var(TY_VAR);

    fn generalize(env: TypeEnv, ty: Type, expected_bound_vars: &[TypeVar]) {
        let polytype = env.generalize_ty(ty);

        assert_eq!(polytype.into_inner().0, expected_bound_vars);
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
            variables: vec![("", PolyType::new(vec![], VAR_TY))],
        };

        generalize(env, VAR_TY, &[]);
    }

    #[test]
    fn test_generalization3() {
        let env = TypeEnv {
            variables: vec![("", PolyType::new(vec![], VAR_TY))],
        };

        let ty_var2 = TypeVar::from_u32(1);
        let var_ty2 = Type::Var(ty_var2);

        let ty = Type::Function(vec![var_ty2], Box::new(VAR_TY));

        generalize(env, ty, &[ty_var2]);
    }
}
