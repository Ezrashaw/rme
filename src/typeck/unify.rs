use std::collections::HashMap;

use crate::ty::TypeVar;

use super::ty::Type;

pub fn unify(subst: &mut Subst, mut t1: Type, mut t2: Type) -> Result<(), UnifyError> {
    subst.subst_shallow(&mut t1);
    subst.subst_shallow(&mut t2);
    let (t1, t2) = (t1, t2);

    match (t1, t2) {
        (Type::Primitive(p1), Type::Primitive(p2)) if p1 == p2 => Ok(()),

        (Type::Var(v1), Type::Var(v2)) if v1 == v2 => Ok(()),
        (Type::Var(var), ty) | (ty, Type::Var(var)) => {
            if ty.any_var(|&v| v == var) {
                return Err(UnifyError::InfiniteType);
            }

            subst.push(var, ty);

            Ok(())
        }

        (Type::Function(args1, ret1), Type::Function(args2, ret2)) => {
            unify(subst, *ret1, *ret2)?;
            for (t1, t2) in args1.into_iter().zip(args2) {
                unify(subst, t1, t2)?;
            }

            Ok(())
        }

        _ => Err(UnifyError::TypeError),
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum UnifyError {
    InfiniteType,
    TypeError,
}

#[derive(Debug)]
pub struct Subst(HashMap<TypeVar, Type>);

impl Subst {
    pub fn empty() -> Self {
        Self(HashMap::new())
    }

    fn push(&mut self, from: TypeVar, mut to: Type) {
        self.subst(&mut to);
        let res = self.0.insert(from, to);

        assert!(res.is_none());
    }

    pub fn subst(&self, ty: &mut Type) {
        ty.replace_vars(|var| self.0.get(&var).cloned());
    }

    pub fn subst_shallow(&self, ty: &mut Type) {
        if let Type::Var(var) = ty && let Some(replace) = self.0.get(&var).cloned()  {
            *ty = replace;
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ty::TypeVar,
        typeck::{
            ty::{PrimType, Type},
            unify::UnifyError,
        },
    };

    const BOOL: Type = Type::Primitive(PrimType::Bool);
    const FLOAT: Type = Type::Primitive(PrimType::Float);

    fn ty_var(val: u32) -> Type {
        Type::Var(TypeVar::from_u32(val))
    }

    fn unify(t1: Type, t2: Type) -> Result<Vec<(u32, Type)>, UnifyError> {
        let mut subst = super::Subst::empty();
        super::unify(&mut subst, t1, t2)?;

        let mut subst = subst
            .0
            .into_iter()
            .map(|(k, v)| (k.value(), v))
            .collect::<Vec<_>>();
        subst.sort_by_key(|&(k, _)| k);

        Ok(subst)
    }

    #[test]
    fn unify_primitive() {
        let subst = unify(BOOL, BOOL).unwrap();
        assert!(subst.is_empty());

        let subst = unify(FLOAT, FLOAT).unwrap();
        assert!(subst.is_empty());
    }

    #[test]
    fn unify_primitive_bad() {
        let res = unify(FLOAT, BOOL);
        assert_eq!(res, Err(UnifyError::TypeError));

        let res = unify(BOOL, FLOAT);
        assert_eq!(res, Err(UnifyError::TypeError));
    }

    #[test]
    fn unify_same_var() {
        let subst = unify(ty_var(1), ty_var(1)).unwrap();
        assert!(subst.is_empty());

        let subst = unify(ty_var(42), ty_var(42)).unwrap();
        assert!(subst.is_empty());
    }

    #[test]
    fn unify_ty_var() {
        let subst = unify(ty_var(0), BOOL).unwrap();
        assert_eq!(subst, [(0, BOOL)]);

        let subst = unify(FLOAT, ty_var(1)).unwrap();
        assert_eq!(subst, [(1, FLOAT)]);

        let subst = unify(Type::Function(Vec::new(), Box::new(FLOAT)), ty_var(1)).unwrap();
        assert_eq!(subst, [(1, Type::Function(Vec::new(), Box::new(FLOAT)))]);
    }

    #[test]
    fn unify_failing_occurs() {
        let res = unify(ty_var(0), Type::Function(Vec::new(), Box::new(ty_var(0))));

        assert_eq!(res, Err(UnifyError::InfiniteType));
    }

    #[test]
    fn unify_function_no_args() {
        let subst = unify(
            Type::Function(Vec::new(), Box::new(BOOL)),
            Type::Function(Vec::new(), Box::new(BOOL)),
        )
        .unwrap();

        assert!(subst.is_empty());
    }

    #[test]
    fn unify_function_with_args() {
        let args = vec![
            FLOAT,
            BOOL,
            Type::Function(Vec::new(), Box::new(FLOAT)),
            ty_var(0),
        ];
        let subst = unify(
            Type::Function(args.clone(), Box::new(FLOAT)),
            Type::Function(args, Box::new(FLOAT)),
        )
        .unwrap();

        assert!(subst.is_empty());
    }

    #[test]
    fn unify_function_with_variables() {
        let args1 = vec![BOOL];
        let args2 = vec![ty_var(0)];
        let subst = unify(
            Type::Function(args1, Box::new(FLOAT)),
            Type::Function(args2, Box::new(FLOAT)),
        )
        .unwrap();

        assert_eq!(subst, [(0, BOOL)]);
    }

    #[test]
    fn unify_function_with_ctx() {
        let args1 = vec![ty_var(0), ty_var(1)];
        let args2 = vec![FLOAT, ty_var(0)];
        let subst = unify(
            Type::Function(args1, Box::new(BOOL)),
            Type::Function(args2, Box::new(BOOL)),
        )
        .unwrap();

        assert_eq!(subst, [(0, FLOAT), (1, FLOAT)]);
    }

    #[test]
    fn unify_composing_subst() {
        let args1 = vec![ty_var(0), ty_var(1)];
        let arg2 = Type::Function(Vec::new(), Box::new(ty_var(0)));
        let args2 = vec![FLOAT, arg2];
        let subst = unify(
            Type::Function(args1, Box::new(BOOL)),
            Type::Function(args2, Box::new(BOOL)),
        )
        .unwrap();

        assert_eq!(
            subst,
            [(0, FLOAT), (1, Type::Function(vec![], Box::new(FLOAT)))]
        );
    }

    #[test]
    fn unify_id_applied_id() {
        let id_fn = Type::Function(vec![ty_var(0)], Box::new(ty_var(0)));
        let id_fn2 = Type::Function(vec![ty_var(1)], Box::new(ty_var(1)));
        let subst = unify(
            Type::Function(vec![id_fn2.clone()], Box::new(id_fn2.clone())),
            id_fn.clone(),
        )
        .unwrap();

        assert_eq!(subst, [(0, id_fn2)]);
    }
}
