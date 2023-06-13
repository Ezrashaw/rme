use std::collections::HashMap;

use crate::{
    ast::{Expression, Statement},
    ty::TypeVar,
    typeck::ty::PrimType,
};

use super::{ty::Type, unify::Subst, PolyType};

pub fn infer_stmt(stmt: &Statement) -> (Type, Subst) {
    match stmt {
        Statement::Expr(expr) => infer_expr(&mut TypeEnv::empty(), expr.inner()),
        Statement::VarDef(_) => todo!(),
        Statement::FnDef(_) => todo!(),
    }
}

fn infer_expr(env: &mut TypeEnv, expr: &Expression) -> (Type, Subst) {
    let x = match expr {
        Expression::Literal(lit) => (PrimType::from_lit(*lit).into(), Subst::empty()),
        // Expression::Variable(var) => (
        //     env.lookup(var).expect("unknown variable").clone(),
        //     Subst::empty(),
        // ),
        // Expression::Paren { open, expr, close } => todo!(),
        // Expression::BinaryOp { lhs, op, rhs } => todo!(),
        // Expression::UnaryOp { op, expr } => todo!(),
        // Expression::FunctionCall {
        //     expr,
        //     args,
        //     open,
        //     close,
        // } => todo!(),
        _ => todo!(),
    };

    x
}

pub struct TypeEnv<'a> {
    variables: HashMap<&'a str, PolyType>,
}

impl<'a> TypeEnv<'a> {
    fn empty() -> Self {
        Self {
            variables: HashMap::new(),
        }
    }

    // fn lookup(&self, var: &str) -> Option<&Type> {
    //     self.variables.get(var)
    // }
}

struct TypeVarGen(u32);

impl TypeVarGen {
    pub fn next(&mut self) -> TypeVar {
        let val = self.0;
        self.0 += 1;

        TypeVar::new(val)
    }
}
