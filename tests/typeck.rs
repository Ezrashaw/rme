use crate::RegressionTests;
use rme::{
    ast::TypedStmt,
    parse,
    typeck::{self, utils::TypeVarGen, TypeEnv},
    SourceMap,
};
use std::io;

/// Regression tests for the type inference algorithm ([`rme::typeck`]).
pub struct TypeckTests;

impl RegressionTests for TypeckTests {
    const NAMESPACE: &'static str = "typeck";

    fn run_test(out: &mut impl io::Write, input: &str) -> io::Result<()> {
        let sm = SourceMap::from_input(input.to_owned());

        match parse(input, 0) {
            Ok(ast) => {
                let mut env = TypeEnv::empty();
                let mut vg = TypeVarGen::new();
                let stmts = ast.statements.iter().map(|s| s.0.inner());
                let tys = typeck::infer(&mut env, &mut vg, stmts.clone());

                for (ty, stmt) in tys.iter().zip(stmts) {
                    let ty_stmt = TypedStmt(stmt, ty);
                    writeln!(out, "{ty_stmt}")?;
                }
            }
            Err(err) => err.emit_to_write(out, &sm)?,
        }

        Ok(())
    }
}
