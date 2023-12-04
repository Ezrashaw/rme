use crate::RegressionTests;
use rme::{
    ast::TypedStmt,
    parse,
    typeck::{self, TypeEnv},
    SourceMap,
};
use std::io;

/// Regression tests for the type inference algorithm ([`rme::typeck`]).
pub struct TypeckTests;

impl RegressionTests for TypeckTests {
    const NAMESPACE: &'static str = "typeck";

    fn run_test(out: &mut impl io::Write, input: &str) -> io::Result<()> {
        let sm = SourceMap::from_input(input.to_owned());

        match parse(input) {
            Ok(ast) => {
                let mut env = TypeEnv::empty();
                let tys = typeck::infer(&mut env, &ast);

                match tys {
                    Ok(tys) => {
                        for (ty, stmt) in tys.iter().zip(ast.statements) {
                            let ty_stmt = TypedStmt(stmt.0.inner(), ty);
                            writeln!(out, "{ty_stmt}")?;
                        }
                    }
                    Err(err) => err.into_diag().emit_to_write(out, &sm)?,
                }
            }
            Err(err) => err.emit_to_write(out, &sm)?,
        }

        Ok(())
    }
}
