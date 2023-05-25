use crate::RegressionTests;
use rme::{parser::parse, SourceMap};
use std::io;

/// Regression tests for the parser ([`rme::Parser`]).
pub struct ParserTests;

impl RegressionTests for ParserTests {
    const NAMESPACE: &'static str = "parser";

    fn run_test(out: &mut impl io::Write, input: &str) -> io::Result<()> {
        let sm = SourceMap::from_input(input.to_owned());

        match parse(input, 0) {
            Ok(ast) => {
                writeln!(out, "formatted:              {ast}")?;
                writeln!(out, "formatted (precedence): {ast:#}\n")?;
                writeln!(out, "{ast:#?}")?
            }
            Err(err) => err.emit_to_write(out, &sm),
        }

        Ok(())
    }
}
