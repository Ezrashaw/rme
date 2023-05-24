use crate::RegressionTests;
use rme::{lex, SourceMap};
use std::io;

/// Regression tests for the lexer ([`rme::Lexer`]).
pub struct LexerTests;

impl RegressionTests for LexerTests {
    const NAMESPACE: &'static str = "lexer";

    fn run_test(out: &mut impl io::Write, input: &str) -> io::Result<()> {
        let sm = SourceMap::from_input(input);

        for tok in lex(input, 0) {
            match tok {
                Ok(tok) => writeln!(out, "{tok:?}")?,
                Err(err) => err.emit_to_write(out, &sm),
            }
        }

        Ok(())
    }
}
