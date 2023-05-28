use std::iter::Peekable;

use crate::{
    ast::{Ast, Statement, VarDef},
    DErr, Keyword, Lexer, Sp, Span, Token, TokenKind,
};

mod expr;

pub fn parse(input: &str, span_offset: usize) -> Result<Ast, DErr> {
    // FIXME: there is a panic here with using collect.
    let tokens = Lexer::new(input, span_offset).collect::<Result<Vec<Token>, DErr>>()?;

    let parser = Parser::new(tokens.into_iter());
    parser.parse()
}

pub struct Parser<I: Iterator<Item = Token>> {
    input: Peekable<I>,
}

impl<I: Iterator<Item = Token>> Parser<I> {
    pub fn new(input: I) -> Self {
        Self {
            input: input.peekable(),
        }
    }

    fn next(&mut self) -> Result<Token, DErr> {
        self.input
            .next()
            .ok_or_else(|| DErr::new_err("unexpected end of input", Span::EOF))
    }

    fn create_expected_err(expected: &str, found: Token) -> DErr {
        DErr::new_err(
            format!(
                "expected `{expected}` but found `{}`",
                found.inner().diag_str()
            ),
            found.span(),
        )
    }

    fn expect_id(&mut self) -> Result<Sp<String>, DErr> {
        let next = self.next()?;
        if let TokenKind::Identifier(id) = next.inner() {
            Ok(Sp::new(id.clone(), next.span()))
        } else {
            Err(Self::create_expected_err(
                TokenKind::Identifier(String::new()).diag_str(),
                next,
            ))
        }
    }

    fn expect(&mut self, kind: TokenKind) -> Result<Span, DErr> {
        let read = self.next()?;
        if *read.inner() == kind {
            Ok(read.span())
        } else {
            Err(Self::create_expected_err(kind.diag_str(), read))
        }
    }

    fn eat(&mut self, kind: TokenKind) -> Option<Span> {
        let tok = self.input.peek()?;
        if *tok.inner() == kind {
            Some(self.input.next().unwrap().span())
        } else {
            None
        }
    }

    fn is(&mut self, kind: TokenKind) -> bool {
        let tok = self.input.peek();
        if let Some(tok) = tok {
            *tok.inner() == kind
        } else {
            false
        }
    }
}

impl<I: Iterator<Item = Token>> Parser<I> {
    /// Parses an entire program and consumes the parser itself. Statements are
    /// semicolon-delimited. Note that this is part of the `<program>`
    /// production and *not* part of the `<statement>` production.
    ///
    /// Corresponds to the `<program>` non-terminal.
    pub fn parse(mut self) -> Result<Ast, DErr> {
        let mut statements = Vec::new();
        while self.input.peek().is_some() {
            let stmt = self.parse_stmt()?;
            let semi = self.expect(TokenKind::Semi)?;

            statements.push((stmt, semi));
        }

        Ok(Ast { statements })
    }

    /// Parses a single statement and ensures that no other input was given.
    /// This is called per-line in the REPL.
    ///
    /// Parses the `<statement>` non-terminal, however also ensures that no
    /// input is left over (not per the grammar).
    pub fn parse_single_stmt(mut self) -> Result<Sp<Statement>, DErr> {
        let stmt = self.parse_stmt()?;

        if let Some(tok) = self.input.next() {
            return Err(DErr::new_err(
                "input left over",
                Span::merge(tok.span(), Span::EOF),
            ));
        }

        Ok(stmt)
    }

    /// Parses a statement, without semicolon delimiter (per the grammar).
    ///
    /// Corresponds to the `<statement>` non-terminal.
    fn parse_stmt(&mut self) -> Result<Sp<Statement>, DErr> {
        let stmt = if self.is(TokenKind::Keyword(Keyword::Let)) {
            let var_def = self.parse_var_def()?;
            Statement::VarDef(var_def)
        } else {
            let expr = self.parse_expr()?;
            Statement::Expr(expr)
        };

        Ok(stmt.spanify())
    }

    /// Parses a `let` statement, also called a variable definition (`var_def`).
    ///
    /// Corresponds to the `<var_def>` non-terminal.
    fn parse_var_def(&mut self) -> Result<Sp<VarDef>, DErr> {
        let let_kw = self.expect(TokenKind::Keyword(Keyword::Let))?;
        let name = self.expect_id()?;
        let equals = self.expect(TokenKind::Equals)?;
        let expr = self.parse_expr()?;

        let var_def = VarDef {
            let_kw,
            name,
            equals,
            expr,
        };

        Ok(var_def.spanify())
    }
}
