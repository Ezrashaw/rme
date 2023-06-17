use std::iter::Peekable;

use crate::{
    ast::{Ast, FnDef, Statement, VarDef},
    lexer::Lexer,
    token::{Keyword, Token, TokenKind},
    DErr, Diag, Sp, Span,
};

mod expr;

pub fn parse(input: &str, span_offset: usize) -> Result<Ast, DErr> {
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
            .ok_or_else(|| Diag::error("unexpected end of input", Span::EOF))
    }

    fn peek(&mut self) -> Result<&Token, DErr> {
        self.input
            .peek()
            .ok_or_else(|| Diag::error("unexpected end of input", Span::EOF))
    }

    fn create_expected_err(expected: &str, found: Token) -> DErr {
        Diag::error(
            format!(
                "expected `{expected}` but found `{}`",
                found.inner().user_str()
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
                TokenKind::Identifier(String::new()).user_str(),
                next,
            ))
        }
    }

    fn expect(&mut self, kind: TokenKind) -> Result<Span, DErr> {
        let read = self.next()?;
        if *read.inner() == kind {
            Ok(read.span())
        } else {
            Err(Self::create_expected_err(kind.user_str(), read))
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

    fn parse_comma_delimited<T>(
        &mut self,
        f: fn(&mut Self) -> Result<T, DErr>,
    ) -> Result<Vec<(T, Option<Span>)>, DErr> {
        let mut values = Vec::new();

        // We must immediately short-circuit if we see a closing parenthesis;
        // the loop below only breaks on commas. Notice how we don't eat the
        // closing parenthesis, the calling function does, so that it gets the
        // span easily.
        if self.is(TokenKind::ParenClose) {
            return Ok(values);
        }

        loop {
            let val = f(self)?;

            // If we see a comma then (according to the grammar), more
            // arguments must exist. If not, then no more arguments can exist,
            // and we exit (allowing the caller to eat the closing
            // parenthesis). Note that this is different to many languages,
            // which allow a trailing comma.
            if let Some(comma) = self.eat(TokenKind::Comma) {
                values.push((val, Some(comma)));
            } else {
                values.push((val, None));
                break;
            }
        }

        Ok(values)
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
    // FIXME: this function needs to be tested, our current infrastructure
    //        can't handle this as it doesn't descend from `Parser::parse`
    pub fn parse_single_stmt(mut self) -> Result<Sp<Statement>, DErr> {
        let stmt = self.parse_stmt()?;

        // as part of the grammar (and user-friendliness), we allow a trailing
        // semicolon (ignoring it if does exist) for convenience purposes
        self.eat(TokenKind::Semi);

        // this is different from `Parser::parse` which eagerly parses while
        // there are tokens remaining
        if let Some(tok) = self.input.next() {
            return Err(Diag::error(
                "extraneous input",
                Span::merge(tok.span(), Span::EOF),
            ));
        }

        Ok(stmt)
    }

    /// Parses a statement, without semicolon delimiter (per the grammar).
    ///
    /// Corresponds to the `<statement>` non-terminal.
    fn parse_stmt(&mut self) -> Result<Sp<Statement>, DErr> {
        let stmt = match self.peek()?.inner() {
            TokenKind::Keyword(Keyword::Let) => Statement::VarDef(self.parse_var_def()?),
            TokenKind::Keyword(Keyword::Fn) => Statement::FnDef(self.parse_fn_def()?),
            _ => Statement::Expr(self.parse_expr()?),
        };

        Ok(stmt.spanify())
    }

    /// Parses a `let` statement, also called a variable definition (`var_def`).
    ///
    /// Corresponds to the `<var_def>` non-terminal.
    fn parse_var_def(&mut self) -> Result<Sp<VarDef>, DErr> {
        let var_def = VarDef {
            let_kw: self.expect(TokenKind::Keyword(Keyword::Let))?,
            name: self.expect_id()?,
            equals: self.expect(TokenKind::Equals)?,
            expr: self.parse_expr()?,
        };

        Ok(var_def.spanify())
    }

    /// Parses a function definition.
    ///
    /// Corresponds to the `<fn_def>` non-terminal.
    fn parse_fn_def(&mut self) -> Result<Sp<FnDef>, DErr> {
        let fn_kw = self.expect(TokenKind::Keyword(Keyword::Fn))?;
        let name = self.expect_id()?;
        let open = self.expect(TokenKind::ParenOpen)?;
        let args = self.parse_comma_delimited(Self::expect_id)?;
        let close = self.expect(TokenKind::ParenClose)?;
        let equals = self.expect(TokenKind::Equals)?;
        let expr = self.parse_expr()?;

        let fn_def = FnDef {
            fn_kw,
            name,
            parens: (open, close),
            args,
            equals,
            expr,
        };

        Ok(fn_def.spanify())
    }
}
