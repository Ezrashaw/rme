use std::iter::Peekable;

use crate::{
    ast::{Statement, VarDef},
    lex, DErr, Sp, Span, Token, TokenKind,
};

mod expr;

pub fn parse(input: &str, span_offset: usize) -> Result<Sp<Statement>, DErr> {
    let tokens = lex(input, span_offset)
        .into_iter()
        .collect::<Result<Vec<Token>, DErr>>()?;

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

    fn create_expected_err(&self, expected: &str, found: Token) -> DErr {
        DErr::new_err(
            format!("expected `{expected}` but found `{}`", found.as_diag_str()),
            found.span(),
        )
    }

    fn expect_id(&mut self) -> Result<Sp<String>, DErr> {
        let next = self.next()?;
        if let TokenKind::Identifier(id) = next.inner() {
            Ok(Sp::new(id.clone(), next.span()))
        } else {
            Err(self.create_expected_err(TokenKind::Identifier(String::new()).as_diag_str(), next))
        }
    }

    fn expect(&mut self, kind: TokenKind) -> Result<Span, DErr> {
        let read = self.next()?;
        if *read == kind {
            Ok(read.span())
        } else {
            Err(self.create_expected_err(kind.as_diag_str(), read))
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
}

impl<I: Iterator<Item = Token>> Parser<I> {
    pub fn parse(mut self) -> Result<Sp<Statement>, DErr> {
        let stmt = self.parse_stmt();

        if stmt.is_ok() {
            if let Some(tok) = self.input.next() {
                return Err(DErr::new_err(
                    "input left over",
                    Span::merge(tok.span(), Span::EOF),
                ));
            }
        }

        stmt
    }

    fn parse_stmt(&mut self) -> Result<Sp<Statement>, DErr> {
        let tok = self
            .input
            .peek()
            .ok_or_else(|| DErr::new_err("input was empty", Span::EOF))?;

        let stmt = if *tok.inner() == TokenKind::LetKeyword {
            // FIXME: inline once `TokenKind` becomes `Copy`
            let let_kw = tok.span();
            let (var_def, span) = self.parse_var_def(let_kw)?.into_parts();

            Sp::new(Statement::VarDef(var_def), span)
        } else {
            let (expr, span) = self.parse_expr()?.into_parts();

            Sp::new(Statement::Expr(expr), span)
        };

        Ok(stmt)
    }

    fn parse_var_def(&mut self, let_kw: Span) -> Result<Sp<VarDef>, DErr> {
        let name = self.expect_id()?;
        let equals = self.expect(TokenKind::Equals)?;
        let expr = self.parse_expr()?;

        let span = Span::merge(let_kw, expr.span());
        Ok(Sp::new(
            VarDef {
                let_kw,
                name,
                equals,
                expr,
            },
            span,
        ))
    }
}
