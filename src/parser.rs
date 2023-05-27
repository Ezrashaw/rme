use std::iter::Peekable;

use crate::{
    ast::{Ast, Statement, VarDef},
    DErr, Keyword, Lexer, Sp, Span, Token, TokenKind,
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
}

impl<I: Iterator<Item = Token>> Parser<I> {
    pub fn parse(mut self) -> Result<Ast, DErr> {
        let mut statements = Vec::new();
        while self.input.peek().is_some() {
            let stmt = self.parse_stmt()?;
            let semi = self.expect(TokenKind::Semi)?;

            statements.push((stmt, semi));
        }

        Ok(Ast { statements })
    }

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

    fn parse_stmt(&mut self) -> Result<Sp<Statement>, DErr> {
        let (stmt, sp) = if let Some(let_kw) = self.eat(TokenKind::Keyword(Keyword::Let)) {
            let var_def = self.parse_var_def(let_kw)?;
            let sp = var_def.span();
            (Statement::VarDef(var_def), sp)
        } else {
            let expr = self.parse_expr()?;
            let sp = expr.span();
            (Statement::Expr(expr), sp)
        };

        Ok(Sp::new(stmt, sp))
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
