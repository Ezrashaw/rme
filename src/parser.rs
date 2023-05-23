use std::iter::Peekable;

use crate::{
    ast::{BinOperator, Expression, Statement, UnOperator, VarDef},
    DErr, Sp, Span, Token, TokenKind,
};

pub struct Parser<I: Iterator<Item = Token>> {
    input: Peekable<I>,
}

impl<I: Iterator<Item = Token>> Parser<I> {
    pub fn new(input: I) -> Self {
        Self {
            input: input.peekable(),
        }
    }

    fn next_token(&mut self) -> Result<Token, DErr> {
        self.input
            .next()
            .ok_or_else(|| DErr::new_err("unexpected end of input", Span::EOF))
    }

    fn create_expected_err(&self, expected: &str, found: Token) -> DErr {
        DErr::new_err(
            format!("expected {expected} but found `{}`", found.as_diag_str()),
            found.span(),
        )
    }

    fn expect_token(&mut self, token: TokenKind, expected: &str) -> Result<Span, DErr> {
        let read = self.next_token()?;
        if *read == token {
            Ok(read.span())
        } else {
            Err(self.create_expected_err(expected, read))
        }
    }

    pub fn parse(&mut self) -> Result<Sp<Statement>, DErr> {
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
            let (var_def, span) = self.parse_var_def()?.into_parts();

            Sp::new(Statement::VarDef(var_def), span)
        } else {
            let (expr, span) = self.parse_expr()?.into_parts();

            Sp::new(Statement::Expr(expr), span)
        };

        Ok(stmt)
    }

    fn parse_var_def(&mut self) -> Result<Sp<VarDef>, DErr> {
        let let_kw = self.expect_token(
            TokenKind::LetKeyword,
            "BUG: should always get let keyword here",
        )?;

        let variable = self.next_token()?;
        let variable = if let TokenKind::Identifier(id) = variable.inner() {
            Sp::new(id.clone(), variable.span())
        } else {
            return Err(self.create_expected_err("identifier", variable));
        };

        let equals = self.expect_token(TokenKind::Equals, "`=`")?;
        let expr = self.parse_expr()?;

        let span = Span::merge(let_kw, expr.span());

        Ok(Sp::new(
            VarDef {
                let_kw,
                variable,
                equals,
                expr,
            },
            span,
        ))
    }

    fn parse_expr(&mut self) -> Result<Sp<Expression>, DErr> {
        let mut expr = self.parse_term()?;

        while let Some(next_token) = self.input.peek() {
            let op = match **next_token {
                TokenKind::Minus => BinOperator::Sub,
                TokenKind::Plus => BinOperator::Add,
                _ => break,
            };
            let next_token = self.next_token()?;

            let rhs = self.parse_factor()?.map_inner(Box::new);

            let lhs = expr.map_inner(Box::new);
            let op = Sp::new(op, next_token.span());

            let span = Span::merge(lhs.span(), rhs.span());
            expr = Sp::new(Expression::BinOp { lhs, rhs, op }, span);
        }

        Ok(expr)
    }

    fn parse_term(&mut self) -> Result<Sp<Expression>, DErr> {
        let mut expr = self.parse_factor()?;

        while let Some(next_token) = self.input.peek() {
            let op = match **next_token {
                TokenKind::Star => BinOperator::Mul,
                TokenKind::Slash => BinOperator::Div,
                _ => break,
            };
            let next_token = self.next_token()?;

            let rhs = self.parse_factor()?.map_inner(Box::new);
            let lhs = expr.map_inner(Box::new);
            let op = Sp::new(op, next_token.span());

            let span = Span::merge(lhs.span(), rhs.span());
            expr = Sp::new(Expression::BinOp { lhs, rhs, op }, span);
        }

        Ok(expr)
    }

    fn parse_factor(&mut self) -> Result<Sp<Expression>, DErr> {
        let token = self.next_token()?;

        Ok(match token.inner() {
            TokenKind::ParenOpen => {
                let expr = self.parse_expr()?;
                let close = self.expect_token(TokenKind::ParenClose, "closing parenthesis")?;

                Sp::new(
                    Expression::Paren {
                        open: token.span(),
                        close,
                        expr: expr.map_inner(Box::new),
                    },
                    Span::merge(token.span(), close),
                )
            }
            TokenKind::Minus => {
                let factor = self.parse_factor()?;
                let fspan = factor.span();

                Sp::new(
                    Expression::UnaryOp {
                        expr: factor.map_inner(Box::new),
                        op: Sp::new(UnOperator::Negation, token.span()),
                    },
                    Span::merge(token.span(), fspan),
                )
            }
            TokenKind::Literal(val) => Sp::new(Expression::Literal(*val), token.span()),
            TokenKind::Identifier(var) => {
                let maybe_fn_call = self.input.peek();
                if Some(&TokenKind::ParenOpen) == maybe_fn_call.map(Sp::inner) {
                    let open = self.input.next().unwrap().span();

                    let mut args = Vec::new();
                    while !matches!(
                        self.input.peek().map(Sp::inner),
                        Some(TokenKind::ParenClose)
                    ) {
                        let arg = self.parse_expr()?;
                        let comma = if matches!(
                            self.input.peek().map(Sp::inner),
                            Some(TokenKind::ParenClose)
                        ) {
                            None
                        } else {
                            Some(self.expect_token(TokenKind::Comma, "`,`")?)
                        };

                        args.push((arg, comma));
                    }

                    let close = self.next_token()?.span();

                    let span = Span::merge(token.span(), close);

                    Sp::new(
                        Expression::FunctionCall {
                            name: Sp::new(var.clone(), token.span()),
                            open,
                            args,
                            close,
                        },
                        span,
                    )
                } else {
                    Sp::new(Expression::Variable(var.clone()), token.span())
                }
            }
            _ => {
                return Err(self.create_expected_err("factor", token));
            }
        })
    }
}
