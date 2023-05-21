use std::iter::Peekable;

use crate::{
    ast::{BinOperator, Expression, Statement, UnOperator, VarDef},
    DiagEmitter, DiagLevel, Sp, Span, Token, TokenKind,
};

pub struct Parser<'a, I: Iterator<Item = Token>> {
    input: Peekable<I>,
    emitter: &'a DiagEmitter<'a>,
}

impl<'a, I: Iterator<Item = Token>> Parser<'a, I> {
    pub fn new(input: I, emitter: &'a DiagEmitter<'a>) -> Self {
        Self {
            input: input.peekable(),
            emitter,
        }
    }

    fn next_token(&mut self) -> Option<Token> {
        self.input.next().or_else(|| {
            self.emitter
                .print_diag(DiagLevel::Error, "unexpected end of input", Span::EOF);

            None
        })
    }

    fn emit_expected_err(&self, expected: &str, found: Token) {
        self.emitter.print_diag(
            DiagLevel::Error,
            format!("expected {expected} but found `{}`", found.inner()),
            found.span(),
        );
    }

    fn expect_token(&mut self, token: TokenKind, expected: &str) -> Option<Span> {
        let read = self.next_token()?;
        if *read == token {
            Some(read.span())
        } else {
            self.emit_expected_err(expected, read);

            None
        }
    }

    pub fn parse(mut self) -> Option<Sp<Statement>> {
        let stmt = self.parse_stmt();

        if stmt.is_some() {
            if let Some(tok) = self.input.next() {
                self.emitter.print_diag(
                    DiagLevel::Error,
                    "input left over",
                    Span::merge(tok.span(), Span::EOF),
                );

                return None;
            }
        }

        stmt
    }

    fn parse_stmt(&mut self) -> Option<Sp<Statement>> {
        // FIXME: this is ok, but it returns without error for empty input
        let tok = self.input.peek()?;

        let stmt = if *tok.inner() == TokenKind::LetKeyword {
            let (var_def, span) = self.parse_var_def()?.into_parts();

            Sp::new(Statement::VarDef(var_def), span)
        } else {
            let (expr, span) = self.parse_expr()?.into_parts();

            Sp::new(Statement::Expr(expr), span)
        };

        Some(stmt)
    }

    fn parse_var_def(&mut self) -> Option<Sp<VarDef>> {
        let let_kw = self.expect_token(
            TokenKind::LetKeyword,
            "BUG: should always get let keyword here",
        )?;

        let variable = self.next_token()?;
        let variable = if let TokenKind::Identifier(id) = variable.inner() {
            Sp::new(id.clone(), variable.span())
        } else {
            self.emit_expected_err("identifier", variable);
            return None;
        };

        let equals = self.expect_token(TokenKind::Equals, "`=`")?;
        let expr = self.parse_expr()?;

        let span = Span::merge(let_kw, expr.span());

        Some(Sp::new(
            VarDef {
                let_kw,
                variable,
                equals,
                expr,
            },
            span,
        ))
    }

    fn parse_expr(&mut self) -> Option<Sp<Expression>> {
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

        Some(expr)
    }

    fn parse_term(&mut self) -> Option<Sp<Expression>> {
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

        Some(expr)
    }

    fn parse_factor(&mut self) -> Option<Sp<Expression>> {
        let token = self.next_token()?;

        Some(match token.inner() {
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
            TokenKind::Number(val) => Sp::new(Expression::Literal(*val), token.span()),
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
                self.emit_expected_err("factor", token);

                return None;
            }
        })
    }
}
