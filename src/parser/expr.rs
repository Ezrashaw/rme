use crate::{
    ast::{BinOperator, Expression, UnOperator},
    parser::Parser,
    DErr, Sp, Span, Token, TokenKind,
};

impl<I: Iterator<Item = Token>> Parser<I> {
    /// Parses an expression.
    ///
    /// Corresponds to the `<expression>` non-terminal.
    pub(super) fn parse_expr(&mut self) -> Result<Sp<Expression>, DErr> {
        self.parse_additive_expr()
    }

    parse_binop! {
        /// Parses an additive expression. Parses at the addition and
        /// subtraction precedence level.
        ///
        /// Corresponds to the `<additive-expression>` non-terminal.
        fn parse_additive_expr => parse_term + [
            TokenKind::Plus => BinOperator::Add,
            TokenKind::Minus => BinOperator::Sub
        ]
    }

    parse_binop! {
        /// Parses a term. Parses at the multiplication and division precedence
        /// level.
        ///
        /// Corresponds to the `<term>` non-terminal.
        fn parse_term => parse_unary_prefix + [
            TokenKind::Star => BinOperator::Mul,
            TokenKind::Slash => BinOperator::Div
        ]
    }

    /// Parses a unary prefix operator expression. Currently, this is only
    /// unary negation.
    ///
    /// Corresponds to the `<unary_prefix>` non-terminal.
    fn parse_unary_prefix(&mut self) -> Result<Sp<Expression>, DErr> {
        if let Some(op_span) = self.eat(TokenKind::Minus) {
            let expr = self.parse_unary_prefix()?;
            let expr = Expression::new_unop(Sp::new(UnOperator::Negation, op_span), expr);

            Ok(expr)
        } else {
            self.parse_unary_postfix()
        }
    }

    /// Parses a unary postfix operator expression. Currently, this is only
    /// the factorial operator.
    ///
    /// Corresponds to the `<unary_postfix>` non-terminal.
    fn parse_unary_postfix(&mut self) -> Result<Sp<Expression>, DErr> {
        let mut expr = self.parse_factor()?;

        // Unary postfix operators are inherently left-recursive. We have to
        // remove left recursion (with a loop) to be able to parse them.
        while let Some(bang_span) = self.eat(TokenKind::Bang) {
            expr = Expression::new_unop(Sp::new(UnOperator::Factorial, bang_span), expr);
        }

        Ok(expr)
    }

    /// Parses a "factor".
    /// This contains the highest precedence rules:
    /// - parenthesized expressions
    /// - function calls
    /// - variable references
    /// - literals
    ///
    /// Corresponds to the `<factor>` non-terminal.
    fn parse_factor(&mut self) -> Result<Sp<Expression>, DErr> {
        let (tok, tok_span) = self.next()?.into_parts();

        Ok(match tok {
            TokenKind::ParenOpen => {
                let expr = self.parse_expr()?.map_inner(Box::new);
                let close = self.expect(TokenKind::ParenClose)?;

                let expr = Expression::Paren {
                    open: tok_span,
                    close,
                    expr,
                };

                Sp::new(expr, Span::merge(tok_span, close))
            }
            TokenKind::Literal(lit) => Sp::new(Expression::Literal(lit), tok_span),
            TokenKind::Identifier(id) => {
                if let Some(open_paren) = self.eat(TokenKind::ParenOpen) {
                    self.parse_fn_call_args(Sp::new(id, tok_span), open_paren)?
                } else {
                    Sp::new(Expression::Variable(id), tok_span)
                }
            }
            _ => {
                return Err(Self::create_expected_err(
                    "factor",
                    Token::new(tok, tok_span),
                ));
            }
        })
    }

    fn parse_fn_call_args(
        &mut self,
        name: Sp<String>,
        open_paren: Span,
    ) -> Result<Sp<Expression>, DErr> {
        let mut args = Vec::new();
        let close_paren = if let Some(close_paren) = self.eat(TokenKind::ParenClose) {
            close_paren
        } else {
            loop {
                let arg = self.parse_expr()?;

                if let Some(comma) = self.eat(TokenKind::Comma) {
                    args.push((arg, Some(comma)));
                } else {
                    args.push((arg, None));
                    break self.expect(TokenKind::ParenClose)?;
                }
            }
        };

        let full_span = Span::merge(name.span(), close_paren);
        let expr = Expression::FunctionCall {
            name,
            args,
            open: open_paren,
            close: close_paren,
        };
        Ok(Sp::new(expr, full_span))
    }
}

macro_rules! parse_binop {
    ($(#[$attr:meta])* fn $name:ident => $lower:ident + [$($tok:pat => $op:expr),+]) => {
        $(#[$attr])*
        fn $name(&mut self) -> Result<Sp<Expression>, DErr> {
            let mut expr = self.$lower()?;

            while let Some(peek) = self.input.peek() {
                let op = match *peek.inner() {
                    $($tok => $op,)*
                    _ => break,
                };

                let op_span = self.next()?.span();
                let rhs = self.$lower()?;

                expr = Expression::new_binop(Sp::new(op, op_span), expr, rhs);
            }

            Ok(expr)
        }
    };
}

use parse_binop;
