use crate::{
    ast::{BinOperator, Expression, UnOperator},
    parser::Parser,
    DErr, Sp, Span, Token, TokenKind,
};

impl<I: Iterator<Item = Token>> Parser<I> {
    pub(super) fn parse_expr(&mut self) -> Result<Sp<Expression>, DErr> {
        self.parse_additive_exp()
    }

    parse_binop!(parse_additive_exp, parse_term, [
        TokenKind::Plus => BinOperator::Add,
        TokenKind::Minus => BinOperator::Sub
    ]);

    parse_binop!(parse_term, parse_factor, [
        TokenKind::Star => BinOperator::Mul,
        TokenKind::Slash => BinOperator::Div
    ]);

    fn parse_factor(&mut self) -> Result<Sp<Expression>, DErr> {
        let (tok, tok_span) = self.next()?.into_parts();

        Ok(match tok {
            TokenKind::ParenOpen => {
                let expr = self.parse_expr()?;
                let close = self.expect(TokenKind::ParenClose)?;

                let expr = Expression::Paren {
                    open: tok_span,
                    close,
                    expr: expr.map_inner(Box::new),
                };

                Sp::new(expr, Span::merge(tok_span, close))
            }
            // unary operators
            _ if let Some(op) = tok_to_un_op(&tok) => {
                let factor = self.parse_factor()?;
                let fspan = factor.span();

                let expr = Expression::UnaryOp {
                    expr: factor.map_inner(Box::new),
                    op: Sp::new(op, tok_span),
                };

                Sp::new(expr, Span::merge(tok_span, fspan))
            }
            TokenKind::Literal(val) => Sp::new(Expression::Literal(val), tok_span),
            TokenKind::Identifier(id) => {
                if let Some(open_paren) = self.eat(TokenKind::ParenOpen) {
                    self.parse_fn_call(Sp::new(id,tok_span), open_paren)?
                } else {
                    Sp::new(Expression::Variable(id.clone()), tok_span)
                }
            }
            _ => {
                return Err(self.create_expected_err("factor", Token::new(tok, tok_span)));
            }
        })
    }

    fn parse_fn_call(
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

fn tok_to_un_op(tok: &TokenKind) -> Option<UnOperator> {
    Some(match tok {
        TokenKind::Minus => UnOperator::Negation,
        _ => return None,
    })
}

macro_rules! parse_binop {
    ($name:ident, $lower:ident, [$($tok:pat => $op:expr),+]) => {
        fn $name(&mut self) -> Result<Sp<Expression>, DErr> {
            let mut expr = self.$lower()?;

            while let Some(peek) = self.input.peek() {
                let op = match **peek {
                    $($tok => $op,)*
                    _ => break,
                };

                let op_span = self.next()?.span();
                let op = Sp::new(op, op_span);
                let rhs = self.$lower()?.map_inner(Box::new);

                let span = Span::merge(expr.span(), rhs.span());
                expr = Sp::new(Expression::BinOp { lhs: expr.map_inner(Box::new), rhs, op }, span);
            }

            Ok(expr)
        }
    };
}

use parse_binop;
