use crate::{
    ast::{BinOperator, Expression, UnOperator},
    parser::Parser,
    token::{Token, TokenKind},
    DErr, Sp,
};

//
type ExprRes = Result<Sp<Expression>, DErr>;

impl<I: Iterator<Item = Token>> Parser<I> {
    /// Parses an expression.
    ///
    /// Corresponds to the `<expression>` non-terminal.
    pub(super) fn parse_expr(&mut self) -> ExprRes {
        self.parse_equality_expr()
    }

    parse_binop! {
        /// Parses an equality expression. Parses at the equals (`==`)
        /// precedence level.
        ///
        /// Corresponds to the `<equality-expression>` non-terminal.
        fn parse_equality_expr => parse_additive_expr + [
            TokenKind::DoubleEquals => BinOperator::Eq
        ]
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
    fn parse_unary_prefix(&mut self) -> ExprRes {
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
    fn parse_unary_postfix(&mut self) -> ExprRes {
        let mut expr = self.parse_fn_call()?;

        // Unary postfix operators are inherently left-recursive. We have to
        // remove left recursion (with a loop) to be able to parse them.
        while let Some(bang_span) = self.eat(TokenKind::Bang) {
            expr = Expression::new_unop(Sp::new(UnOperator::Factorial, bang_span), expr);
        }

        Ok(expr)
    }

    /// Parses a function call.
    ///
    /// Corresponds to the `<fn_call>` non-terminal.
    fn parse_fn_call(&mut self) -> ExprRes {
        let mut expr = self.parse_factor()?;

        while let Some(open) = self.eat(TokenKind::ParenOpen) {
            let fn_call = Expression::FunctionCall {
                expr: expr.map_inner(Box::new),
                open,
                args: self.parse_comma_delimited(Self::parse_expr)?,
                close: self.expect(TokenKind::ParenClose)?,
            };
            expr = fn_call.spanify();
        }

        Ok(expr)
    }

    /// Parses a "factor".
    /// This contains the highest precedence rules:
    /// - parenthesized expressions
    /// - variable references
    /// - literals
    ///
    /// Corresponds to the `<factor>` non-terminal.
    fn parse_factor(&mut self) -> ExprRes {
        let (tok, tok_span) = self.next()?.into_parts();

        Ok(match tok {
            TokenKind::ParenOpen => {
                let expr = Expression::Paren {
                    open: tok_span,
                    expr: self.parse_expr()?.map_inner(Box::new),
                    close: self.expect(TokenKind::ParenClose)?,
                };

                expr.spanify()
            }
            TokenKind::Literal(lit) => Sp::new(Expression::Literal(lit), tok_span),
            TokenKind::Identifier(id) => Sp::new(Expression::Variable(id), tok_span),
            _ => {
                // The word "expression" is used, as opposed to something more
                // local like "factor" because it conveys intent better and an
                // expression is valid here, it'll just be parsed by a
                // production from higher in the call-stack instead.
                return Err(Self::create_expected_err(
                    "expression",
                    Token::new(tok, tok_span),
                ));
            }
        })
    }
}

macro_rules! parse_binop {
    ($(#[$attr:meta])* fn $name:ident => $lower:ident + [$($tok:pat => $op:expr),+]) => {
        $(#[$attr])*
        fn $name(&mut self) -> ExprRes {
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
