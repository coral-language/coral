#![allow(clippy::if_same_then_else)]

use super::operators::*;
use super::types::Parser;
use crate::ast::*;
use crate::error::{UnifiedError as Error, UnifiedErrorKind as ErrorKind, error};
use crate::lexer::TokenKind;
use smallvec::SmallVec;
use text_size::TextRange;
use thin_vec::ThinVec;

pub type ParseResult<T> = Result<T, Box<Error>>;

impl<'a> Parser<'a> {
    pub(super) fn parse_expression(&mut self) -> ParseResult<Expr<'a>> {
        self.parse_expression_with_context(true)
    }

    pub(super) fn parse_expression_with_context(
        &mut self,
        allow_in: bool,
    ) -> ParseResult<Expr<'a>> {
        if self.peek().kind == TokenKind::Lambda {
            self.parse_lambda()
        } else {
            self.parse_named_expr(allow_in)
        }
    }

    fn parse_named_expr(&mut self, allow_in: bool) -> ParseResult<Expr<'a>> {
        let expr = self.parse_ternary(allow_in)?;

        if self.peek().kind == TokenKind::ColonEqual {
            self.advance();
            let value = self.parse_named_expr(allow_in)?;
            let span = TextRange::new(expr.span().start(), value.span().end());

            return Ok(Expr::NamedExpr(NamedExprNode {
                target: self.arena.alloc(expr),
                value: self.arena.alloc(value),
                span,
            }));
        }

        Ok(expr)
    }

    fn parse_lambda(&mut self) -> ParseResult<Expr<'a>> {
        let start = self.peek().span.start();
        self.consume(TokenKind::Lambda)?;

        let args = if self.peek().kind != TokenKind::Colon {
            self.parse_lambda_arguments()?
        } else {
            Arguments {
                posonlyargs: &[],
                args: &[],
                vararg: None,
                kwonlyargs: &[],
                kw_defaults: &[],
                kwarg: None,
                defaults: &[],
            }
        };

        self.consume(TokenKind::Colon)?;

        let body = self.arena.alloc(self.parse_expression_with_context(true)?);
        let end = body.span().end();

        Ok(Expr::Lambda(LambdaExpr {
            args,
            body,
            span: TextRange::new(start, end),
        }))
    }

    fn parse_lambda_arguments(&mut self) -> ParseResult<Arguments<'a>> {
        let mut posonlyargs: SmallVec<[Arg<'a>; 8]> = SmallVec::new();
        let mut args: SmallVec<[Arg<'a>; 8]> = SmallVec::new();
        let mut kwonlyargs: SmallVec<[Arg<'a>; 8]> = SmallVec::new();
        let mut posonly_defaults: SmallVec<[Expr<'a>; 4]> = SmallVec::new();
        let mut defaults: SmallVec<[Expr<'a>; 4]> = SmallVec::new();
        let mut kw_defaults: SmallVec<[Option<Expr<'a>>; 4]> = SmallVec::new();
        let mut vararg = None;
        let mut kwarg = None;

        let mut _seen_slash = false;
        let mut seen_star = false;

        while self.peek().kind != TokenKind::Colon && !self.is_at_end() {
            if self.peek().kind == TokenKind::Slash {
                self.advance();
                _seen_slash = true;

                if !args.is_empty() {
                    posonlyargs = args.clone();
                    posonly_defaults = defaults.clone();
                    args.clear();
                    defaults.clear();
                }

                if !self.match_token(TokenKind::Comma) {
                    break;
                }
                continue;
            }

            if self.peek().kind == TokenKind::Star {
                self.advance();
                seen_star = true;

                if self.peek().kind == TokenKind::Ident {
                    let name = self.consume_ident()?;

                    vararg = Some(Box::new(Arg {
                        arg: name,
                        annotation: None,
                    }));
                }

                if !self.match_token(TokenKind::Comma) {
                    break;
                }
                continue;
            }

            if self.peek().kind == TokenKind::DoubleStar {
                self.advance();
                let name = self.consume_ident()?;

                kwarg = Some(Box::new(Arg {
                    arg: name,
                    annotation: None,
                }));

                if !self.match_token(TokenKind::Comma) {
                    break;
                }

                if self.peek().kind != TokenKind::Colon && !self.is_at_end() {
                    let invalid_token_span = self.peek().span;
                    return Err(error(
                        ErrorKind::InvalidSyntax {
                            message: "No parameters allowed after **kwargs in lambda. \
                                     **kwargs must be the final parameter."
                                .to_string(),
                        },
                        invalid_token_span,
                    ));
                }
                break;
            }

            let name = self.consume_ident()?;

            let arg = Arg {
                arg: name,
                annotation: None,
            };

            let has_default = self.peek().kind == TokenKind::Equal;
            if has_default {
                self.advance(); // consume =
                let default_expr = self.parse_expression()?;

                if seen_star {
                    kw_defaults.push(Some(default_expr));
                } else {
                    defaults.push(default_expr);
                }
            } else if seen_star {
                kw_defaults.push(None);
            }

            if seen_star {
                kwonlyargs.push(arg);
            } else {
                args.push(arg);
            }

            if !self.match_token(TokenKind::Comma) {
                break;
            }
        }

        let mut all_defaults = posonly_defaults;
        all_defaults.extend(defaults);

        let posonlyargs_slice = self.arena.alloc_slice_iter(posonlyargs);
        let args_slice = self.arena.alloc_slice_iter(args);
        let kwonlyargs_slice = self.arena.alloc_slice_iter(kwonlyargs);
        let defaults_slice = self.arena.alloc_slice_iter(all_defaults);
        let kw_defaults_slice = self.arena.alloc_slice_iter(kw_defaults);

        Ok(Arguments {
            posonlyargs: posonlyargs_slice,
            args: args_slice,
            vararg,
            kwonlyargs: kwonlyargs_slice,
            kw_defaults: kw_defaults_slice,
            kwarg,
            defaults: defaults_slice,
        })
    }

    pub(super) fn parse_comprehensions(&mut self) -> ParseResult<&'a [Comprehension<'a>]> {
        let mut generators: SmallVec<[Comprehension<'a>; 2]> = SmallVec::new();

        while self.peek().kind == TokenKind::For || self.peek().kind == TokenKind::Async {
            let is_async = self.match_token(TokenKind::Async);
            self.consume(TokenKind::For)?;

            let target = self.parse_comprehension_target()?;
            self.consume(TokenKind::In)?;
            let iter = self.parse_or(true)?;

            let mut ifs: SmallVec<[Expr<'a>; 2]> = SmallVec::new();
            while self.match_token(TokenKind::If) {
                ifs.push(self.parse_or(true)?);
            }

            generators.push(Comprehension {
                target,
                iter,
                ifs: self.arena.alloc_slice_iter(ifs),
                is_async,
            });
        }

        Ok(self.arena.alloc_slice_iter(generators))
    }

    /// Parse the target of a comprehension, which can be a tuple or a single expression
    fn parse_comprehension_target(&mut self) -> ParseResult<Expr<'a>> {
        let start = self.peek().span.start();
        let first = self.parse_expression_with_context(false)?;

        if self.peek().kind == TokenKind::Comma {
            let mut elts = vec![first];

            while self.match_token(TokenKind::Comma) {
                if self.peek().kind == TokenKind::In {
                    break;
                }
                elts.push(self.parse_expression_with_context(false)?);
            }

            let end = elts.last().map(|e| e.span().end()).unwrap_or(start);
            Ok(Expr::Tuple(TupleExpr {
                elts: self.arena.alloc_slice_vec(elts),
                span: TextRange::new(start, end),
            }))
        } else {
            Ok(first)
        }
    }

    fn parse_ternary(&mut self, allow_in: bool) -> ParseResult<Expr<'a>> {
        let mut expr = self.parse_or(allow_in)?;

        if self.match_token(TokenKind::If) {
            let test = self.parse_or(allow_in)?;
            self.consume(TokenKind::Else)?;
            let orelse = self.parse_ternary(allow_in)?;
            let span = TextRange::new(expr.span().start(), orelse.span().end());

            expr = Expr::IfExp(IfExpExpr {
                test: self.arena.alloc(test),
                body: self.arena.alloc(expr),
                orelse: self.arena.alloc(orelse),
                span,
            });
        }

        Ok(expr)
    }

    pub(super) fn parse_or(&mut self, allow_in: bool) -> ParseResult<Expr<'a>> {
        let first = self.parse_and(allow_in)?;
        let mut values: SmallVec<[Expr<'a>; 2]> = SmallVec::new();
        values.push(first);

        while self.match_token(TokenKind::Or) {
            let right = self.parse_and(allow_in)?;
            values.push(right);
        }

        if values.len() == 1 {
            Ok(values.into_iter().next().unwrap())
        } else {
            let span = TextRange::new(
                values[0].span().start(),
                values.last().unwrap().span().end(),
            );
            Ok(Expr::BoolOp(BoolOpExpr {
                op: OP_OR,
                values: self.arena.alloc_slice_iter(values),
                span,
            }))
        }
    }

    fn parse_and(&mut self, allow_in: bool) -> ParseResult<Expr<'a>> {
        let first = self.parse_not(allow_in)?;
        let mut values: SmallVec<[Expr<'a>; 2]> = SmallVec::new();
        values.push(first);

        while self.match_token(TokenKind::And) {
            let right = self.parse_not(allow_in)?;
            values.push(right);
        }

        if values.len() == 1 {
            Ok(values.into_iter().next().unwrap())
        } else {
            let span = TextRange::new(
                values[0].span().start(),
                values.last().unwrap().span().end(),
            );
            Ok(Expr::BoolOp(BoolOpExpr {
                op: OP_AND,
                values: self.arena.alloc_slice_iter(values),
                span,
            }))
        }
    }

    fn parse_not(&mut self, allow_in: bool) -> ParseResult<Expr<'a>> {
        if self.peek().kind == TokenKind::Not {
            let start = self.peek().span.start();
            self.advance();
            let expr = self.parse_not(allow_in)?;
            let span = TextRange::new(start, expr.span().end());
            Ok(Expr::UnaryOp(UnaryOpExpr {
                op: OP_NOT,
                operand: self.arena.alloc(expr),
                span,
            }))
        } else {
            self.parse_comparison(allow_in)
        }
    }

    fn parse_comparison(&mut self, allow_in: bool) -> ParseResult<Expr<'a>> {
        let left = self.parse_bitwise_or()?;
        let mut ops: SmallVec<[&'a str; 2]> = SmallVec::new();
        let mut comparators: SmallVec<[Expr<'a>; 2]> = SmallVec::new();

        loop {
            let token_kind = self.peek().kind;

            if token_kind == TokenKind::Not {
                let next_pos = self.current + 1;
                if next_pos < self.tokens.len() && self.tokens[next_pos].kind == TokenKind::In {
                    if !allow_in {
                        break;
                    }

                    self.advance(); // consume "not"
                    self.advance(); // consume "in"
                    let right = self.parse_bitwise_or()?;
                    ops.push(OP_NOTIN);
                    comparators.push(right);
                    continue;
                } else {
                    break;
                }
            }

            if token_kind == TokenKind::In && !allow_in {
                break;
            }

            let op = match token_kind {
                TokenKind::Less => OP_LT,
                TokenKind::LessEqual => OP_LE,
                TokenKind::Greater => OP_GT,
                TokenKind::GreaterEqual => OP_GE,
                TokenKind::EqualEqual => OP_EQ,
                TokenKind::NotEqual => OP_NE,
                TokenKind::In => OP_IN,
                TokenKind::Is => {
                    self.advance();
                    if self.peek().kind == TokenKind::Not {
                        self.advance();
                        OP_ISNOT
                    } else {
                        OP_IS
                    }
                }
                _ => break,
            };

            if token_kind != TokenKind::Is {
                self.advance();
            }

            let right = self.parse_bitwise_or()?;
            ops.push(op);
            comparators.push(right);
        }

        if ops.is_empty() {
            Ok(left)
        } else {
            let span = TextRange::new(
                left.span().start(),
                comparators.last().unwrap().span().end(),
            );
            Ok(Expr::Compare(CompareExpr {
                left: self.arena.alloc(left),
                ops: self.arena.alloc_slice_iter(ops),
                comparators: self.arena.alloc_slice_iter(comparators),
                span,
            }))
        }
    }

    fn parse_bitwise_or(&mut self) -> ParseResult<Expr<'a>> {
        let mut left = self.parse_bitwise_xor()?;

        while self.match_token(TokenKind::Pipe) {
            let right = self.parse_bitwise_xor()?;
            let span = TextRange::new(left.span().start(), right.span().end());

            left = Expr::BinOp(BinOpExpr {
                left: self.arena.alloc(left),
                op: OP_BITOR,
                right: self.arena.alloc(right),
                span,
            });
        }

        Ok(left)
    }

    fn parse_bitwise_xor(&mut self) -> ParseResult<Expr<'a>> {
        let mut left = self.parse_bitwise_and()?;

        while self.match_token(TokenKind::Caret) {
            let right = self.parse_bitwise_and()?;
            let span = TextRange::new(left.span().start(), right.span().end());

            left = Expr::BinOp(BinOpExpr {
                left: self.arena.alloc(left),
                op: OP_BITXOR,
                right: self.arena.alloc(right),
                span,
            });
        }

        Ok(left)
    }

    fn parse_bitwise_and(&mut self) -> ParseResult<Expr<'a>> {
        let mut left = self.parse_shift()?;

        while self.match_token(TokenKind::Ampersand) {
            let right = self.parse_shift()?;
            let span = TextRange::new(left.span().start(), right.span().end());

            left = Expr::BinOp(BinOpExpr {
                left: self.arena.alloc(left),
                op: OP_BITAND,
                right: self.arena.alloc(right),
                span,
            });
        }

        Ok(left)
    }

    fn parse_shift(&mut self) -> ParseResult<Expr<'a>> {
        let mut left = self.parse_additive()?;

        while matches!(
            self.peek().kind,
            TokenKind::LeftShift | TokenKind::RightShift
        ) {
            let op = if self.match_token(TokenKind::LeftShift) {
                OP_LSHIFT
            } else {
                self.advance();
                OP_RSHIFT
            };
            let right = self.parse_additive()?;
            let span = TextRange::new(left.span().start(), right.span().end());

            left = Expr::BinOp(BinOpExpr {
                left: self.arena.alloc(left),
                op,
                right: self.arena.alloc(right),
                span,
            });
        }

        Ok(left)
    }

    fn parse_additive(&mut self) -> ParseResult<Expr<'a>> {
        let mut left = self.parse_multiplicative()?;

        while matches!(self.peek().kind, TokenKind::Plus | TokenKind::Minus) {
            let op = if self.match_token(TokenKind::Plus) {
                OP_ADD
            } else {
                self.advance();
                OP_SUB
            };
            let right = self.parse_multiplicative()?;
            let span = TextRange::new(left.span().start(), right.span().end());

            left = Expr::BinOp(BinOpExpr {
                left: self.arena.alloc(left),
                op,
                right: self.arena.alloc(right),
                span,
            });
        }

        Ok(left)
    }

    fn parse_multiplicative(&mut self) -> ParseResult<Expr<'a>> {
        let mut left = self.parse_unary()?;

        while matches!(
            self.peek().kind,
            TokenKind::Star
                | TokenKind::Slash
                | TokenKind::DoubleSlash
                | TokenKind::Percent
                | TokenKind::At
        ) {
            let op = match self.peek().kind {
                TokenKind::Star => {
                    self.advance();
                    OP_MULT
                }
                TokenKind::Slash => {
                    self.advance();
                    OP_DIV
                }
                TokenKind::DoubleSlash => {
                    self.advance();
                    OP_FLOORDIV
                }
                TokenKind::Percent => {
                    self.advance();
                    OP_MOD
                }
                TokenKind::At => {
                    self.advance();
                    OP_MATMULT
                }
                _ => unreachable!(),
            };
            let right = self.parse_unary()?;
            let span = TextRange::new(left.span().start(), right.span().end());

            left = Expr::BinOp(BinOpExpr {
                left: self.arena.alloc(left),
                op,
                right: self.arena.alloc(right),
                span,
            });
        }

        Ok(left)
    }

    fn parse_unary(&mut self) -> ParseResult<Expr<'a>> {
        match self.peek().kind {
            TokenKind::Await => {
                let start = self.peek().span.start();
                let await_token = self.peek().span;
                self.advance();

                if !self.context.in_async_function {
                    return Err(error(ErrorKind::AwaitOutsideAsync, await_token));
                }

                let expr = self.parse_unary()?;
                let end = expr.span().end();
                Ok(Expr::Await(AwaitExpr {
                    value: self.arena.alloc(expr),
                    span: TextRange::new(start, end),
                }))
            }
            TokenKind::Not => {
                let start = self.peek().span.start();
                self.advance();
                let expr = self.parse_unary()?;
                let span = TextRange::new(start, expr.span().end());
                Ok(Expr::UnaryOp(UnaryOpExpr {
                    op: OP_NOT,
                    operand: self.arena.alloc(expr),
                    span,
                }))
            }
            TokenKind::Minus => {
                let start = self.peek().span.start();
                self.advance();
                let expr = self.parse_unary()?;
                let span = TextRange::new(start, expr.span().end());
                Ok(Expr::UnaryOp(UnaryOpExpr {
                    op: OP_USUB,
                    operand: self.arena.alloc(expr),
                    span,
                }))
            }
            TokenKind::Plus => {
                let start = self.peek().span.start();
                self.advance();
                let expr = self.parse_unary()?;
                let span = TextRange::new(start, expr.span().end());
                Ok(Expr::UnaryOp(UnaryOpExpr {
                    op: OP_UADD,
                    operand: self.arena.alloc(expr),
                    span,
                }))
            }
            TokenKind::Tilde => {
                let start = self.peek().span.start();
                self.advance();
                let expr = self.parse_unary()?;
                let span = TextRange::new(start, expr.span().end());
                Ok(Expr::UnaryOp(UnaryOpExpr {
                    op: OP_INVERT,
                    operand: self.arena.alloc(expr),
                    span,
                }))
            }
            _ => self.parse_power(),
        }
    }

    fn parse_power(&mut self) -> ParseResult<Expr<'a>> {
        let mut left = self.parse_postfix()?;

        if self.match_token(TokenKind::DoubleStar) {
            let right = self.parse_unary()?;
            let span = TextRange::new(left.span().start(), right.span().end());

            left = Expr::BinOp(BinOpExpr {
                left: self.arena.alloc(left),
                op: OP_POW,
                right: self.arena.alloc(right),
                span,
            });
        }

        Ok(left)
    }

    fn parse_postfix(&mut self) -> ParseResult<Expr<'a>> {
        let mut expr = self.parse_primary()?;

        loop {
            match self.peek().kind {
                TokenKind::LeftParen => {
                    let opening_span = self.peek().span;
                    self.push_delimiter('(', opening_span);
                    self.advance();
                    let (args, keywords) = self.parse_arguments_call()?;
                    let closing_span = self.peek().span;
                    self.consume(TokenKind::RightParen)?;
                    self.pop_delimiter(')', closing_span)?;
                    let args_slice = self.arena.alloc_slice_iter(args);
                    let keywords_slice = self.arena.alloc_slice_iter(keywords);
                    let span = TextRange::new(expr.span().start(), self.prev().span.end());

                    expr = Expr::Call(CallExpr {
                        func: self.arena.alloc(expr),
                        args: args_slice,
                        keywords: keywords_slice,
                        span,
                    });
                }
                TokenKind::LeftBracket => {
                    let opening_span = self.peek().span;
                    self.push_delimiter('[', opening_span);
                    self.advance();

                    let first_elem = self.parse_slice_element()?;

                    if self.peek().kind == TokenKind::Comma {
                        let mut elts = vec![first_elem.clone()];
                        while self.match_token(TokenKind::Comma) {
                            if self.peek().kind == TokenKind::RightBracket {
                                break;
                            }
                            elts.push(self.parse_slice_element()?);
                        }
                        let end = elts
                            .last()
                            .map(|e| e.span().end())
                            .unwrap_or(first_elem.span().start());
                        let slice = Expr::Tuple(TupleExpr {
                            elts: self.arena.alloc_slice_vec(elts),
                            span: TextRange::new(first_elem.span().start(), end),
                        });
                        let closing_span = self.peek().span;
                        self.consume(TokenKind::RightBracket)?;
                        self.pop_delimiter(']', closing_span)?;
                        let span = TextRange::new(expr.span().start(), self.prev().span.end());

                        expr = Expr::Subscript(SubscriptExpr {
                            value: self.arena.alloc(expr),
                            slice: self.arena.alloc(slice),
                            span,
                        });
                    } else {
                        let closing_span = self.peek().span;
                        self.consume(TokenKind::RightBracket)?;
                        self.pop_delimiter(']', closing_span)?;
                        let span = TextRange::new(expr.span().start(), self.prev().span.end());

                        expr = Expr::Subscript(SubscriptExpr {
                            value: self.arena.alloc(expr),
                            slice: self.arena.alloc(first_elem),
                            span,
                        });
                    }
                }
                TokenKind::Dot => {
                    self.advance();
                    let attr = self.consume_ident()?;
                    let span = TextRange::new(expr.span().start(), self.prev().span.end());

                    expr = Expr::Attribute(AttributeExpr {
                        value: self.arena.alloc(expr),
                        attr,
                        span,
                    });
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    fn parse_primary(&mut self) -> ParseResult<Expr<'a>> {
        match self.peek().kind {
            TokenKind::Number => {
                let token = self.advance();
                let start = usize::from(token.span.start());
                let end = usize::from(token.span.end());
                let value = &self.source[start..end];
                Ok(Expr::Constant(ConstantExpr {
                    value: self.arena.alloc_str(value),
                    span: token.span,
                }))
            }
            TokenKind::Complex => {
                let token = self.advance();
                let start = usize::from(token.span.start());
                let end = usize::from(token.span.end());
                let value = &self.source[start..end];
                Ok(Expr::Complex(ComplexExpr {
                    value: self.arena.alloc_str(value),
                    span: token.span,
                }))
            }
            TokenKind::String | TokenKind::RawString => {
                let mut tokens = vec![self.advance()];

                loop {
                    while self.peek().kind == TokenKind::Newline {
                        self.advance();
                    }

                    if matches!(self.peek().kind, TokenKind::String | TokenKind::RawString) {
                        tokens.push(self.advance());
                    } else {
                        break;
                    }
                }

                let mut concatenated = String::new();
                for token in &tokens {
                    let start = usize::from(token.span.start());
                    let end = usize::from(token.span.end());
                    let text = &self.source[start..end];

                    let content = if text.starts_with("r\"\"\"") || text.starts_with("R\"\"\"") {
                        &text[4..text.len() - 3]
                    } else if text.starts_with("r'''") || text.starts_with("R'''") {
                        &text[4..text.len() - 3]
                    } else if text.starts_with("r\"") || text.starts_with("R\"") {
                        &text[2..text.len() - 1]
                    } else if text.starts_with("r'") || text.starts_with("R'") {
                        &text[2..text.len() - 1]
                    } else if text.starts_with("\"\"\"") || text.starts_with("'''") {
                        &text[3..text.len() - 3]
                    } else if text.starts_with('"') || text.starts_with('\'') {
                        &text[1..text.len() - 1]
                    } else {
                        text
                    };

                    concatenated.push_str(content);
                }

                let start = tokens[0].span.start();
                let end = tokens.last().unwrap().span.end();
                let span = TextRange::new(start, end);

                Ok(Expr::Constant(ConstantExpr {
                    value: self.arena.alloc_str(&concatenated),
                    span,
                }))
            }
            TokenKind::Bytes | TokenKind::RawBytes => {
                let token = self.advance();
                let start = usize::from(token.span.start());
                let end = usize::from(token.span.end());
                let value = &self.source[start..end];
                Ok(Expr::Bytes(BytesExpr {
                    value: self.arena.alloc_str(value),
                    span: token.span,
                }))
            }
            TokenKind::FString => self.parse_fstring(),
            TokenKind::RawFString => self.parse_raw_fstring(),
            TokenKind::TString => self.parse_tstring(),
            TokenKind::RawTString => self.parse_raw_tstring(),
            TokenKind::Ident => {
                let span = self.peek().span;
                let name = self.get_ident_text();

                if name == "module" && self.current + 2 < self.tokens.len() {
                    let next = &self.tokens[self.current + 1];
                    let next_next = &self.tokens[self.current + 2];

                    if next.kind == TokenKind::Colon && next_next.kind == TokenKind::Colon {
                        self.advance(); // consume 'module'
                        self.advance(); // consume first ':'
                        self.advance(); // consume second ':'

                        let function_name = self.consume_ident()?;

                        self.consume(TokenKind::LeftParen)?;
                        self.consume(TokenKind::RightParen)?;

                        let end = self.prev().span.end();
                        return Ok(Expr::ModuleIntrospection(ModuleIntrospectionExpr {
                            function: function_name,
                            span: TextRange::new(span.start(), end),
                        }));
                    }
                }

                let name = self.consume_ident()?;
                Ok(Expr::Name(NameExpr { id: name, span }))
            }
            TokenKind::LeftParen => {
                let start = self.peek().span.start();
                let opening_span = self.peek().span;
                self.push_delimiter('(', opening_span);
                self.advance();

                if self.peek().kind == TokenKind::RightParen {
                    let end = self.peek().span.end();
                    let closing_span = self.peek().span;
                    self.consume(TokenKind::RightParen)?;
                    self.pop_delimiter(')', closing_span)?;
                    return Ok(Expr::Tuple(TupleExpr {
                        elts: &[],
                        span: TextRange::new(start, end),
                    }));
                }

                let first_expr = self.parse_expression()?;

                if self.peek().kind == TokenKind::For || self.peek().kind == TokenKind::Async {
                    let generators = self.parse_comprehensions()?;
                    let end = self.peek().span.end();
                    let closing_span = self.peek().span;
                    self.consume(TokenKind::RightParen)?;
                    self.pop_delimiter(')', closing_span)?;
                    return Ok(Expr::GeneratorExp(GeneratorExpExpr {
                        elt: self.arena.alloc(first_expr),
                        generators,
                        span: TextRange::new(start, end),
                    }));
                }

                if !self.match_token(TokenKind::Comma) {
                    let closing_span = self.peek().span;
                    self.consume(TokenKind::RightParen)?;
                    self.pop_delimiter(')', closing_span)?;
                    return Ok(first_expr);
                }

                let mut elts = vec![first_expr];
                while self.peek().kind != TokenKind::RightParen && !self.is_at_end() {
                    elts.push(self.parse_expression()?);
                    if !self.match_token(TokenKind::Comma) {
                        break;
                    }
                }
                let end = self.peek().span.end();
                let closing_span = self.peek().span;
                self.consume(TokenKind::RightParen)?;
                self.pop_delimiter(')', closing_span)?;
                Ok(Expr::Tuple(TupleExpr {
                    elts: self.arena.alloc_slice_vec(elts),
                    span: TextRange::new(start, end),
                }))
            }
            TokenKind::LeftBracket => {
                let start = self.peek().span.start();
                let opening_span = self.peek().span;
                self.push_delimiter('[', opening_span);
                self.advance();

                if self.peek().kind == TokenKind::RightBracket {
                    let end = self.peek().span.end();
                    let closing_span = self.peek().span;
                    self.consume(TokenKind::RightBracket)?;
                    self.pop_delimiter(']', closing_span)?;
                    return Ok(Expr::List(ListExpr {
                        elts: &[],
                        span: TextRange::new(start, end),
                    }));
                }

                let first_expr = self.parse_expression()?;

                if self.peek().kind == TokenKind::For || self.peek().kind == TokenKind::Async {
                    let generators = self.parse_comprehensions()?;
                    let end = self.peek().span.end();
                    let closing_span = self.peek().span;
                    self.consume(TokenKind::RightBracket)?;
                    self.pop_delimiter(']', closing_span)?;
                    return Ok(Expr::ListComp(ListCompExpr {
                        elt: self.arena.alloc(first_expr),
                        generators,
                        span: TextRange::new(start, end),
                    }));
                }

                let mut elts = vec![first_expr];
                while self.match_token(TokenKind::Comma) {
                    if self.peek().kind == TokenKind::RightBracket {
                        break;
                    }
                    elts.push(self.parse_expression()?);
                }
                let end = self.peek().span.end();
                let closing_span = self.peek().span;
                self.consume(TokenKind::RightBracket)?;
                self.pop_delimiter(']', closing_span)?;
                Ok(Expr::List(ListExpr {
                    elts: self.arena.alloc_slice_vec(elts),
                    span: TextRange::new(start, end),
                }))
            }
            TokenKind::True => {
                let token = self.advance();
                Ok(Expr::Constant(ConstantExpr {
                    value: CONST_TRUE,
                    span: token.span,
                }))
            }
            TokenKind::False => {
                let token = self.advance();
                Ok(Expr::Constant(ConstantExpr {
                    value: CONST_FALSE,
                    span: token.span,
                }))
            }
            TokenKind::None => {
                let token = self.advance();
                Ok(Expr::Constant(ConstantExpr {
                    value: CONST_NONE,
                    span: token.span,
                }))
            }
            TokenKind::Ellipsis => {
                let token = self.advance();
                Ok(Expr::Constant(ConstantExpr {
                    value: self.arena.alloc_str("..."),
                    span: token.span,
                }))
            }
            TokenKind::Yield => self.parse_yield_expr(),
            TokenKind::Star => {
                let start = self.peek().span.start();
                self.advance();
                let value = self.arena.alloc(self.parse_primary()?);
                let span = TextRange::new(start, value.span().end());
                Ok(Expr::Starred(StarredExpr { value, span }))
            }
            TokenKind::LeftBrace => {
                let start = self.peek().span.start();
                let opening_span = self.peek().span;
                self.push_delimiter('{', opening_span);
                self.advance();

                if self.peek().kind == TokenKind::RightBrace {
                    let end = self.peek().span.end();
                    let closing_span = self.peek().span;
                    self.consume(TokenKind::RightBrace)?;
                    self.pop_delimiter('}', closing_span)?;
                    return Ok(Expr::Dict(DictExpr {
                        keys: &[],
                        values: &[],
                        span: TextRange::new(start, end),
                    }));
                }

                if self.peek().kind == TokenKind::DoubleStar {
                    let mut keys = ThinVec::new();
                    let mut values = ThinVec::new();

                    while self.peek().kind != TokenKind::RightBrace && !self.is_at_end() {
                        if self.peek().kind == TokenKind::DoubleStar {
                            self.advance(); // consume **
                            let value = self.parse_expression()?;
                            keys.push(None); // None key indicates ** unpacking
                            values.push(value);
                        } else {
                            let key = self.parse_expression()?;
                            self.consume(TokenKind::Colon)?;
                            let value = self.parse_expression()?;
                            keys.push(Some(key));
                            values.push(value);
                        }

                        if !self.match_token(TokenKind::Comma) {
                            break;
                        }
                    }

                    let end = self.peek().span.end();
                    let closing_span = self.peek().span;
                    self.consume(TokenKind::RightBrace)?;
                    self.pop_delimiter('}', closing_span)?;
                    return Ok(Expr::Dict(DictExpr {
                        keys: self.arena.alloc_slice_iter(keys),
                        values: self.arena.alloc_slice_iter(values),
                        span: TextRange::new(start, end),
                    }));
                }

                let first_expr = self.parse_expression()?;

                if self.peek().kind == TokenKind::Colon {
                    self.advance();
                    let first_value = self.parse_expression()?;

                    if self.peek().kind == TokenKind::For || self.peek().kind == TokenKind::Async {
                        let generators = self.parse_comprehensions()?;
                        let end = self.peek().span.end();
                        let closing_span = self.peek().span;
                        self.consume(TokenKind::RightBrace)?;
                        self.pop_delimiter('}', closing_span)?;
                        return Ok(Expr::DictComp(DictCompExpr {
                            key: self.arena.alloc(first_expr),
                            value: self.arena.alloc(first_value),
                            generators,
                            span: TextRange::new(start, end),
                        }));
                    }

                    let mut keys = vec![Some(first_expr)];
                    let mut values = vec![first_value];

                    while self.match_token(TokenKind::Comma) {
                        if self.peek().kind == TokenKind::RightBrace {
                            break;
                        }

                        if self.peek().kind == TokenKind::DoubleStar {
                            self.advance(); // consume **
                            let value = self.parse_expression()?;
                            keys.push(None); // None key indicates ** unpacking
                            values.push(value);
                        } else {
                            let key = self.parse_expression()?;
                            self.consume(TokenKind::Colon)?;
                            let value = self.parse_expression()?;
                            keys.push(Some(key));
                            values.push(value);
                        }
                    }

                    let end = self.peek().span.end();
                    let closing_span = self.peek().span;
                    self.consume(TokenKind::RightBrace)?;
                    self.pop_delimiter('}', closing_span)?;
                    return Ok(Expr::Dict(DictExpr {
                        keys: self.arena.alloc_slice_vec(keys),
                        values: self.arena.alloc_slice_iter(values),
                        span: TextRange::new(start, end),
                    }));
                }

                if self.peek().kind == TokenKind::For || self.peek().kind == TokenKind::Async {
                    let generators = self.parse_comprehensions()?;
                    let end = self.peek().span.end();
                    let closing_span = self.peek().span;
                    self.consume(TokenKind::RightBrace)?;
                    self.pop_delimiter('}', closing_span)?;
                    return Ok(Expr::SetComp(SetCompExpr {
                        elt: self.arena.alloc(first_expr),
                        generators,
                        span: TextRange::new(start, end),
                    }));
                }

                let mut elts = vec![first_expr];
                while self.match_token(TokenKind::Comma) {
                    if self.peek().kind == TokenKind::RightBrace {
                        break;
                    }
                    elts.push(self.parse_expression()?);
                }

                let end = self.peek().span.end();
                let closing_span = self.peek().span;
                self.consume(TokenKind::RightBrace)?;
                self.pop_delimiter('}', closing_span)?;
                Ok(Expr::Set(SetExpr {
                    elts: self.arena.alloc_slice_vec(elts),
                    span: TextRange::new(start, end),
                }))
            }
            _ => Err(error(
                ErrorKind::UnexpectedToken {
                    expected: None,
                    found: format!("{:?}", self.peek().kind),
                },
                self.peek().span,
            )),
        }
    }

    fn parse_arguments_call(&mut self) -> ParseResult<(ThinVec<Expr<'a>>, ThinVec<Keyword<'a>>)> {
        let mut args = ThinVec::new();
        let mut keywords = ThinVec::new();
        let mut seen_keyword = false;
        let mut seen_keyword_names = std::collections::HashSet::new();

        while self.peek().kind != TokenKind::RightParen && !self.is_at_end() {
            if self.peek().kind == TokenKind::DoubleStar {
                self.advance(); // consume **
                let value = self.parse_expression()?;
                seen_keyword = true;
                keywords.push(Keyword {
                    arg: None, // **kwargs has no argument name
                    value,
                });
            } else if self.peek().kind == TokenKind::Star {
                self.advance(); // consume *
                let value = self.parse_expression()?;
                let span = TextRange::new(self.prev().span.start(), value.span().end());
                args.push(Expr::Starred(StarredExpr {
                    value: self.arena.alloc(value),
                    span,
                }));
            } else if self.peek().kind == TokenKind::Ident {
                let checkpoint_pos = self.current;
                let name = self.consume_ident()?;

                if self.peek().kind == TokenKind::Equal {
                    seen_keyword = true;
                    self.advance(); // consume =

                    if !seen_keyword_names.insert(name) {
                        return Err(error(
                            ErrorKind::DuplicateArgument {
                                name: name.to_string(),
                            },
                            self.prev().span,
                        ));
                    }

                    let value = self.parse_expression()?;
                    keywords.push(Keyword {
                        arg: Some(name),
                        value,
                    });
                } else {
                    self.current = checkpoint_pos;

                    if seen_keyword {
                        return Err(error(ErrorKind::PositionalAfterKeyword, self.peek().span));
                    }

                    args.push(self.parse_expression()?);
                }
            } else {
                if seen_keyword {
                    return Err(error(ErrorKind::PositionalAfterKeyword, self.peek().span));
                }

                args.push(self.parse_expression()?);
            }

            if !self.match_token(TokenKind::Comma) {
                break;
            }
        }

        Ok((args, keywords))
    }

    #[allow(dead_code)]
    #[allow(dead_code)]
    fn parse_slice_or_index(&mut self) -> ParseResult<Expr<'a>> {
        let start = self.peek().span.start();

        if self.peek().kind == TokenKind::Colon {
            return self.parse_slice_from_colon(start, None);
        }

        let first_expr = self.parse_expression()?;

        if self.peek().kind != TokenKind::Colon {
            return Ok(first_expr);
        }

        self.parse_slice_from_colon(start, Some(first_expr))
    }

    #[allow(dead_code)]
    #[allow(dead_code)]
    fn parse_slice_from_colon(
        &mut self,
        start: text_size::TextSize,
        lower: Option<Expr<'a>>,
    ) -> ParseResult<Expr<'a>> {
        self.consume(TokenKind::Colon)?;

        let upper = if self.peek().kind != TokenKind::Colon
            && self.peek().kind != TokenKind::RightBracket
        {
            Some(self.parse_expression()?)
        } else {
            None
        };

        let step = if self.match_token(TokenKind::Colon) {
            if self.peek().kind != TokenKind::RightBracket {
                Some(self.parse_expression()?)
            } else {
                None
            }
        } else {
            None
        };

        let end = self.peek().span.end();
        let span = TextRange::new(start, end);

        Ok(Expr::Slice(SliceExpr {
            lower: lower.map(|e| self.arena.alloc(e)),
            upper: upper.map(|e| self.arena.alloc(e)),
            step: step.map(|e| self.arena.alloc(e)),
            span,
        }))
    }

    /// Parse an f-string into a JoinedStr with properly decomposed expressions.
    /// Handles: basic interpolation, conversions (!s/!r/!a), format specs, escaping, nesting
    /// Supports: f"...", f'...', f"""...""", f'''...'''
    fn parse_fstring(&mut self) -> ParseResult<Expr<'a>> {
        let token = self.advance();
        let span = token.span;
        let start = usize::from(span.start());
        let end = usize::from(span.end());
        let text = &self.source[start..end];

        let content = if text.starts_with("f\"\"\"") || text.starts_with("F\"\"\"") {
            &text[4..text.len() - 3] // Triple-quoted double
        } else if text.starts_with("f'''") || text.starts_with("F'''") {
            &text[4..text.len() - 3] // Triple-quoted single
        } else if text.starts_with("f\"") || text.starts_with("F\"") {
            &text[2..text.len() - 1] // Regular double
        } else if text.starts_with("f'") || text.starts_with("F'") {
            &text[2..text.len() - 1] // Regular single
        } else {
            return Err(error(
                ErrorKind::InvalidSyntax {
                    message: "Invalid f-string prefix".to_string(),
                },
                span,
            ));
        };

        let values = self.parse_fstring_content(content, span, 0)?;
        let values_slice = self.arena.alloc_slice_iter(values);

        Ok(Expr::JoinedStr(JoinedStrExpr {
            values: values_slice,
            span,
        }))
    }

    /// Parse a raw f-string (rf"..." or fr"...")
    /// Raw f-strings treat backslashes literally but still allow interpolation
    fn parse_raw_fstring(&mut self) -> ParseResult<Expr<'a>> {
        let token = self.advance();
        let span = token.span;
        let start = usize::from(span.start());
        let end = usize::from(span.end());
        let text = &self.source[start..end];

        let content = if text.starts_with("rf\"\"\"")
            || text.starts_with("Rf\"\"\"")
            || text.starts_with("rF\"\"\"")
            || text.starts_with("RF\"\"\"")
            || text.starts_with("fr\"\"\"")
            || text.starts_with("Fr\"\"\"")
            || text.starts_with("fR\"\"\"")
            || text.starts_with("FR\"\"\"")
        {
            &text[5..text.len() - 3] // Triple-quoted double
        } else if text.starts_with("rf'''")
            || text.starts_with("Rf'''")
            || text.starts_with("rF'''")
            || text.starts_with("RF'''")
            || text.starts_with("fr'''")
            || text.starts_with("Fr'''")
            || text.starts_with("fR'''")
            || text.starts_with("FR'''")
        {
            &text[5..text.len() - 3] // Triple-quoted single
        } else if text.starts_with("rf\"")
            || text.starts_with("Rf\"")
            || text.starts_with("rF\"")
            || text.starts_with("RF\"")
            || text.starts_with("fr\"")
            || text.starts_with("Fr\"")
            || text.starts_with("fR\"")
            || text.starts_with("FR\"")
        {
            &text[3..text.len() - 1] // Regular double
        } else if text.starts_with("rf'")
            || text.starts_with("Rf'")
            || text.starts_with("rF'")
            || text.starts_with("RF'")
            || text.starts_with("fr'")
            || text.starts_with("Fr'")
            || text.starts_with("fR'")
            || text.starts_with("FR'")
        {
            &text[3..text.len() - 1] // Regular single
        } else {
            return Err(error(
                ErrorKind::InvalidSyntax {
                    message: "Invalid raw f-string prefix".to_string(),
                },
                span,
            ));
        };

        let values = self.parse_fstring_content(content, span, 0)?;
        let values_slice = self.arena.alloc_slice_iter(values);

        Ok(Expr::JoinedStr(JoinedStrExpr {
            values: values_slice,
            span,
        }))
    }

    /// Parse the content of an f-string, returning a mix of Constant and FormattedValue nodes
    /// depth: tracks nesting level of f-strings (for validation)
    fn parse_fstring_content(
        &mut self,
        content: &str,
        full_span: TextRange,
        depth: u32,
    ) -> ParseResult<ThinVec<Expr<'a>>> {
        const MAX_NESTING_DEPTH: u32 = 10;
        if depth > MAX_NESTING_DEPTH {
            return Err(error(
                ErrorKind::InvalidSyntax {
                    message: format!(
                        "F-string nesting depth exceeds maximum of {}",
                        MAX_NESTING_DEPTH
                    ),
                },
                full_span,
            ));
        }

        let mut values = ThinVec::new();
        let mut chars = content.chars().peekable();
        let mut literal = String::new();

        while let Some(ch) = chars.next() {
            match ch {
                '{' => {
                    if chars.peek() == Some(&'{') {
                        chars.next();
                        literal.push('{');
                        continue;
                    }

                    if !literal.is_empty() {
                        let lit_str = self.arena.alloc_str(&literal);
                        values.push(Expr::Constant(ConstantExpr {
                            value: lit_str,
                            span: full_span,
                        }));
                        literal.clear();
                    }

                    let (expr_str, conversion, format_spec) =
                        self.extract_fstring_expr(&mut chars, full_span)?;

                    if expr_str.trim().is_empty() {
                        return Err(error(
                            ErrorKind::InvalidSyntax {
                                message: "Empty expression in f-string (expressions inside '{}' cannot be empty)".to_string(),
                            },
                            full_span,
                        ));
                    }

                    let value_expr = self
                        .arena
                        .alloc(self.parse_fstring_expression(&expr_str, full_span)?);

                    let format_spec_expr = if let Some(spec) = format_spec {
                        let spec_values =
                            self.parse_fstring_content(&spec, full_span, depth + 1)?;
                        let spec_slice = self.arena.alloc_slice_iter(spec_values);
                        Some(self.arena.alloc(JoinedStrExpr {
                            values: spec_slice,
                            span: full_span,
                        }))
                    } else {
                        None
                    };

                    values.push(Expr::FormattedValue(FormattedValueExpr {
                        value: value_expr,
                        conversion,
                        format_spec: format_spec_expr,
                        span: full_span,
                    }));
                }
                '}' => {
                    if chars.peek() == Some(&'}') {
                        chars.next();
                        literal.push('}');
                        continue;
                    }

                    return Err(error(
                        ErrorKind::InvalidSyntax {
                            message: "Unmatched '}' in f-string".to_string(),
                        },
                        full_span,
                    ));
                }
                _ => {
                    literal.push(ch);
                }
            }
        }

        if !literal.is_empty() {
            let lit_str = self.arena.alloc_str(&literal);
            values.push(Expr::Constant(ConstantExpr {
                value: lit_str,
                span: full_span,
            }));
        }

        Ok(values)
    }

    /// Extract an f-string expression, including conversion and format spec
    /// Returns: (expression, conversion, format_spec)
    fn extract_fstring_expr(
        &mut self,
        chars: &mut std::iter::Peekable<std::str::Chars>,
        span: TextRange,
    ) -> ParseResult<(String, Option<char>, Option<String>)> {
        let mut expr = String::new();
        let mut depth = 0; // Track nesting of (), [], {}
        let mut in_string = false;
        let mut string_char = '\0';

        while let Some(&ch) = chars.peek() {
            if in_string {
                expr.push(ch);
                chars.next();
                if ch == string_char && !expr.ends_with('\\') {
                    in_string = false;
                }
                continue;
            }

            match ch {
                '\'' | '"' => {
                    in_string = true;
                    string_char = ch;
                    expr.push(ch);
                    chars.next();
                }
                '(' | '[' | '{' => {
                    depth += 1;
                    expr.push(ch);
                    chars.next();
                }
                ')' | ']' => {
                    if depth > 0 {
                        depth -= 1;
                        expr.push(ch);
                        chars.next();
                    } else {
                        break;
                    }
                }
                '}' => {
                    if depth > 0 {
                        depth -= 1;
                        expr.push(ch);
                        chars.next();
                    } else {
                        chars.next();
                        return Ok((expr, None, None));
                    }
                }
                '!' if depth == 0 => {
                    let mut chars_clone = chars.clone();
                    chars_clone.next(); // skip '!'
                    if matches!(chars_clone.peek(), Some(&'s') | Some(&'r') | Some(&'a')) {
                        chars.next(); // consume '!'
                        let conversion = chars.next();

                        match conversion {
                            Some('s') | Some('r') | Some('a') => {
                                if chars.peek() == Some(&':') {
                                    chars.next(); // consume ':'
                                    let format_spec = self.extract_format_spec(chars)?;
                                    return Ok((expr, conversion, Some(format_spec)));
                                } else if chars.peek() == Some(&'}') {
                                    chars.next(); // consume '}'
                                    return Ok((expr, conversion, None));
                                } else {
                                    return Ok((expr, conversion, None));
                                }
                            }
                            Some(_c) => {
                                return Err(error(
                                    ErrorKind::InvalidSyntax {
                                        message: "Invalid f-string format spec".to_string(),
                                    },
                                    span,
                                ));
                            }
                            None => {
                                return Err(error(
                                    ErrorKind::InvalidSyntax {
                                        message: "Unexpected end in f-string".to_string(),
                                    },
                                    span,
                                ));
                            }
                        }
                    } else {
                        expr.push(ch);
                        chars.next();
                    }
                }
                ':' if depth == 0 => {
                    chars.next(); // consume ':'
                    let format_spec = self.extract_format_spec(chars)?;
                    return Ok((expr, None, Some(format_spec)));
                }
                _ => {
                    expr.push(ch);
                    chars.next();
                }
            }
        }

        Err(error(
            ErrorKind::InvalidSyntax {
                message: "Unclosed '{' in f-string - missing closing '}'".to_string(),
            },
            span,
        ))
    }

    /// Extract format spec (everything after : until })
    /// Also validates the format spec for common format codes
    fn extract_format_spec(
        &mut self,
        chars: &mut std::iter::Peekable<std::str::Chars>,
    ) -> ParseResult<String> {
        let mut spec = String::new();
        let mut depth = 0;

        while let Some(&ch) = chars.peek() {
            match ch {
                '{' => {
                    depth += 1;
                    spec.push(ch);
                    chars.next();
                }
                '}' => {
                    if depth > 0 {
                        depth -= 1;
                        spec.push(ch);
                        chars.next();
                    } else {
                        chars.next();

                        if depth == 0 && !spec.contains('{') {
                            self.validate_format_spec(&spec)?;
                        }
                        return Ok(spec);
                    }
                }
                _ => {
                    spec.push(ch);
                    chars.next();
                }
            }
        }

        if depth == 0 && !spec.contains('{') {
            self.validate_format_spec(&spec)?;
        }
        Ok(spec)
    }

    /// Validate format spec for common format codes
    /// Format spec grammar (simplified): [[fill]align][sign][#][0][width][grouping_option][.precision][type]
    fn validate_format_spec(&self, spec: &str) -> ParseResult<()> {
        if spec.is_empty() {
            return Ok(());
        }

        if spec.contains('{') {
            return Ok(());
        }

        let chars: Vec<char> = spec.chars().collect();
        let mut pos = 0;

        let is_type_code = |c: char| -> bool {
            matches!(
                c,
                'b' | 'c'
                    | 'd'
                    | 'e'
                    | 'E'
                    | 'f'
                    | 'F'
                    | 'g'
                    | 'G'
                    | 'n'
                    | 'o'
                    | 's'
                    | 'x'
                    | 'X'
                    | '%'
            )
        };

        if pos + 1 < chars.len() && matches!(chars[pos + 1], '<' | '>' | '^' | '=') {
            pos += 2;
        } else if pos < chars.len() && matches!(chars[pos], '<' | '>' | '^' | '=') {
            pos += 1;
        }

        if pos < chars.len() && matches!(chars[pos], '+' | '-' | ' ') {
            pos += 1;
        }

        if pos < chars.len() && chars[pos] == '#' {
            pos += 1;
        }

        if pos < chars.len() && chars[pos] == '0' {
            pos += 1;
        }

        while pos < chars.len() && (chars[pos].is_ascii_digit() || chars[pos] == '*') {
            pos += 1;
        }

        if pos < chars.len() && matches!(chars[pos], ',' | '_') {
            pos += 1;
        }

        if pos < chars.len() && chars[pos] == '.' {
            pos += 1;
            while pos < chars.len() && (chars[pos].is_ascii_digit() || chars[pos] == '*') {
                pos += 1;
            }
        }

        if pos < chars.len() && is_type_code(chars[pos]) {
            pos += 1;
        }

        let _ = pos; // Suppress unused warning
        Ok(())
    }

    /// Parse an expression string from inside an f-string.
    /// Creates a temporary lexer and parser for the expression.
    fn parse_fstring_expression(
        &mut self,
        expr_str: &str,
        span: TextRange,
    ) -> ParseResult<Expr<'a>> {
        let mut lexer = crate::Lexer::new(expr_str);
        let (tokens, _lexical_errors, _lexical_warnings) = lexer.tokenize();

        if tokens.is_empty() || (tokens.len() == 1 && tokens[0].kind == TokenKind::Eof) {
            return Err(error(
                ErrorKind::InvalidSyntax {
                    message: "Empty expression in f-string".to_string(),
                },
                span,
            ));
        }

        let expr_source = self.arena.alloc_str(expr_str);

        let saved_tokens = std::mem::replace(&mut self.tokens, tokens);
        let saved_current = std::mem::replace(&mut self.current, 0);
        let saved_source = std::mem::replace(&mut self.source, expr_source);

        let result = self.parse_expression();

        self.tokens = saved_tokens;
        self.current = saved_current;
        self.source = saved_source;

        result
    }

    fn parse_yield_expr(&mut self) -> ParseResult<Expr<'a>> {
        let start = self.peek().span.start();
        self.consume(TokenKind::Yield)?;

        if self.match_token(TokenKind::From) {
            let value = self.parse_expression()?;
            let end = value.span().end();
            Ok(Expr::YieldFrom(YieldFromExpr {
                value: self.arena.alloc(value),
                span: TextRange::new(start, end),
            }))
        } else {
            let value = if self.peek().kind != TokenKind::Newline && !self.is_at_end() {
                Some(self.arena.alloc(self.parse_expression()?))
            } else {
                None
            };

            let end = value
                .as_ref()
                .map(|e| e.span().end())
                .unwrap_or(self.prev().span.end());

            Ok(Expr::Yield(YieldExpr {
                value,
                span: TextRange::new(start, end),
            }))
        }
    }

    /// Parse a slice element which can be either an index expression or a slice notation.
    /// Examples: `1`, `1:5`, `:5`, `1:`, `::2`, `1:5:2`
    fn parse_slice_element(&mut self) -> ParseResult<Expr<'a>> {
        let start = self.peek().span.start();

        if self.peek().kind == TokenKind::Colon {
            self.advance(); // consume ':'

            let upper = if self.peek().kind != TokenKind::Colon
                && self.peek().kind != TokenKind::RightBracket
                && self.peek().kind != TokenKind::Comma
            {
                Some(self.arena.alloc(self.parse_expression()?))
            } else {
                None
            };

            let step = if self.match_token(TokenKind::Colon) {
                if self.peek().kind != TokenKind::RightBracket
                    && self.peek().kind != TokenKind::Comma
                {
                    Some(self.arena.alloc(self.parse_expression()?))
                } else {
                    None
                }
            } else {
                None
            };

            let end = step
                .as_ref()
                .map(|s| s.span().end())
                .or_else(|| upper.as_ref().map(|u| u.span().end()))
                .unwrap_or(start);

            Ok(Expr::Slice(SliceExpr {
                lower: None,
                upper,
                step,
                span: TextRange::new(start, end),
            }))
        } else {
            let first_expr = self.parse_expression()?;

            if self.peek().kind == TokenKind::Colon {
                self.advance(); // consume ':'

                let upper = if self.peek().kind != TokenKind::Colon
                    && self.peek().kind != TokenKind::RightBracket
                    && self.peek().kind != TokenKind::Comma
                {
                    Some(self.arena.alloc(self.parse_expression()?))
                } else {
                    None
                };

                let step = if self.match_token(TokenKind::Colon) {
                    if self.peek().kind != TokenKind::RightBracket
                        && self.peek().kind != TokenKind::Comma
                    {
                        Some(self.arena.alloc(self.parse_expression()?))
                    } else {
                        None
                    }
                } else {
                    None
                };

                let end = step
                    .as_ref()
                    .map(|s| s.span().end())
                    .or_else(|| upper.as_ref().map(|u| u.span().end()))
                    .unwrap_or(first_expr.span().end());

                Ok(Expr::Slice(SliceExpr {
                    lower: Some(self.arena.alloc(first_expr)),
                    upper,
                    step,
                    span: TextRange::new(start, end),
                }))
            } else {
                Ok(first_expr)
            }
        }
    }

    /// Parse a t-string (template string with delayed interpolation)
    /// Unlike f-strings which eagerly evaluate, t-strings are compiled to template functions
    /// for delayed evaluation. This enables SQL-safe escaping, HTML templating, etc.
    fn parse_tstring(&mut self) -> ParseResult<Expr<'a>> {
        let token = self.advance();
        let span = token.span;
        let start = usize::from(span.start());
        let end = usize::from(span.end());
        let text = &self.source[start..end];

        let content = if text.starts_with("t\"") || text.starts_with("T\"") {
            &text[2..text.len() - 1]
        } else if text.starts_with("t'") || text.starts_with("T'") {
            &text[2..text.len() - 1]
        } else {
            return Err(error(
                ErrorKind::InvalidSyntax {
                    message: "Invalid t-string prefix".to_string(),
                },
                span,
            ));
        };

        let values = self.parse_fstring_content(content, span, 0)?;
        let values_slice = self.arena.alloc_slice_iter(values);

        Ok(Expr::TString(TStringExpr {
            values: values_slice,
            span,
        }))
    }

    /// Parse a raw t-string (rt"..." or tr"...")
    /// Raw t-strings disable escape sequence processing but still support interpolation.
    /// Like regular t-strings, they are compiled to template functions for delayed evaluation.
    fn parse_raw_tstring(&mut self) -> ParseResult<Expr<'a>> {
        let token = self.advance();
        let span = token.span;
        let start = usize::from(span.start());
        let end = usize::from(span.end());
        let text = &self.source[start..end];

        let content = if text.len() >= 4 {
            let prefix_end = if text.starts_with("rt\"")
                || text.starts_with("tr\"")
                || text.starts_with("RT\"")
                || text.starts_with("TR\"")
                || text.starts_with("rT\"")
                || text.starts_with("tR\"")
                || text.starts_with("Rt\"")
                || text.starts_with("Tr\"")
            {
                3
            } else if text.starts_with("rt'")
                || text.starts_with("tr'")
                || text.starts_with("RT'")
                || text.starts_with("TR'")
                || text.starts_with("rT'")
                || text.starts_with("tR'")
                || text.starts_with("Rt'")
                || text.starts_with("Tr'")
            {
                3
            } else {
                return Err(error(
                    ErrorKind::InvalidSyntax {
                        message: "Invalid raw t-string prefix".to_string(),
                    },
                    span,
                ));
            };
            &text[prefix_end..text.len() - 1]
        } else {
            return Err(error(
                ErrorKind::InvalidSyntax {
                    message: "Invalid t-string format".to_string(),
                },
                span,
            ));
        };

        let values = self.parse_fstring_content(content, span, 0)?;
        let values_slice = self.arena.alloc_slice_iter(values);

        Ok(Expr::TString(TStringExpr {
            values: values_slice,
            span,
        }))
    }
}
