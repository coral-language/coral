use super::types::Parser;
use crate::ast::*;
use crate::error::{UnifiedError as Error, UnifiedErrorKind as ErrorKind, error};
use crate::lexer::TokenKind;
use text_size::TextRange;

pub type ParseResult<T> = Result<T, Box<Error>>;

impl<'a> Parser<'a> {
    pub(super) fn parse_stmt(&mut self) -> ParseResult<Stmt<'a>> {
        let result = match self.peek().kind {
            TokenKind::Comment => {
                return Err(error(
                    ErrorKind::UnexpectedToken {
                        expected: None,
                        found: "comment".to_string(),
                    },
                    self.peek().span,
                ));
            }
            TokenKind::At => self.parse_decorated(),
            TokenKind::Async => self.parse_async_stmt(),
            TokenKind::Def => self.parse_func_def(&[], false),
            TokenKind::Class => self.parse_class_def(&[]),
            TokenKind::Protocol => self.parse_class_def(&[]),
            TokenKind::If => self.parse_if_stmt(),
            TokenKind::While => self.parse_while_stmt(),
            TokenKind::For => self.parse_for_stmt(false),
            TokenKind::Return => self.parse_return_stmt(),
            TokenKind::Import => return self.parse_import_stmt(), // Handles flag itself
            TokenKind::From => return self.parse_from_stmt(),     // Handles flag itself
            TokenKind::Export => self.parse_export_stmt(),
            TokenKind::Try => self.parse_try_stmt(),
            TokenKind::With => self.parse_with_stmt(false),
            TokenKind::Raise => self.parse_raise_stmt(),
            TokenKind::Assert => self.parse_assert_stmt(),
            TokenKind::Del => self.parse_delete_stmt(),
            TokenKind::Global => self.parse_global_stmt(),
            TokenKind::Nonlocal => self.parse_nonlocal_stmt(),
            TokenKind::Pass => {
                let span = self.peek().span;
                self.advance();
                self.consume_newline();
                Ok(Stmt::Pass(span))
            }
            TokenKind::Break => {
                let span = self.peek().span;

                if !self.context.in_loop {
                    return Err(error(ErrorKind::BreakOutsideLoop, span));
                }
                self.advance();
                self.consume_newline();
                Ok(Stmt::Break(span))
            }
            TokenKind::Continue => {
                let span = self.peek().span;

                if !self.context.in_loop {
                    return Err(error(ErrorKind::ContinueOutsideLoop, span));
                }
                self.advance();
                self.consume_newline();
                Ok(Stmt::Continue(span))
            }
            TokenKind::Yield => self.parse_yield_stmt(),
            TokenKind::Ident => {
                let ident_text = self.get_ident_text();
                match ident_text {
                    "match" => {
                        if self.current + 1 < self.tokens.len() {
                            let next_token = &self.tokens[self.current + 1];
                            if matches!(
                                next_token.kind,
                                TokenKind::Equal
                                    | TokenKind::PlusEqual
                                    | TokenKind::MinusEqual
                                    | TokenKind::StarEqual
                                    | TokenKind::SlashEqual
                                    | TokenKind::DoubleSlashEqual
                                    | TokenKind::PercentEqual
                                    | TokenKind::DoubleStarEqual
                                    | TokenKind::AmpersandEqual
                                    | TokenKind::PipeEqual
                                    | TokenKind::CaretEqual
                                    | TokenKind::LeftShiftEqual
                                    | TokenKind::RightShiftEqual
                                    | TokenKind::AtEqual
                                    | TokenKind::ColonEqual
                            ) {
                                self.parse_simple_stmt()
                            } else {
                                self.parse_match_stmt()
                            }
                        } else {
                            self.parse_match_stmt()
                        }
                    }
                    "type" => {
                        if self.current + 1 < self.tokens.len() {
                            let next_token = &self.tokens[self.current + 1];
                            if matches!(
                                next_token.kind,
                                TokenKind::Equal
                                    | TokenKind::PlusEqual
                                    | TokenKind::MinusEqual
                                    | TokenKind::StarEqual
                                    | TokenKind::SlashEqual
                                    | TokenKind::DoubleSlashEqual
                                    | TokenKind::PercentEqual
                                    | TokenKind::DoubleStarEqual
                                    | TokenKind::AmpersandEqual
                                    | TokenKind::PipeEqual
                                    | TokenKind::CaretEqual
                                    | TokenKind::LeftShiftEqual
                                    | TokenKind::RightShiftEqual
                                    | TokenKind::AtEqual
                                    | TokenKind::ColonEqual
                            ) {
                                self.parse_simple_stmt()
                            } else {
                                self.parse_type_alias_stmt()
                            }
                        } else {
                            self.parse_type_alias_stmt()
                        }
                    }
                    _ => self.parse_simple_stmt(),
                }
            }
            _ => self.parse_simple_stmt(),
        };

        if result.is_ok() {
            self.seen_non_future_statement = true;
        }

        result
    }

    fn parse_assignment_target(&mut self) -> ParseResult<Expr<'a>> {
        if self.peek().kind == TokenKind::Star {
            let start = self.peek().span.start();
            self.advance();

            let target = if self.peek().kind == TokenKind::Ident {
                let span = self.peek().span;
                let name = self.consume_ident()?;
                Expr::Name(NameExpr { id: name, span })
            } else {
                return Err(error(
                    ErrorKind::UnexpectedToken {
                        expected: None,
                        found: format!("{:?}", self.peek().kind),
                    },
                    self.peek().span,
                ));
            };
            let value = self.arena.alloc(target);
            let span = TextRange::new(start, value.span().end());
            Ok(Expr::Starred(StarredExpr { value, span }))
        } else {
            self.parse_expression()
        }
    }

    pub(super) fn parse_simple_stmt(&mut self) -> ParseResult<Stmt<'a>> {
        let expr = self.parse_expression()?;

        match self.peek().kind {
            TokenKind::Colon => {
                let is_valid_target = matches!(
                    expr,
                    Expr::Name(_) | Expr::Attribute(_) | Expr::Subscript(_)
                );

                if is_valid_target {
                    self.advance();
                    let annotation = self.parse_expression()?;

                    let value = if self.match_token(TokenKind::Equal) {
                        Some(self.parse_expression()?)
                    } else {
                        None
                    };

                    let span = TextRange::new(
                        expr.span().start(),
                        value
                            .as_ref()
                            .map(|v| v.span().end())
                            .unwrap_or(annotation.span().end()),
                    );
                    self.consume_newline();

                    Ok(Stmt::AnnAssign(AnnAssignStmt {
                        target: expr,
                        annotation,
                        value,
                        span,
                    }))
                } else {
                    self.consume_newline();
                    Ok(Stmt::Expr(ExprStmt {
                        value: expr.clone(),
                        span: expr.span(),
                    }))
                }
            }
            TokenKind::Comma => {
                let start = expr.span().start();
                let mut targets = vec![expr];

                while self.match_token(TokenKind::Comma) {
                    targets.push(self.parse_assignment_target()?);
                }

                let end = targets.last().map(|e| e.span().end()).unwrap_or(start);

                self.consume(TokenKind::Equal)?;

                let first_value = self.parse_expression()?;
                let value = if self.peek().kind == TokenKind::Comma {
                    let mut values = vec![first_value.clone()];
                    while self.match_token(TokenKind::Comma) {
                        values.push(self.parse_expression()?);
                    }
                    let end = values
                        .last()
                        .map(|e| e.span().end())
                        .unwrap_or(first_value.span().start());
                    Expr::Tuple(TupleExpr {
                        elts: self.arena.alloc_slice_vec(values),
                        span: TextRange::new(first_value.span().start(), end),
                    })
                } else {
                    first_value
                };
                let tuple_target = Expr::Tuple(TupleExpr {
                    elts: self.arena.alloc_slice_vec(targets),
                    span: TextRange::new(start, end),
                });

                let targets_slice = self.arena.alloc_slice_vec(vec![tuple_target]);
                let span = TextRange::new(start, value.span().end());
                self.consume_newline();
                Ok(Stmt::Assign(AssignStmt {
                    targets: targets_slice,
                    value,
                    span,
                }))
            }
            TokenKind::Equal => {
                let target = expr;

                let mut targets = vec![target];
                while self.match_token(TokenKind::Equal) {
                    targets.push(self.parse_expression()?);
                }
                let value = targets.pop().unwrap();
                let targets_slice = self.arena.alloc_slice_vec(targets);
                let span = TextRange::new(targets_slice[0].span().start(), value.span().end());
                self.consume_newline();
                Ok(Stmt::Assign(AssignStmt {
                    targets: targets_slice,
                    value,
                    span,
                }))
            }
            TokenKind::PlusEqual
            | TokenKind::MinusEqual
            | TokenKind::StarEqual
            | TokenKind::SlashEqual
            | TokenKind::DoubleSlashEqual
            | TokenKind::PercentEqual
            | TokenKind::DoubleStarEqual
            | TokenKind::AmpersandEqual
            | TokenKind::PipeEqual
            | TokenKind::CaretEqual
            | TokenKind::LeftShiftEqual
            | TokenKind::RightShiftEqual
            | TokenKind::AtEqual => {
                let op = self.peek().kind;
                let op_str = match op {
                    TokenKind::PlusEqual => "+=",
                    TokenKind::MinusEqual => "-=",
                    TokenKind::StarEqual => "*=",
                    TokenKind::SlashEqual => "/=",
                    TokenKind::DoubleSlashEqual => "//=",
                    TokenKind::PercentEqual => "%=",
                    TokenKind::DoubleStarEqual => "**=",
                    TokenKind::AmpersandEqual => "&=",
                    TokenKind::PipeEqual => "|=",
                    TokenKind::CaretEqual => "^=",
                    TokenKind::LeftShiftEqual => "<<=",
                    TokenKind::RightShiftEqual => ">>=",
                    TokenKind::AtEqual => "@=",
                    _ => unreachable!(),
                };
                self.advance();
                let value = self.parse_expression()?;
                let span = TextRange::new(expr.span().start(), value.span().end());
                self.consume_newline();
                Ok(Stmt::AugAssign(AugAssignStmt {
                    target: expr,
                    op: self.arena.alloc_str(op_str),
                    value,
                    span,
                }))
            }
            _ => {
                self.consume_newline();
                Ok(Stmt::Expr(ExprStmt {
                    value: expr.clone(),
                    span: expr.span(),
                }))
            }
        }
    }

    pub(super) fn parse_return_stmt(&mut self) -> ParseResult<Stmt<'a>> {
        let start = self.peek().span.start();
        let return_token = self.peek().span;
        self.consume(TokenKind::Return)?;

        if !self.context.in_function {
            return Err(error(ErrorKind::ReturnOutsideFunction, return_token));
        }

        let value = if self.peek().kind != TokenKind::Newline && !self.is_at_end() {
            Some(self.parse_expression()?)
        } else {
            None
        };

        let end = value
            .as_ref()
            .map(|e| e.span().end())
            .unwrap_or(return_token.end());
        self.consume_newline();
        Ok(Stmt::Return(ReturnStmt {
            value,
            span: TextRange::new(start, end),
        }))
    }

    pub(super) fn parse_if_stmt(&mut self) -> ParseResult<Stmt<'a>> {
        let start = self.peek().span.start();
        self.consume(TokenKind::If)?;

        let test = self.parse_expression()?;

        if self.peek().kind != TokenKind::Colon {
            self.recover_missing_colon("if statement")?;
        } else {
            self.consume(TokenKind::Colon)?;
            self.consume_newline();
        }

        let body = if self.peek().kind == TokenKind::Indent {
            self.parse_block()?
        } else {
            vec![self.parse_simple_stmt()?]
        };
        let mut orelse = vec![];

        while self.match_token(TokenKind::Elif) {
            let _elif_test = self.parse_expression()?;

            if self.peek().kind != TokenKind::Colon {
                self.recover_missing_colon("elif clause")?;
            } else {
                self.consume(TokenKind::Colon)?;
                self.consume_newline();
            }
            let _ = self.parse_block()?;
        }

        if self.match_token(TokenKind::Else) {
            if self.peek().kind != TokenKind::Colon {
                self.recover_missing_colon("else clause")?;
            } else {
                self.consume(TokenKind::Colon)?;
                self.consume_newline();
            }
            orelse = self.parse_block()?;
        }

        let orelse_slice = self.arena.alloc_slice_vec(orelse);
        let body_slice = self.arena.alloc_slice_vec(body);

        let span = TextRange::new(start, body_slice[body_slice.len() - 1].span().end());

        Ok(Stmt::If(IfStmt {
            test,
            body: body_slice,
            orelse: orelse_slice,
            span,
        }))
    }

    pub(super) fn parse_func_def(
        &mut self,
        decorators: &'a [Expr<'a>],
        is_async: bool,
    ) -> ParseResult<Stmt<'a>> {
        let start = self.peek().span.start();
        self.consume(TokenKind::Def)?;

        let name = if self.peek().kind == TokenKind::Constructor {
            self.advance();
            self.arena.alloc_str("constructor")
        } else {
            self.consume_ident()?
        };

        let type_params = if self.peek().kind == TokenKind::LeftBracket {
            self.parse_type_params()?
        } else {
            self.arena.alloc_slice_vec(Vec::new())
        };

        let opening_span = self.peek().span;
        self.push_delimiter('(', opening_span);
        self.consume(TokenKind::LeftParen)?;

        let args = self.parse_arguments()?;

        let closing_span = self.peek().span;
        self.consume(TokenKind::RightParen)?;
        self.pop_delimiter(')', closing_span)?;

        let returns = if self.match_token(TokenKind::Arrow) {
            Some(Box::new(self.parse_expression()?))
        } else {
            None
        };

        // Protocol methods don't have colons or bodies
        let (body_slice, span) = if self.context.in_protocol {
            // For protocol methods, don't require colon, just consume newline
            self.consume_newline();
            // Protocol methods have empty body
            let empty_body = self.arena.alloc_slice_vec(Vec::new());
            let span = TextRange::new(
                start,
                returns
                    .as_ref()
                    .map(|r| r.span().end())
                    .unwrap_or(closing_span.end()),
            );
            (empty_body, span)
        } else {
            // Regular function methods require colon and body
            if self.peek().kind != TokenKind::Colon {
                self.recover_missing_colon("function definition")?;
            } else {
                self.consume(TokenKind::Colon)?;
                self.consume_newline();
            }

            let saved_context = self.context;
            self.context = self.context.enter_function(is_async);

            let body = if self.peek().kind == TokenKind::Indent {
                self.parse_block()?
            } else {
                vec![self.parse_stmt()?]
            };
            let body_slice = self.arena.alloc_slice_vec(body);

            self.context = saved_context;

            let span = TextRange::new(start, body_slice[body_slice.len() - 1].span().end());
            (body_slice, span)
        };

        let docstring = Parser::extract_docstring_from_body(body_slice, self.source, self.arena);

        Ok(Stmt::FuncDef(FuncDefStmt {
            name,
            type_params,
            args,
            body: body_slice,
            decorators,
            returns,
            is_async,
            span,
            docstring,
        }))
    }

    pub(super) fn parse_while_stmt(&mut self) -> ParseResult<Stmt<'a>> {
        let start = self.peek().span.start();
        self.consume(TokenKind::While)?;

        let test = self.parse_expression()?;

        if self.peek().kind != TokenKind::Colon {
            self.recover_missing_colon("while statement")?;
        } else {
            self.consume(TokenKind::Colon)?;
            self.consume_newline();
        }

        let saved_context = self.context;
        self.context = self.context.enter_loop();

        let body = self.parse_block()?;
        let body_slice = self.arena.alloc_slice_vec(body);

        self.context = saved_context;

        let mut orelse = Vec::new();
        if self.match_token(TokenKind::Else) {
            if self.peek().kind != TokenKind::Colon {
                self.recover_missing_colon("else clause")?;
            } else {
                self.consume(TokenKind::Colon)?;
                self.consume_newline();
            }
            orelse = self.parse_block()?;
        }
        let orelse_slice = self.arena.alloc_slice_vec(orelse);

        let end = if orelse_slice.is_empty() {
            body_slice.last().map(|s| s.span().end()).unwrap_or(start)
        } else {
            orelse_slice.last().map(|s| s.span().end()).unwrap_or(start)
        };

        Ok(Stmt::While(WhileStmt {
            test,
            body: body_slice,
            orelse: orelse_slice,
            span: TextRange::new(start, end),
        }))
    }

    pub(super) fn parse_for_stmt(&mut self, is_async: bool) -> ParseResult<Stmt<'a>> {
        let start = self.peek().span.start();
        self.consume(TokenKind::For)?;

        let target = self.parse_for_target()?;

        self.consume(TokenKind::In)?;
        let iter = self.parse_expression()?;

        if self.peek().kind != TokenKind::Colon {
            self.recover_missing_colon("for statement")?;
        } else {
            self.consume(TokenKind::Colon)?;
            self.consume_newline();
        }

        let saved_context = self.context;
        self.context = self.context.enter_loop();

        let body = self.parse_block()?;
        let body_slice = self.arena.alloc_slice_vec(body);

        self.context = saved_context;

        let mut orelse = Vec::new();
        if self.match_token(TokenKind::Else) {
            if self.peek().kind != TokenKind::Colon {
                self.recover_missing_colon("else clause")?;
            } else {
                self.consume(TokenKind::Colon)?;
                self.consume_newline();
            }
            orelse = self.parse_block()?;
        }
        let orelse_slice = self.arena.alloc_slice_vec(orelse);

        let end = if orelse_slice.is_empty() {
            body_slice.last().map(|s| s.span().end()).unwrap_or(start)
        } else {
            orelse_slice.last().map(|s| s.span().end()).unwrap_or(start)
        };

        Ok(Stmt::For(ForStmt {
            target,
            iter,
            body: body_slice,
            orelse: orelse_slice,
            is_async,
            span: TextRange::new(start, end),
        }))
    }

    /// Parse the target of a for loop, which can be a tuple or a single expression
    fn parse_for_target(&mut self) -> ParseResult<Expr<'a>> {
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

    pub(super) fn parse_class_def(&mut self, decorators: &'a [Expr<'a>]) -> ParseResult<Stmt<'a>> {
        let start = self.peek().span.start();
        let is_protocol = self.peek().kind == TokenKind::Protocol;
        if is_protocol {
            self.consume(TokenKind::Protocol)?;
        } else {
            self.consume(TokenKind::Class)?;
        }

        let name = self.consume_ident()?;

        let type_params = if self.peek().kind == TokenKind::LeftBracket {
            self.parse_type_params()?
        } else {
            self.arena.alloc_slice_vec(Vec::new())
        };

        // Parse "implements Protocol1, Protocol2" syntax
        let (bases, keywords) = if self.match_token(TokenKind::Implements) {
            let mut base_list = Vec::new();

            // Parse comma-separated list of protocols
            loop {
                let protocol = self.parse_expression()?;
                base_list.push(protocol);

                if !self.match_token(TokenKind::Comma) {
                    break;
                }
            }

            (
                self.arena.alloc_slice_vec(base_list),
                self.arena.alloc_slice_vec(Vec::new()),
            )
        } else if self.match_token(TokenKind::LeftParen) {
            let opening_span = self.prev().span;
            self.push_delimiter('(', opening_span);
            let mut base_list = Vec::new();
            let mut keyword_list = Vec::new();

            if self.peek().kind != TokenKind::RightParen {
                loop {
                    let expr = self.parse_expression()?;

                    if self.peek().kind == TokenKind::Equal {
                        self.advance(); // consume =
                        let value = self.parse_expression()?;

                        let name = match expr {
                            Expr::Name(name_expr) => Some(name_expr.id),
                            _ => None, // This shouldn't happen in valid syntax
                        };

                        keyword_list.push(Keyword { arg: name, value });
                    } else {
                        base_list.push(expr);
                    }

                    if !self.match_token(TokenKind::Comma) {
                        break;
                    }

                    if self.peek().kind == TokenKind::RightParen {
                        break;
                    }
                }
            }
            let closing_span = self.peek().span;
            self.consume(TokenKind::RightParen)?;
            self.pop_delimiter(')', closing_span)?;
            (
                self.arena.alloc_slice_vec(base_list),
                self.arena.alloc_slice_vec(keyword_list),
            )
        } else {
            (
                self.arena.alloc_slice_vec(Vec::new()),
                self.arena.alloc_slice_vec(Vec::new()),
            )
        };

        if self.peek().kind != TokenKind::Colon {
            self.recover_missing_colon("class definition")?;
        } else {
            self.consume(TokenKind::Colon)?;
            self.consume_newline();
        }

        // Set protocol context if we're parsing a protocol
        let saved_context = self.context;
        if is_protocol {
            self.context = self.context.enter_protocol();
        }

        let body = self.parse_block()?;
        let body_slice = self.arena.alloc_slice_vec(body);

        // Restore context
        self.context = saved_context;

        let docstring = Parser::extract_docstring_from_body(body_slice, self.source, self.arena);

        let end = body_slice.last().map(|s| s.span().end()).unwrap_or(start);

        Ok(Stmt::ClassDef(ClassDefStmt {
            name,
            type_params,
            bases,
            keywords,
            body: body_slice,
            decorators,
            is_protocol,
            span: TextRange::new(start, end),
            docstring,
        }))
    }

    /// Parse a dotted name (e.g., "os.path", "collections.abc.Mapping")
    fn parse_dotted_name(&mut self) -> ParseResult<&'a str> {
        let mut parts = Vec::new();

        parts.push(self.consume_ident()?);

        while self.peek().kind == TokenKind::Dot {
            self.advance(); // consume the dot
            parts.push(self.consume_ident()?);
        }

        let full_name = parts.join(".");
        Ok(self.arena.alloc_str(&full_name))
    }

    pub(super) fn parse_import_stmt(&mut self) -> ParseResult<Stmt<'a>> {
        let start = self.peek().span.start();
        self.consume(TokenKind::Import)?;

        let mut names = Vec::new();
        loop {
            let name = self.parse_dotted_name()?;
            let alias = if self.match_token(TokenKind::As) {
                Some(self.consume_ident()?)
            } else {
                None
            };
            names.push((name, alias));

            if !self.match_token(TokenKind::Comma) {
                break;
            }
        }

        self.consume_newline();
        let end = self.peek().span.start();

        self.seen_non_future_statement = true;

        Ok(Stmt::Import(ImportStmt {
            names: self.arena.alloc_slice_vec(names),
            span: TextRange::new(start, end),
        }))
    }

    pub(super) fn parse_from_stmt(&mut self) -> ParseResult<Stmt<'a>> {
        let start = self.peek().span.start();
        self.consume(TokenKind::From)?;

        let mut level = 0;
        loop {
            match self.peek().kind {
                TokenKind::Dot => {
                    self.advance();
                    level += 1;
                }
                TokenKind::Ellipsis => {
                    self.advance();
                    level += 3;
                }
                _ => break,
            }
        }

        let module = if self.peek().kind == TokenKind::Ident {
            Some(self.parse_dotted_name()?)
        } else {
            None
        };

        let is_future_import = module == Some("__future__");
        if is_future_import && self.seen_non_future_statement {
            return Err(error(
                ErrorKind::FutureImportNotFirst,
                TextRange::new(start, self.peek().span.end()),
            ));
        }

        if level > 0 && module.is_none() && level > 10 {
            return Err(error(
                ErrorKind::RelativeImportBeyondTopLevel,
                TextRange::new(start, self.peek().span.end()),
            ));
        }

        self.consume(TokenKind::Import)?;

        let has_parens = self.match_token(TokenKind::LeftParen);

        if has_parens {
            self.consume_newline();
        }

        let mut names = Vec::new();
        if self.peek().kind == TokenKind::Star {
            self.advance();
            names.push(("*", None));
        } else {
            loop {
                while matches!(
                    self.peek().kind,
                    TokenKind::Newline | TokenKind::Indent | TokenKind::Dedent
                ) {
                    self.advance();
                }

                if has_parens && self.peek().kind == TokenKind::RightParen {
                    break;
                }
                if self.peek().kind == TokenKind::Eof {
                    break;
                }

                let name = self.consume_ident()?;
                let alias = if self.match_token(TokenKind::As) {
                    Some(self.consume_ident()?)
                } else {
                    None
                };
                names.push((name, alias));

                while matches!(
                    self.peek().kind,
                    TokenKind::Newline | TokenKind::Indent | TokenKind::Dedent
                ) {
                    self.advance();
                }

                if !self.match_token(TokenKind::Comma) {
                    break;
                }
            }
        }

        if has_parens {
            while matches!(
                self.peek().kind,
                TokenKind::Newline | TokenKind::Indent | TokenKind::Dedent
            ) {
                self.advance();
            }
            self.consume(TokenKind::RightParen)?;
        }

        self.consume_newline();
        let end = self.peek().span.start();

        if !is_future_import {
            self.seen_non_future_statement = true;
        }

        Ok(Stmt::From(FromStmt {
            level,
            module,
            names: self.arena.alloc_slice_vec(names),
            span: TextRange::new(start, end),
        }))
    }

    pub(super) fn parse_export_stmt(&mut self) -> ParseResult<Stmt<'a>> {
        let start = self.peek().span.start();
        self.consume(TokenKind::Export)?;

        let mut names = Vec::new();

        loop {
            let name = self.consume_ident()?;
            let alias = if self.match_token(TokenKind::As) {
                Some(self.consume_ident()?)
            } else {
                None
            };
            names.push((name, alias));

            if !self.match_token(TokenKind::Comma) {
                break;
            }
        }

        let module = if self.match_token(TokenKind::From) {
            Some(self.parse_dotted_name()?)
        } else {
            None
        };

        self.consume_newline();
        let end = self.peek().span.start();

        Ok(Stmt::Export(ExportStmt {
            names: self.arena.alloc_slice_vec(names),
            module,
            span: TextRange::new(start, end),
        }))
    }

    pub(super) fn parse_try_stmt(&mut self) -> ParseResult<Stmt<'a>> {
        let start = self.peek().span.start();
        self.consume(TokenKind::Try)?;

        if self.peek().kind != TokenKind::Colon {
            self.recover_missing_colon("try statement")?;
        } else {
            self.consume(TokenKind::Colon)?;
            self.consume_newline();
        }

        let body = self.parse_block()?;
        let body_slice = self.arena.alloc_slice_vec(body);

        let mut handlers = Vec::new();
        let mut has_regular_except = false;
        let mut has_except_star = false;

        while self.peek().kind == TokenKind::Except {
            let handler_start = self.peek().span.start();
            self.advance();

            let is_exception_group = self.match_token(TokenKind::Star);

            if is_exception_group {
                if has_regular_except {
                    return Err(error(
                        ErrorKind::MixedExceptSyntax,
                        TextRange::new(handler_start, self.peek().span.end()),
                    ));
                }
                has_except_star = true;
            } else {
                if has_except_star {
                    return Err(error(
                        ErrorKind::MixedExceptSyntax,
                        TextRange::new(handler_start, self.peek().span.end()),
                    ));
                }
                has_regular_except = true;
            }

            let (typ, name) = if self.peek().kind == TokenKind::Colon {
                if is_exception_group {
                    return Err(error(
                        ErrorKind::BareExceptStar,
                        TextRange::new(handler_start, self.peek().span.start()),
                    ));
                }
                (None, None)
            } else {
                let typ = Some(self.parse_expression()?);
                let name = if self.match_token(TokenKind::As) {
                    Some(self.consume_ident()?)
                } else {
                    None
                };
                (typ, name)
            };

            if self.peek().kind != TokenKind::Colon {
                self.recover_missing_colon("except clause")?;
            } else {
                self.consume(TokenKind::Colon)?;
                self.consume_newline();
            }

            let handler_body = self.parse_block()?;
            let handler_body_slice = self.arena.alloc_slice_vec(handler_body);
            let handler_end = handler_body_slice
                .last()
                .map(|s| s.span().end())
                .unwrap_or(handler_start);

            handlers.push(ExceptHandler {
                typ,
                name,
                body: handler_body_slice,
                is_exception_group,
                span: TextRange::new(handler_start, handler_end),
            });
        }

        let mut orelse = Vec::new();
        if self.match_token(TokenKind::Else) {
            if self.peek().kind != TokenKind::Colon {
                self.recover_missing_colon("else clause")?;
            } else {
                self.consume(TokenKind::Colon)?;
                self.consume_newline();
            }
            orelse = self.parse_block()?;
        }

        let mut finalbody = Vec::new();
        if self.match_token(TokenKind::Finally) {
            if self.peek().kind != TokenKind::Colon {
                self.recover_missing_colon("finally clause")?;
            } else {
                self.consume(TokenKind::Colon)?;
                self.consume_newline();
            }
            finalbody = self.parse_block()?;
        }

        let end = finalbody
            .last()
            .or(orelse.last())
            .or(handlers.last().and_then(|h| h.body.last()))
            .map(|s| s.span().end())
            .unwrap_or(start);

        Ok(Stmt::Try(TryStmt {
            body: body_slice,
            handlers: self.arena.alloc_slice_vec(handlers),
            orelse: self.arena.alloc_slice_vec(orelse),
            finalbody: self.arena.alloc_slice_vec(finalbody),
            span: TextRange::new(start, end),
        }))
    }

    pub(super) fn parse_with_stmt(&mut self, is_async: bool) -> ParseResult<Stmt<'a>> {
        let start = self.peek().span.start();
        self.consume(TokenKind::With)?;

        let mut items = Vec::new();
        loop {
            let context_expr = self.parse_expression()?;
            let optional_vars = if self.match_token(TokenKind::As) {
                Some(self.parse_expression()?)
            } else {
                None
            };
            items.push(WithItem {
                context_expr,
                optional_vars,
            });

            if !self.match_token(TokenKind::Comma) {
                break;
            }
        }

        if self.peek().kind != TokenKind::Colon {
            self.recover_missing_colon("with statement")?;
        } else {
            self.consume(TokenKind::Colon)?;
            self.consume_newline();
        }

        let body = self.parse_block()?;
        let body_slice = self.arena.alloc_slice_vec(body);
        let end = body_slice.last().map(|s| s.span().end()).unwrap_or(start);

        Ok(Stmt::With(WithStmt {
            items: self.arena.alloc_slice_vec(items),
            body: body_slice,
            is_async,
            span: TextRange::new(start, end),
        }))
    }

    pub(super) fn parse_raise_stmt(&mut self) -> ParseResult<Stmt<'a>> {
        let start = self.peek().span.start();
        self.consume(TokenKind::Raise)?;

        let exc = if self.peek().kind != TokenKind::Newline
            && self.peek().kind != TokenKind::Dedent
            && !self.is_at_end()
        {
            Some(self.parse_expression()?)
        } else {
            None
        };

        let cause = if self.match_token(TokenKind::From) {
            Some(self.parse_expression()?)
        } else {
            None
        };

        self.consume_newline();
        let end = self.peek().span.start();

        Ok(Stmt::Raise(RaiseStmt {
            exc,
            cause,
            span: TextRange::new(start, end),
        }))
    }

    pub(super) fn parse_assert_stmt(&mut self) -> ParseResult<Stmt<'a>> {
        let start = self.peek().span.start();
        self.consume(TokenKind::Assert)?;

        let test = self.parse_expression()?;
        let msg = if self.match_token(TokenKind::Comma) {
            Some(self.parse_expression()?)
        } else {
            None
        };

        self.consume_newline();
        let end = self.peek().span.start();

        Ok(Stmt::Assert(AssertStmt {
            test,
            msg,
            span: TextRange::new(start, end),
        }))
    }

    pub(super) fn parse_delete_stmt(&mut self) -> ParseResult<Stmt<'a>> {
        let start = self.peek().span.start();
        self.consume(TokenKind::Del)?;

        let mut targets = Vec::new();
        loop {
            targets.push(self.parse_expression()?);
            if !self.match_token(TokenKind::Comma) {
                break;
            }
        }

        self.consume_newline();
        let end = self.peek().span.start();

        Ok(Stmt::Delete(DeleteStmt {
            targets: self.arena.alloc_slice_vec(targets),
            span: TextRange::new(start, end),
        }))
    }

    pub(super) fn parse_global_stmt(&mut self) -> ParseResult<Stmt<'a>> {
        let start = self.peek().span.start();
        self.consume(TokenKind::Global)?;

        let mut names = Vec::new();
        loop {
            names.push(self.consume_ident()?);
            if !self.match_token(TokenKind::Comma) {
                break;
            }
        }

        self.consume_newline();
        let end = self.peek().span.start();

        Ok(Stmt::Global(GlobalStmt {
            names: self.arena.alloc_slice_vec(names),
            span: TextRange::new(start, end),
        }))
    }

    pub(super) fn parse_nonlocal_stmt(&mut self) -> ParseResult<Stmt<'a>> {
        let start = self.peek().span.start();
        self.consume(TokenKind::Nonlocal)?;

        let mut names = Vec::new();
        loop {
            names.push(self.consume_ident()?);
            if !self.match_token(TokenKind::Comma) {
                break;
            }
        }

        self.consume_newline();
        let end = self.peek().span.start();

        Ok(Stmt::Nonlocal(NonlocalStmt {
            names: self.arena.alloc_slice_vec(names),
            span: TextRange::new(start, end),
        }))
    }

    pub(super) fn parse_async_stmt(&mut self) -> ParseResult<Stmt<'a>> {
        let async_token = self.peek().span;
        self.consume(TokenKind::Async)?;

        match self.peek().kind {
            TokenKind::Def => self.parse_func_def(&[], true),
            TokenKind::For => {
                if !self.context.in_async_function {
                    return Err(error(ErrorKind::AsyncForOutsideAsync, async_token));
                }
                self.parse_for_stmt(true)
            }
            TokenKind::With => {
                if !self.context.in_async_function {
                    return Err(error(ErrorKind::AsyncWithOutsideAsync, async_token));
                }
                self.parse_with_stmt(true)
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

    pub(super) fn parse_yield_stmt(&mut self) -> ParseResult<Stmt<'a>> {
        let start = self.peek().span.start();
        let yield_token = self.peek().span;
        self.consume(TokenKind::Yield)?;

        if !self.context.in_function {
            return Err(error(ErrorKind::YieldOutsideFunction, yield_token));
        }

        if self.peek().kind == TokenKind::From {
            self.advance(); // consume 'from'
            let value = self.parse_expression()?;
            let end = value.span().end();
            self.consume_newline();

            let yield_from_expr = Expr::YieldFrom(YieldFromExpr {
                value: self.arena.alloc(value),
                span: TextRange::new(start, end),
            });

            Ok(Stmt::Yield(YieldStmt {
                value: Some(self.arena.alloc(yield_from_expr)),
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

            self.consume_newline();

            Ok(Stmt::Yield(YieldStmt {
                value,
                span: TextRange::new(start, end),
            }))
        }
    }

    pub(super) fn parse_type_alias_stmt(&mut self) -> ParseResult<Stmt<'a>> {
        let start = self.peek().span.start();

        if self.peek().kind == TokenKind::Ident && self.get_ident_text() == "type" {
            self.advance();
        } else {
            return Err(error(
                ErrorKind::UnexpectedToken {
                    expected: None,
                    found: format!("{:?}", self.peek().kind),
                },
                self.peek().span,
            ));
        }

        let name = self.consume_ident()?;

        let type_params = if self.peek().kind == TokenKind::LeftBracket {
            self.parse_type_params()?
        } else {
            self.arena.alloc_slice_vec(Vec::new())
        };

        self.consume(TokenKind::Equal)?;

        let value = self.parse_expression()?;
        let end = value.span().end();

        self.consume_newline();

        Ok(Stmt::TypeAlias(TypeAliasStmt {
            name,
            type_params,
            value,
            span: TextRange::new(start, end),
        }))
    }

    pub(super) fn parse_match_stmt(&mut self) -> ParseResult<Stmt<'a>> {
        let start = self.peek().span.start();

        if self.peek().kind == TokenKind::Ident && self.get_ident_text() == "match" {
            self.advance();
        } else {
            return Err(error(
                ErrorKind::UnexpectedToken {
                    expected: None,
                    found: format!("{:?}", self.peek().kind),
                },
                self.peek().span,
            ));
        }

        let subject = self.parse_expression()?;

        if self.peek().kind != TokenKind::Colon {
            self.recover_missing_colon("match statement")?;
        } else {
            self.consume(TokenKind::Colon)?;
            self.consume_newline();
        }

        self.consume(TokenKind::Indent)?;

        let mut cases = Vec::new();

        while self.peek().kind == TokenKind::Ident && self.get_ident_text() == "case" {
            cases.push(self.parse_match_case()?);
        }

        self.consume(TokenKind::Dedent)?;
        let end = self.peek().span.start();

        Ok(Stmt::Match(crate::ast::patterns::MatchStmt {
            subject,
            cases: self.arena.alloc_slice_vec(cases),
            span: TextRange::new(start, end),
        }))
    }

    fn parse_match_case(&mut self) -> ParseResult<crate::ast::patterns::MatchCase<'a>> {
        let start = self.peek().span.start();

        if self.peek().kind == TokenKind::Ident && self.get_ident_text() == "case" {
            self.advance();
        } else {
            return Err(error(
                ErrorKind::UnexpectedToken {
                    expected: None,
                    found: format!("{:?}", self.peek().kind),
                },
                self.peek().span,
            ));
        }

        let pattern = self.parse_pattern()?;

        let guard = if self.match_token(TokenKind::If) {
            Some(self.parse_expression()?)
        } else {
            None
        };

        if self.peek().kind != TokenKind::Colon {
            self.recover_missing_colon("case clause")?;
        } else {
            self.consume(TokenKind::Colon)?;
            self.consume_newline();
        }

        let body_vec = self.parse_block()?;
        let body = self.arena.alloc_slice_vec(body_vec);
        let end = self.peek().span.start();

        Ok(crate::ast::patterns::MatchCase {
            pattern,
            guard,
            body,
            span: TextRange::new(start, end),
        })
    }

    fn parse_pattern(&mut self) -> ParseResult<crate::ast::patterns::Pattern<'a>> {
        self.parse_or_pattern()
    }

    fn parse_or_pattern(&mut self) -> ParseResult<crate::ast::patterns::Pattern<'a>> {
        let start = self.peek().span.start();
        let first = self.parse_as_pattern()?;

        if self.peek().kind == TokenKind::Pipe {
            let mut patterns = vec![first];

            while self.match_token(TokenKind::Pipe) {
                patterns.push(self.parse_as_pattern()?);
            }

            for pattern in &patterns {
                if self.is_wildcard_pattern(pattern) {
                    return Err(error(
                        ErrorKind::InvalidPatternSyntax {
                            pattern: "Wildcard pattern '_' cannot appear in or-patterns"
                                .to_string(),
                        },
                        pattern.span(),
                    ));
                }
            }

            let end = patterns.last().unwrap().span().end();
            return Ok(crate::ast::patterns::Pattern::MatchOr(
                crate::ast::patterns::MatchOrPattern {
                    patterns: self.arena.alloc_slice_vec(patterns),
                    span: TextRange::new(start, end),
                },
            ));
        }

        Ok(first)
    }

    fn parse_as_pattern(&mut self) -> ParseResult<crate::ast::patterns::Pattern<'a>> {
        let start = self.peek().span.start();
        let pattern = self.parse_primary_pattern()?;

        if self.peek().kind == TokenKind::As {
            self.advance();
            let name = self.consume_ident()?;
            let end = self.prev().span.end();

            return Ok(crate::ast::patterns::Pattern::MatchAs(
                crate::ast::patterns::MatchAsPattern {
                    pattern: Some(Box::new(pattern)),
                    name: Some(name),
                    span: TextRange::new(start, end),
                },
            ));
        }

        Ok(pattern)
    }

    fn parse_primary_pattern(&mut self) -> ParseResult<crate::ast::patterns::Pattern<'a>> {
        let start = self.peek().span.start();

        match self.peek().kind {
            TokenKind::Ident => {
                let name_str = self.source
                    [self.peek().span.start().into()..self.peek().span.end().into()]
                    .to_string();
                if name_str == "_" {
                    let span = self.peek().span;
                    self.advance();
                    return Ok(crate::ast::patterns::Pattern::MatchAs(
                        crate::ast::patterns::MatchAsPattern {
                            pattern: None,
                            name: None,
                            span,
                        },
                    ));
                }

                let name = self.consume_ident()?;

                if self.peek().kind == TokenKind::LeftParen {
                    return self.parse_class_pattern(name, start);
                }

                let end = self.prev().span.end();
                Ok(crate::ast::patterns::Pattern::MatchAs(
                    crate::ast::patterns::MatchAsPattern {
                        pattern: None,
                        name: Some(name),
                        span: TextRange::new(start, end),
                    },
                ))
            }

            TokenKind::True => {
                let span = self.peek().span;
                self.advance();
                Ok(crate::ast::patterns::Pattern::MatchSingleton(
                    crate::ast::patterns::MatchSingletonPattern {
                        value: crate::ast::patterns::MatchSingleton::True,
                        span,
                    },
                ))
            }

            TokenKind::False => {
                let span = self.peek().span;
                self.advance();
                Ok(crate::ast::patterns::Pattern::MatchSingleton(
                    crate::ast::patterns::MatchSingletonPattern {
                        value: crate::ast::patterns::MatchSingleton::False,
                        span,
                    },
                ))
            }

            TokenKind::None => {
                let span = self.peek().span;
                self.advance();
                Ok(crate::ast::patterns::Pattern::MatchSingleton(
                    crate::ast::patterns::MatchSingletonPattern {
                        value: crate::ast::patterns::MatchSingleton::None,
                        span,
                    },
                ))
            }

            TokenKind::Number | TokenKind::String => {
                let expr = self.parse_expression()?;
                let span = expr.span();
                Ok(crate::ast::patterns::Pattern::MatchValue(
                    crate::ast::patterns::MatchValuePattern { value: expr, span },
                ))
            }

            TokenKind::LeftBracket => {
                self.advance();
                let patterns = self.parse_pattern_sequence(TokenKind::RightBracket)?;
                let end = self.peek().span.end();
                if self.peek().kind != TokenKind::RightBracket {
                    return Err(error(
                        ErrorKind::InvalidPatternSyntax {
                            pattern: "Unclosed '[' in sequence pattern".to_string(),
                        },
                        TextRange::new(start, self.peek().span.start()),
                    ));
                }
                self.consume(TokenKind::RightBracket)?;

                Ok(crate::ast::patterns::Pattern::MatchSequence(
                    crate::ast::patterns::MatchSequencePattern {
                        patterns: self.arena.alloc_slice_vec(patterns),
                        span: TextRange::new(start, end),
                    },
                ))
            }

            TokenKind::LeftParen => {
                self.advance();

                if self.peek().kind == TokenKind::RightParen {
                    let end = self.peek().span.end();
                    self.consume(TokenKind::RightParen)?;
                    return Ok(crate::ast::patterns::Pattern::MatchSequence(
                        crate::ast::patterns::MatchSequencePattern {
                            patterns: &[],
                            span: TextRange::new(start, end),
                        },
                    ));
                }

                let patterns = self.parse_pattern_sequence(TokenKind::RightParen)?;
                let end = self.peek().span.end();
                if self.peek().kind != TokenKind::RightParen {
                    return Err(error(
                        ErrorKind::InvalidPatternSyntax {
                            pattern: "Unclosed '(' in pattern".to_string(),
                        },
                        TextRange::new(start, self.peek().span.start()),
                    ));
                }
                self.consume(TokenKind::RightParen)?;

                if patterns.len() == 1 {
                    Ok(patterns.into_iter().next().unwrap())
                } else {
                    Ok(crate::ast::patterns::Pattern::MatchSequence(
                        crate::ast::patterns::MatchSequencePattern {
                            patterns: self.arena.alloc_slice_vec(patterns),
                            span: TextRange::new(start, end),
                        },
                    ))
                }
            }

            TokenKind::LeftBrace => {
                self.advance();

                if self.peek().kind == TokenKind::RightBrace {
                    let end = self.peek().span.end();
                    self.consume(TokenKind::RightBrace)?;
                    return Ok(crate::ast::patterns::Pattern::MatchMapping(
                        crate::ast::patterns::MatchMappingPattern {
                            keys: &[],
                            patterns: &[],
                            rest: None,
                            span: TextRange::new(start, end),
                        },
                    ));
                }

                let mut keys = Vec::new();
                let mut patterns = Vec::new();
                let mut rest = None;

                loop {
                    if self.peek().kind == TokenKind::DoubleStar {
                        self.advance();
                        rest = Some(self.consume_ident()?);
                        if self.match_token(TokenKind::Comma)
                            && self.peek().kind == TokenKind::RightBrace
                        {
                            break;
                        }
                        if self.peek().kind == TokenKind::RightBrace {
                            break;
                        }
                    } else {
                        let key = self.parse_expression()?;
                        if self.peek().kind != TokenKind::Colon {
                            return Err(error(
                                ErrorKind::InvalidPatternSyntax {
                                    pattern: "Expected ':' after key in mapping pattern"
                                        .to_string(),
                                },
                                self.peek().span,
                            ));
                        }
                        self.consume(TokenKind::Colon)?;
                        let pattern = self.parse_pattern()?;

                        keys.push(key);
                        patterns.push(pattern);

                        if !self.match_token(TokenKind::Comma) {
                            break;
                        }

                        if self.peek().kind == TokenKind::RightBrace {
                            break;
                        }
                    }
                }

                let end = self.peek().span.end();
                if self.peek().kind != TokenKind::RightBrace {
                    return Err(error(
                        ErrorKind::InvalidPatternSyntax {
                            pattern: "Unclosed '{' in mapping pattern".to_string(),
                        },
                        TextRange::new(start, self.peek().span.start()),
                    ));
                }
                self.consume(TokenKind::RightBrace)?;

                Ok(crate::ast::patterns::Pattern::MatchMapping(
                    crate::ast::patterns::MatchMappingPattern {
                        keys: self.arena.alloc_slice_vec(keys),
                        patterns: self.arena.alloc_slice_vec(patterns),
                        rest,
                        span: TextRange::new(start, end),
                    },
                ))
            }

            TokenKind::Minus | TokenKind::Plus => {
                let expr = self.parse_expression()?;
                let span = expr.span();
                Ok(crate::ast::patterns::Pattern::MatchValue(
                    crate::ast::patterns::MatchValuePattern { value: expr, span },
                ))
            }

            _ => match self.parse_expression() {
                Ok(expr) => {
                    let span = expr.span();
                    Ok(crate::ast::patterns::Pattern::MatchValue(
                        crate::ast::patterns::MatchValuePattern { value: expr, span },
                    ))
                }
                Err(_) => Err(error(
                    ErrorKind::InvalidPatternSyntax {
                        pattern: format!("Invalid pattern starting with '{:?}'", self.peek().kind),
                    },
                    self.peek().span,
                )),
            },
        }
    }

    fn parse_pattern_sequence(
        &mut self,
        end_token: TokenKind,
    ) -> ParseResult<Vec<crate::ast::patterns::Pattern<'a>>> {
        let mut patterns = Vec::new();
        let mut star_pattern_count = 0;

        while self.peek().kind != end_token && !self.is_at_end() {
            if self.peek().kind == TokenKind::Star {
                let star_start = self.peek().span.start();
                self.advance();
                let name = self.consume_ident()?;
                let span = TextRange::new(star_start, self.prev().span.end());

                star_pattern_count += 1;
                if star_pattern_count > 1 {
                    return Err(error(
                        ErrorKind::InvalidPatternSyntax {
                            pattern: "Star pattern can appear at most once per sequence"
                                .to_string(),
                        },
                        span,
                    ));
                }

                patterns.push(crate::ast::patterns::Pattern::MatchAs(
                    crate::ast::patterns::MatchAsPattern {
                        pattern: None,
                        name: Some(name),
                        span,
                    },
                ));
            } else {
                patterns.push(self.parse_pattern()?);
            }

            if !self.match_token(TokenKind::Comma) {
                break;
            }

            if self.peek().kind == end_token {
                break;
            }
        }

        Ok(patterns)
    }

    fn parse_class_pattern(
        &mut self,
        class_name: &'a str,
        start: text_size::TextSize,
    ) -> ParseResult<crate::ast::patterns::Pattern<'a>> {
        self.consume(TokenKind::LeftParen)?;

        let mut positional_patterns = Vec::new();
        let mut kwd_attrs = Vec::new();
        let mut kwd_patterns = Vec::new();
        let mut seen_keyword = false;

        while self.peek().kind != TokenKind::RightParen && !self.is_at_end() {
            if self.peek().kind == TokenKind::Ident {
                let next_pos = self.current + 1;
                if next_pos < self.tokens.len() && self.tokens[next_pos].kind == TokenKind::Equal {
                    seen_keyword = true;
                    let attr_name = self.consume_ident()?;
                    self.consume(TokenKind::Equal)?;
                    let pattern = self.parse_pattern()?;

                    kwd_attrs.push(attr_name);
                    kwd_patterns.push(pattern);

                    if !self.match_token(TokenKind::Comma) {
                        break;
                    }
                    continue;
                }
            }

            if seen_keyword {
                return Err(error(
                    ErrorKind::InvalidPatternSyntax {
                        pattern: "Positional patterns must come before keyword patterns in class patterns".to_string(),
                    },
                    self.peek().span,
                ));
            }

            positional_patterns.push(self.parse_pattern()?);

            if !self.match_token(TokenKind::Comma) {
                break;
            }
        }

        let end = self.peek().span.end();
        self.consume(TokenKind::RightParen)?;

        let cls_expr = Expr::Name(crate::ast::expr::NameExpr {
            id: class_name,
            span: TextRange::new(
                start,
                start + text_size::TextSize::from(class_name.len() as u32),
            ),
        });

        Ok(crate::ast::patterns::Pattern::MatchClass(
            crate::ast::patterns::MatchClassPattern {
                cls: cls_expr,
                patterns: self.arena.alloc_slice_vec(positional_patterns),
                kwd_attrs: self.arena.alloc_slice_vec(kwd_attrs),
                kwd_patterns: self.arena.alloc_slice_vec(kwd_patterns),
                span: TextRange::new(start, end),
            },
        ))
    }

    /// Check if a pattern is a wildcard pattern (_)
    fn is_wildcard_pattern(&self, pattern: &crate::ast::patterns::Pattern<'a>) -> bool {
        matches!(
            pattern,
            crate::ast::patterns::Pattern::MatchAs(p) if p.pattern.is_none() && p.name.is_none()
        )
    }
}
