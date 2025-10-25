use crate::ast::*;
use crate::error::{
    RecoveryAction, RecoveryManager, RecoveryStrategy, SyncPoint, UnifiedError as Error,
    UnifiedErrorKind as ErrorKind, error, warnings::Warning,
};

pub type ParseResult<T> = Result<T, Box<Error>>;
use crate::Arena;
use crate::lexer::{CommentKind, CommentMap, Token, TokenKind};
use smallvec::SmallVec;
use text_size::{TextRange, TextSize};

/// Parsing mode for different execution contexts (module, eval, interactive)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Mode {
    /// Module/file mode - multiple statements (default for files)
    Module,
    /// Eval mode - single expression (for eval())
    Eval,
    /// Interactive/single mode - single statement (for REPL)
    Interactive,
}

/// Context tracking for validation
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct ParserContext {
    /// Are we inside a loop (for/while)?
    pub(super) in_loop: bool,
    /// Are we inside a function?
    pub(super) in_function: bool,
    /// Are we inside an async function?
    pub(super) in_async_function: bool,
    /// Are we inside a protocol definition?
    pub(super) in_protocol: bool,
}

impl ParserContext {
    fn new() -> Self {
        ParserContext {
            in_loop: false,
            in_function: false,
            in_async_function: false,
            in_protocol: false,
        }
    }

    pub(super) fn enter_loop(mut self) -> Self {
        self.in_loop = true;
        self
    }

    pub(super) fn enter_function(mut self, is_async: bool) -> Self {
        self.in_function = true;
        self.in_async_function = is_async;
        self
    }

    pub(super) fn enter_protocol(mut self) -> Self {
        self.in_protocol = true;
        self
    }
}

/// Delimiter tracking for better error messages
#[derive(Debug, Clone, Copy)]
pub(super) struct DelimiterInfo {
    pub(super) kind: char,
    pub(super) span: TextRange,
}

pub struct Parser<'a> {
    pub(super) tokens: Vec<Token>,
    pub(super) current: usize,
    #[allow(dead_code)]
    pub(super) mode: Mode,
    pub(super) arena: &'a Arena,
    pub(super) source: &'a str,
    pub(super) context: ParserContext,
    pub(super) delimiter_stack: Vec<DelimiterInfo>,
    /// Collection of errors encountered during parsing (for error recovery)
    pub(super) errors: Vec<Error>,
    /// Collection of warnings encountered during parsing
    pub(super) warnings: Vec<Warning>,
    /// Error recovery manager
    pub(super) recovery_manager: RecoveryManager,
    /// Track if we've seen any non-future imports or statements (for __future__ validation)
    pub(super) seen_non_future_statement: bool,
    /// Comments preserved for documentation generation and IDE support
    pub comment_map: CommentMap,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: crate::Lexer, arena: &'a Arena) -> Self {
        Self::with_mode(lexer, arena, Mode::Module)
    }

    pub fn with_mode(mut lexer: crate::Lexer, arena: &'a Arena, mode: Mode) -> Self {
        let source = arena.alloc_str(lexer.source());
        let (tokens, lexical_errors, lexical_warnings) = lexer.tokenize();

        let mut errors = Vec::new();
        for lexical_error in lexical_errors {
            errors.push(lexical_error);
        }

        let mut comment_map = CommentMap::new();
        Self::extract_comments(&tokens, source, &mut comment_map);

        Parser {
            tokens,
            current: 0,
            arena,
            source,
            mode,
            context: ParserContext::new(),
            delimiter_stack: Vec::new(),
            errors,
            warnings: lexical_warnings,
            recovery_manager: RecoveryManager::new(),
            seen_non_future_statement: false,
            comment_map,
        }
    }

    pub fn parse_module(&mut self) -> ParseResult<&'a Module<'a>> {
        let _start = self.current;
        let mut body = Vec::new();

        while !self.is_at_end() && self.peek().kind != TokenKind::Eof {
            self.skip_comments_and_newlines();
            if self.is_at_end() || self.peek().kind == TokenKind::Eof {
                break;
            }

            match self.parse_stmt() {
                Ok(stmt) => body.push(stmt),
                Err(error) => {
                    self.record_error(*error);

                    self.synchronize();

                    if self.recovery_manager.limit_reached() || self.is_at_end() {
                        break;
                    }
                }
            }
        }

        if let Some(error) = self.check_unclosed_delimiters() {
            self.record_error(*error);
        }

        let span = if !body.is_empty() {
            let first = body[0].span();
            let last = body[body.len() - 1].span();
            TextRange::new(first.start(), last.end())
        } else {
            TextRange::default()
        };

        let docstring = Self::extract_docstring_from_body(&body, self.source, self.arena);

        let body_slice = self.arena.alloc_slice_vec(body);
        let module = Module {
            body: body_slice,
            span,
            docstring,
        };
        Ok(self.arena.alloc(module))
    }

    /// Parse a single expression (eval mode)
    pub fn parse_eval(&mut self) -> ParseResult<Expr<'a>> {
        while matches!(
            self.peek().kind,
            TokenKind::Newline | TokenKind::Indent | TokenKind::Dedent
        ) {
            self.advance();
        }

        let expr = match self.parse_expression() {
            Ok(e) => e,
            Err(error) => {
                self.record_error(*error);
                self.synchronize();
                Expr::Error(ErrorExpr {
                    span: self.peek().span,
                    _phantom: std::marker::PhantomData,
                })
            }
        };

        while matches!(
            self.peek().kind,
            TokenKind::Newline | TokenKind::Indent | TokenKind::Dedent
        ) {
            self.advance();
        }

        if let Some(error) = self.check_unclosed_delimiters() {
            self.record_error(*error);
        }

        if !self.is_at_end() && self.peek().kind != TokenKind::Eof {
            let err = error(
                ErrorKind::UnexpectedToken {
                    expected: None,
                    found: format!("{:?}", self.peek().kind),
                },
                self.peek().span,
            );
            self.record_error(*err);
        }

        Ok(expr)
    }

    /// Parse a single statement (interactive mode)
    pub fn parse_interactive(&mut self) -> ParseResult<Stmt<'a>> {
        while matches!(
            self.peek().kind,
            TokenKind::Newline | TokenKind::Indent | TokenKind::Dedent
        ) {
            self.advance();
        }

        let stmt = match self.parse_stmt() {
            Ok(s) => s,
            Err(error) => {
                self.record_error(*error);
                self.synchronize();
                Stmt::Expr(ExprStmt {
                    value: Expr::Error(ErrorExpr {
                        span: self.peek().span,
                        _phantom: std::marker::PhantomData,
                    }),
                    span: self.peek().span,
                })
            }
        };

        while matches!(
            self.peek().kind,
            TokenKind::Newline | TokenKind::Indent | TokenKind::Dedent
        ) {
            self.advance();
        }

        if let Some(error) = self.check_unclosed_delimiters() {
            self.record_error(*error);
        }

        if !self.is_at_end() && self.peek().kind != TokenKind::Eof {
            let err = error(
                ErrorKind::UnexpectedToken {
                    expected: None,
                    found: format!("{:?}", self.peek().kind),
                },
                self.peek().span,
            );
            self.record_error(*err);
        }

        Ok(stmt)
    }

    pub(super) fn peek(&self) -> Token {
        if self.current < self.tokens.len() {
            self.tokens[self.current]
        } else {
            Token::new(TokenKind::Eof, TextRange::default())
        }
    }

    pub(super) fn prev(&self) -> Token {
        self.tokens[self.current - 1]
    }

    pub(super) fn advance(&mut self) -> Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.prev()
    }

    pub(super) fn is_at_end(&self) -> bool {
        self.peek().kind == TokenKind::Eof || self.current >= self.tokens.len()
    }

    pub(super) fn match_token(&mut self, kind: TokenKind) -> bool {
        if self.peek().kind == kind {
            self.advance();
            true
        } else {
            false
        }
    }

    pub(super) fn consume(&mut self, kind: TokenKind) -> ParseResult<()> {
        if self.peek().kind == kind {
            self.advance();
            Ok(())
        } else {
            let current_token = self.peek();
            let is_unmatched_closing = matches!(
                current_token.kind,
                TokenKind::RightParen | TokenKind::RightBracket | TokenKind::RightBrace
            ) && self.delimiter_stack.is_empty();

            if is_unmatched_closing {
                let delimiter = match current_token.kind {
                    TokenKind::RightParen => ')',
                    TokenKind::RightBracket => ']',
                    TokenKind::RightBrace => '}',
                    _ => unreachable!(),
                };
                return Err(error(
                    ErrorKind::UnmatchedClosing { delimiter },
                    current_token.span,
                ));
            }

            if let Some(delimiter_error) = self.check_unclosed_delimiters() {
                return Err(delimiter_error);
            }

            Err(error(
                ErrorKind::UnexpectedToken {
                    expected: None,
                    found: format!("{:?}", self.peek().kind),
                },
                self.peek().span,
            ))
        }
    }

    /// Consume a token with a context message for better error reporting
    #[allow(dead_code)]
    pub(super) fn consume_with_context(
        &mut self,
        kind: TokenKind,
        context: &str,
    ) -> ParseResult<()> {
        if self.peek().kind == kind {
            self.advance();
            Ok(())
        } else if kind == TokenKind::Colon {
            Err(error(
                ErrorKind::MissingColon {
                    context: context.to_string(),
                },
                self.peek().span,
            ))
        } else {
            Err(error(
                ErrorKind::UnexpectedToken {
                    expected: None,
                    found: format!("{:?}", self.peek().kind),
                },
                self.peek().span,
            ))
        }
    }

    pub(super) fn consume_ident(&mut self) -> ParseResult<&'a str> {
        if self.peek().kind == TokenKind::Ident {
            let span = self.peek().span;
            self.advance();
            let start = usize::from(span.start());
            let end = usize::from(span.end());
            let ident_text = &self.source[start..end];
            Ok(self.arena.alloc_str(ident_text))
        } else {
            Err(error(
                ErrorKind::UnexpectedToken {
                    expected: None,
                    found: format!("{:?}", self.peek().kind),
                },
                self.peek().span,
            ))
        }
    }

    /// Get the text of the current identifier without consuming it
    pub(super) fn get_ident_text(&self) -> &str {
        if self.peek().kind == TokenKind::Ident {
            let span = self.peek().span;
            let start = usize::from(span.start());
            let end = usize::from(span.end());
            &self.source[start..end]
        } else {
            ""
        }
    }

    /// Get the text of the current identifier without consuming it
    pub(super) fn consume_newline(&mut self) {
        if matches!(
            self.peek().kind,
            TokenKind::RightParen | TokenKind::RightBracket | TokenKind::RightBrace
        ) && self.delimiter_stack.is_empty()
        {
            let delimiter = match self.peek().kind {
                TokenKind::RightParen => ')',
                TokenKind::RightBracket => ']',
                TokenKind::RightBrace => '}',
                _ => unreachable!(),
            };
            let error = error(ErrorKind::UnmatchedClosing { delimiter }, self.peek().span);
            self.record_error(*error);
            self.advance(); // Skip the unmatched closing delimiter
        }

        self.skip_comments_and_newlines();
    }

    pub(super) fn parse_block(&mut self) -> ParseResult<Vec<Stmt<'a>>> {
        self.consume(TokenKind::Indent)?;
        let mut stmts = Vec::new();

        while self.peek().kind != TokenKind::Dedent && !self.is_at_end() {
            self.skip_comments_and_newlines();
            if self.peek().kind == TokenKind::Dedent || self.is_at_end() {
                break;
            }
            stmts.push(self.parse_stmt()?);
        }

        if self.peek().kind == TokenKind::Dedent {
            self.advance();
        }

        Ok(stmts)
    }

    pub(super) fn parse_arguments(&mut self) -> ParseResult<Arguments<'a>> {
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

        self.consume_newline();

        while self.peek().kind != TokenKind::RightParen && !self.is_at_end() {
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

                self.consume_newline();
                continue;
            }

            if self.peek().kind == TokenKind::Star {
                if vararg.is_some() {
                    return Err(error(ErrorKind::InvalidParameterOrder, self.peek().span));
                }

                self.advance();
                seen_star = true;

                if self.peek().kind == TokenKind::Ident {
                    let name = self.consume_ident()?;
                    let annotation = if self.match_token(TokenKind::Colon) {
                        Some(Box::new(self.parse_expression()?))
                    } else {
                        None
                    };
                    vararg = Some(Box::new(Arg {
                        arg: name,
                        annotation,
                    }));
                }

                if !self.match_token(TokenKind::Comma) {
                    break;
                }

                self.consume_newline();
                continue;
            }

            if self.peek().kind == TokenKind::DoubleStar {
                if kwarg.is_some() {
                    return Err(error(ErrorKind::InvalidParameterOrder, self.peek().span));
                }

                self.advance();
                let name = self.consume_ident()?;
                let annotation = if self.match_token(TokenKind::Colon) {
                    Some(Box::new(self.parse_expression()?))
                } else {
                    None
                };
                kwarg = Some(Box::new(Arg {
                    arg: name,
                    annotation,
                }));

                if !self.match_token(TokenKind::Comma) {
                    break;
                }

                self.consume_newline();

                if self.peek().kind != TokenKind::RightParen {
                    return Err(error(ErrorKind::InvalidParameterOrder, self.peek().span));
                }
                continue;
            }

            if kwarg.is_some() {
                return Err(error(ErrorKind::InvalidParameterOrder, self.peek().span));
            }

            let name = self.consume_ident()?;
            let annotation = if self.match_token(TokenKind::Colon) {
                Some(Box::new(self.parse_expression()?))
            } else {
                None
            };

            let arg = Arg {
                arg: name,
                annotation,
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
            } else {
                if !seen_star && !defaults.is_empty() {
                    return Err(error(ErrorKind::InvalidParameterOrder, self.prev().span));
                }

                if seen_star {
                    kw_defaults.push(None);
                }
            }

            if seen_star {
                kwonlyargs.push(arg);
            } else {
                args.push(arg);
            }

            if !self.match_token(TokenKind::Comma) {
                break;
            }

            self.consume_newline();
        }

        let mut all_defaults = posonly_defaults;
        all_defaults.extend(defaults);

        let mut seen_names = std::collections::HashSet::new();

        for arg in posonlyargs
            .iter()
            .chain(args.iter())
            .chain(kwonlyargs.iter())
        {
            if !seen_names.insert(arg.arg) {
                return Err(error(
                    ErrorKind::DuplicateParameter {
                        name: arg.arg.to_string(),
                    },
                    self.prev().span,
                ));
            }
        }

        if let Some(ref va) = vararg
            && !seen_names.insert(va.arg)
        {
            return Err(error(
                ErrorKind::DuplicateParameter {
                    name: va.arg.to_string(),
                },
                self.prev().span,
            ));
        }

        if let Some(ref kwa) = kwarg
            && !seen_names.insert(kwa.arg)
        {
            return Err(error(
                ErrorKind::DuplicateParameter {
                    name: kwa.arg.to_string(),
                },
                self.prev().span,
            ));
        }

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

    /// Parse type parameters for generic functions and classes.
    /// Syntax: [T], [T, U], [T: int], [T: (int, str)], [T = int]
    pub(super) fn parse_type_params(&mut self) -> ParseResult<&'a [TypeParam<'a>]> {
        let mut type_params = Vec::new();

        self.consume(TokenKind::LeftBracket)?;

        self.consume_newline();

        while self.peek().kind != TokenKind::RightBracket && !self.is_at_end() {
            let start = self.peek().span.start();
            let name = self.consume_ident()?;

            let bound = if self.match_token(TokenKind::Colon) {
                Some(self.parse_expression()?)
            } else {
                None
            };

            let default = if self.match_token(TokenKind::Equal) {
                Some(self.parse_expression()?)
            } else {
                None
            };

            let end = default
                .as_ref()
                .or(bound.as_ref())
                .map(|e| e.span().end())
                .unwrap_or_else(|| self.prev().span.end());

            type_params.push(TypeParam {
                name,
                bound,
                default,
                span: TextRange::new(start, end),
            });

            if !self.match_token(TokenKind::Comma) {
                break;
            }

            self.consume_newline();
        }

        self.consume(TokenKind::RightBracket)?;

        Ok(self.arena.alloc_slice_vec(type_params))
    }

    /// Push an opening delimiter onto the stack
    pub(super) fn push_delimiter(&mut self, kind: char, span: TextRange) {
        self.delimiter_stack.push(DelimiterInfo { kind, span });
    }

    /// Pop a delimiter and verify it matches
    pub(super) fn pop_delimiter(
        &mut self,
        expected_closing: char,
        closing_span: TextRange,
    ) -> ParseResult<()> {
        if let Some(opening) = self.delimiter_stack.pop() {
            let expected_for_opening = match opening.kind {
                '(' => ')',
                '[' => ']',
                '{' => '}',
                _ => opening.kind,
            };

            if expected_for_opening == expected_closing {
                Ok(())
            } else {
                self.delimiter_stack.push(opening);
                Err(error(
                    ErrorKind::UnclosedDelimiter {
                        expected: expected_for_opening,
                        opening_span: opening.span,
                    },
                    closing_span,
                ))
            }
        } else {
            Err(error(
                ErrorKind::UnmatchedClosing {
                    delimiter: expected_closing,
                },
                closing_span,
            ))
        }
    }

    /// Check for unclosed delimiters at end of file
    pub(super) fn check_unclosed_delimiters(&self) -> Option<Box<Error>> {
        if let Some(opening) = self.delimiter_stack.last() {
            let expected_closing = match opening.kind {
                '(' => ')',
                '[' => ']',
                '{' => '}',
                _ => opening.kind,
            };
            Some(error(
                ErrorKind::UnclosedDelimiter {
                    expected: expected_closing,
                    opening_span: opening.span,
                },
                opening.span, // Use opening span as error location
            ))
        } else {
            None
        }
    }

    /// Record an error and continue parsing (error recovery mode).
    pub(super) fn record_error(&mut self, error: Error) {
        if !self.recovery_manager.limit_reached() {
            self.errors.push(error);
        }
    }

    /// Synchronize to the next statement boundary after an error.
    /// This implements "panic mode" error recovery.
    pub(super) fn synchronize(&mut self) {
        let start_pos = self.current;
        let start_span = self.peek().span;
        let max_skip = self.recovery_manager.max_skip_tokens();
        let mut tokens_skipped = 0;

        while !self.is_at_end() && tokens_skipped < max_skip {
            let token = self.peek();

            if SyncPoint::Statement.is_sync_token(&token.kind) {
                let action = RecoveryAction::with_skip_count(
                    RecoveryStrategy::SkipToNextStatement,
                    TextRange::new(start_span.start(), token.span.start()),
                    format!(
                        "Skipped {} tokens to resynchronize at {:?}",
                        tokens_skipped, token.kind
                    ),
                    tokens_skipped,
                );
                self.recovery_manager.record(action);

                if matches!(token.kind, TokenKind::Newline | TokenKind::Dedent) {
                    self.advance();
                }

                return;
            }

            if !self.delimiter_stack.is_empty()
                && matches!(
                    token.kind,
                    TokenKind::RightParen | TokenKind::RightBracket | TokenKind::RightBrace
                )
            {
                let action = RecoveryAction::with_skip_count(
                    RecoveryStrategy::SkipToClosing,
                    TextRange::new(start_span.start(), token.span.start()),
                    format!("Skipped {} tokens to closing delimiter", tokens_skipped),
                    tokens_skipped,
                );
                self.recovery_manager.record(action);
                return;
            }

            self.advance();
            tokens_skipped += 1;
        }

        if tokens_skipped > 0 {
            let end_span = if self.current > start_pos {
                self.tokens[self.current - 1].span
            } else {
                start_span
            };

            let action = RecoveryAction::with_skip_count(
                RecoveryStrategy::SkipToNextStatement,
                TextRange::new(start_span.start(), end_span.end()),
                format!("Skipped {} tokens (reached limit or EOF)", tokens_skipped),
                tokens_skipped,
            );
            self.recovery_manager.record(action);
        }
    }

    /// Attempt to recover from a missing colon in a compound statement.
    /// Records the error and tries to continue parsing the statement body.
    pub(super) fn recover_missing_colon(&mut self, context: &str) -> ParseResult<()> {
        let error_span = self.peek().span;

        self.record_error(*error(
            ErrorKind::MissingColon {
                context: context.to_string(),
            },
            error_span,
        ));

        let action = RecoveryAction::new(
            RecoveryStrategy::InsertToken,
            error_span,
            format!("Inserted virtual colon after {} header", context),
        );
        self.recovery_manager.record(action);

        self.consume_newline();

        Ok(())
    }

    /// Attempt to recover from an unclosed delimiter by suggesting the missing closing.
    #[allow(dead_code)]
    pub(super) fn recover_unclosed_delimiter(&mut self) -> ParseResult<()> {
        if let Some(opening) = self.delimiter_stack.last() {
            let expected_closing = match opening.kind {
                '(' => ')',
                '[' => ']',
                '{' => '}',
                _ => opening.kind,
            };

            let error_span = self.peek().span;

            self.record_error(*error(
                ErrorKind::UnclosedDelimiter {
                    expected: expected_closing,
                    opening_span: opening.span,
                },
                error_span,
            ));

            let action = RecoveryAction::new(
                RecoveryStrategy::InsertToken,
                error_span,
                format!("Inserted virtual closing delimiter '{}'", expected_closing),
            );
            self.recovery_manager.record(action);

            self.delimiter_stack.pop();
        }

        Ok(())
    }

    /// Check if current error is likely caused by a previous error (cascading).
    /// This helps prevent reporting multiple errors for a single root cause.
    #[allow(dead_code)]
    pub(super) fn is_cascading_error(&self) -> bool {
        if !self.errors.is_empty() && !self.recovery_manager.actions().is_empty() {
            let last_action = self.recovery_manager.actions().last().unwrap();
            let current_pos = self.peek().span.start();

            let distance = u32::from(current_pos) - u32::from(last_action.span.start());
            distance < 50 // Within 50 characters is likely cascading
        } else {
            false
        }
    }

    /// Get all errors collected during parsing.
    pub fn errors(&self) -> &[Error] {
        &self.errors
    }

    /// Get all warnings encountered during parsing.
    pub fn warnings(&self) -> &[Warning] {
        &self.warnings
    }

    /// Get the recovery manager for statistics.
    pub fn recovery_manager(&self) -> &RecoveryManager {
        &self.recovery_manager
    }

    /// Check if any errors were encountered during parsing.
    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    /// Skip comments and newlines, advancing the parser position.
    pub(super) fn skip_comments_and_newlines(&mut self) {
        while !self.is_at_end()
            && matches!(self.peek().kind, TokenKind::Comment | TokenKind::Newline)
        {
            self.advance();
        }
    }

    /// Extract comments from tokens and populate the comment map.
    fn extract_comments(tokens: &[Token], source: &str, comment_map: &mut CommentMap) {
        for token in tokens {
            if token.kind == TokenKind::Comment {
                let comment_text = &source[token.span];

                let text = comment_text
                    .strip_prefix('#')
                    .unwrap_or(comment_text)
                    .trim();

                let kind = if token.span.start() > TextSize::from(0) {
                    let before_text = &source[..token.span.start().into()];

                    if let Some(last_newline_pos) = before_text.rfind('\n') {
                        let between_newline_and_comment = &before_text[last_newline_pos + 1..];
                        if between_newline_and_comment.trim().is_empty() {
                            CommentKind::Line
                        } else {
                            CommentKind::Trailing
                        }
                    } else {
                        CommentKind::Line
                    }
                } else {
                    CommentKind::Line
                };

                comment_map.add_comment(text.to_string(), token.span, kind);
            }
        }
    }

    /// Extract docstring from a statement body (first string literal expression, if any).
    /// Returns the docstring content as a string slice allocated in the arena.
    pub(super) fn extract_docstring_from_body(
        body: &[Stmt<'a>],
        source: &str,
        arena: &'a Arena,
    ) -> Option<&'a str> {
        if let Some(Stmt::Expr(expr_stmt)) = body.first() {
            match &expr_stmt.value {
                Expr::Constant(constant_expr) => {
                    let string_span = constant_expr.span;
                    let string_text = &source[string_span];

                    let content = if (string_text.starts_with('"') && string_text.ends_with('"'))
                        || (string_text.starts_with('\'') && string_text.ends_with('\''))
                    {
                        &string_text[1..string_text.len() - 1]
                    } else {
                        string_text
                    };

                    Some(arena.alloc_str(&Self::interpret_string_escapes(content)))
                }
                _ => None,
            }
        } else {
            None
        }
    }

    /// Interpret basic escape sequences in a string.
    /// This is a simplified version for docstring extraction.
    fn interpret_string_escapes(s: &str) -> String {
        let mut result = String::new();
        let mut chars = s.chars().peekable();

        while let Some(ch) = chars.next() {
            if ch == '\\' {
                match chars.next() {
                    Some('n') => result.push('\n'),
                    Some('t') => result.push('\t'),
                    Some('r') => result.push('\r'),
                    Some('\\') => result.push('\\'),
                    Some('"') => result.push('"'),
                    Some('\'') => result.push('\''),
                    Some('0') => result.push('\0'),   // null byte
                    Some('b') => result.push('\x08'), // backspace
                    Some('f') => result.push('\x0c'), // form feed
                    Some('a') => result.push('\x07'), // bell
                    Some('v') => result.push('\x0b'), // vertical tab
                    Some(ch) => {
                        result.push('\\');
                        result.push(ch);
                    }
                    None => result.push('\\'), // Trailing backslash
                }
            } else {
                result.push(ch);
            }
        }

        result
    }
}
