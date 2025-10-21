use crate::ast::*;
use crate::error::{
    RecoveryAction, RecoveryManager, RecoveryStrategy, SyncPoint, UnifiedError as Error,
    UnifiedErrorKind as ErrorKind, error,
};

pub type ParseResult<T> = Result<T, Box<Error>>;
use crate::Arena;
use crate::lexer::{Token, TokenKind};
use smallvec::SmallVec;
use text_size::TextRange;

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
}

impl ParserContext {
    fn new() -> Self {
        ParserContext {
            in_loop: false,
            in_function: false,
            in_async_function: false,
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
    /// Error recovery manager
    pub(super) recovery_manager: RecoveryManager,
    /// Track if we've seen any non-future imports or statements (for __future__ validation)
    pub(super) seen_non_future_statement: bool,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: crate::Lexer, arena: &'a Arena) -> Self {
        Self::with_mode(lexer, arena, Mode::Module)
    }

    pub fn with_mode(mut lexer: crate::Lexer, arena: &'a Arena, mode: Mode) -> Self {
        let source = arena.alloc_str(lexer.source());
        let (tokens, lexical_errors) = lexer.tokenize();

        // Convert lexical errors to Errors for backward compatibility
        let mut errors = Vec::new();
        for lexical_error in lexical_errors {
            errors.push(*error(
                ErrorKind::InvalidSyntax {
                    message: "Lexical error".to_string(),
                },
                lexical_error.span,
            ));
        }

        Parser {
            tokens,
            current: 0,
            arena,
            source,
            mode,
            context: ParserContext::new(),
            delimiter_stack: Vec::new(),
            errors,
            recovery_manager: RecoveryManager::new(),
            seen_non_future_statement: false,
        }
    }

    pub fn parse_module(&mut self) -> ParseResult<&'a Module<'a>> {
        let _start = self.current;
        let mut body = Vec::new();

        while !self.is_at_end() && self.peek().kind != TokenKind::Eof {
            if self.peek().kind == TokenKind::Newline {
                self.advance();
                continue;
            }

            // Try to parse a statement with error recovery
            match self.parse_stmt() {
                Ok(stmt) => body.push(stmt),
                Err(error) => {
                    // Record the error
                    self.record_error(*error);

                    // Try to synchronize to the next statement
                    self.synchronize();

                    // If we're still stuck at the same position or hit recovery limit, stop
                    if self.recovery_manager.limit_reached() || self.is_at_end() {
                        break;
                    }
                }
            }
        }

        // Check for unclosed delimiters at EOF and record as error
        if let Some(error) = self.check_unclosed_delimiters() {
            self.record_error(*error);
        }

        // If we collected errors but also got some statements, return the partial AST
        // If we got no statements and have errors, return the first error
        if body.is_empty() && !self.errors.is_empty() {
            return Err(Box::new(self.errors[0].clone()));
        }

        let span = if !body.is_empty() {
            let first = body[0].span();
            let last = body[body.len() - 1].span();
            TextRange::new(first.start(), last.end())
        } else {
            TextRange::default()
        };

        let body_slice = self.arena.alloc_slice_vec(body);
        let module = Module {
            body: body_slice,
            span,
        };
        Ok(self.arena.alloc(module))
    }

    /// Parse a single expression (eval mode)
    pub fn parse_eval(&mut self) -> ParseResult<Expr<'a>> {
        // Skip leading newlines, indents, and dedents
        while matches!(
            self.peek().kind,
            TokenKind::Newline | TokenKind::Indent | TokenKind::Dedent
        ) {
            self.advance();
        }

        // Parse the expression
        let expr = self.parse_expression()?;

        // Skip trailing newlines, indents, and dedents
        while matches!(
            self.peek().kind,
            TokenKind::Newline | TokenKind::Indent | TokenKind::Dedent
        ) {
            self.advance();
        }

        // Check for unclosed delimiters at EOF
        if let Some(error) = self.check_unclosed_delimiters() {
            return Err(error);
        }

        // Ensure we've consumed all input
        if !self.is_at_end() && self.peek().kind != TokenKind::Eof {
            return Err(error(
                ErrorKind::UnexpectedToken {
                    expected: None,
                    found: format!("{:?}", self.peek().kind),
                },
                self.peek().span,
            ));
        }

        Ok(expr)
    }

    /// Parse a single statement (interactive mode)
    pub fn parse_interactive(&mut self) -> ParseResult<Stmt<'a>> {
        // Skip leading newlines, indents, and dedents
        while matches!(
            self.peek().kind,
            TokenKind::Newline | TokenKind::Indent | TokenKind::Dedent
        ) {
            self.advance();
        }

        // Parse the statement
        let stmt = self.parse_stmt()?;

        // Skip trailing newlines, indents, and dedents
        while matches!(
            self.peek().kind,
            TokenKind::Newline | TokenKind::Indent | TokenKind::Dedent
        ) {
            self.advance();
        }

        // Check for unclosed delimiters at EOF
        if let Some(error) = self.check_unclosed_delimiters() {
            return Err(error);
        }

        // Ensure we've consumed all input
        if !self.is_at_end() && self.peek().kind != TokenKind::Eof {
            return Err(error(
                ErrorKind::UnexpectedToken {
                    expected: None,
                    found: format!("{:?}", self.peek().kind),
                },
                self.peek().span,
            ));
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
            // Check if we encountered an unexpected closing delimiter
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

            // Check for unclosed delimiters - they provide better error messages
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
            // Special handling for missing colons
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
        // Check for unmatched closing delimiters before consuming newlines
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

        while self.match_token(TokenKind::Newline) {}
    }

    pub(super) fn parse_block(&mut self) -> ParseResult<Vec<Stmt<'a>>> {
        self.consume(TokenKind::Indent)?;
        let mut stmts = Vec::new();

        while self.peek().kind != TokenKind::Dedent && !self.is_at_end() {
            if self.peek().kind == TokenKind::Newline {
                self.advance();
                continue;
            }
            stmts.push(self.parse_stmt()?);
        }

        if self.peek().kind == TokenKind::Dedent {
            self.advance();
        }

        Ok(stmts)
    }

    pub(super) fn parse_arguments(&mut self) -> ParseResult<Arguments<'a>> {
        // Most functions have < 8 args, use SmallVec to avoid heap allocation
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

        // Allow newlines after opening paren
        self.consume_newline();

        while self.peek().kind != TokenKind::RightParen && !self.is_at_end() {
            // Handle / for positional-only separator
            if self.peek().kind == TokenKind::Slash {
                self.advance();
                _seen_slash = true;

                // Move args to posonlyargs and their defaults
                if !args.is_empty() {
                    posonlyargs = args.clone();
                    posonly_defaults = defaults.clone();
                    args.clear();
                    defaults.clear();
                }

                // Continue parsing after /
                if !self.match_token(TokenKind::Comma) {
                    break;
                }
                // Allow newlines after comma
                self.consume_newline();
                continue;
            }

            // Handle * or *args
            if self.peek().kind == TokenKind::Star {
                // Check if we already have a vararg
                if vararg.is_some() {
                    return Err(error(ErrorKind::InvalidParameterOrder, self.peek().span));
                }

                self.advance();
                seen_star = true;

                // Check if it's *args or just *
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

                // Continue parsing keyword-only args after *
                if !self.match_token(TokenKind::Comma) {
                    break;
                }
                // Allow newlines after comma
                self.consume_newline();
                continue;
            }

            // Handle **kwargs
            if self.peek().kind == TokenKind::DoubleStar {
                // Check if we already have a kwarg
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

                // **kwargs must be last, but allow trailing comma
                if !self.match_token(TokenKind::Comma) {
                    break;
                }
                // Allow newlines after comma
                self.consume_newline();
                // If there's more after comma and it's not ), that's an error
                if self.peek().kind != TokenKind::RightParen {
                    return Err(error(ErrorKind::InvalidParameterOrder, self.peek().span));
                }
                continue;
            }

            // Check if we're trying to add a parameter after **kwargs
            if kwarg.is_some() {
                return Err(error(ErrorKind::InvalidParameterOrder, self.peek().span));
            }

            // Regular argument
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

            // Check for default value
            let has_default = self.peek().kind == TokenKind::Equal;
            if has_default {
                self.advance(); // consume =
                let default_expr = self.parse_expression()?;

                if seen_star {
                    // Keyword-only argument with default
                    kw_defaults.push(Some(default_expr));
                } else {
                    // Positional or positional-or-keyword with default
                    defaults.push(default_expr);
                }
            } else {
                // Check for non-default parameter after default parameter
                // This is only an error for positional parameters (not keyword-only after *)
                if !seen_star && !defaults.is_empty() {
                    // We have a non-default parameter following a default parameter
                    return Err(error(ErrorKind::InvalidParameterOrder, self.prev().span));
                }

                if seen_star {
                    // Keyword-only argument without default
                    kw_defaults.push(None);
                }
            }

            // Add to appropriate list
            if seen_star {
                kwonlyargs.push(arg);
            } else {
                args.push(arg);
            }

            if !self.match_token(TokenKind::Comma) {
                break;
            }
            // Allow newlines after comma
            self.consume_newline();
        }

        // Combine positional-only defaults with regular defaults
        // In the AST, all defaults are stored together, aligned to the right
        let mut all_defaults = posonly_defaults;
        all_defaults.extend(defaults);

        // Validate no duplicate parameter names
        let mut seen_names = std::collections::HashSet::new();

        // Check all parameter types for duplicates
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

        // Also check vararg and kwarg
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

        // Allow newlines after opening bracket
        self.consume_newline();

        while self.peek().kind != TokenKind::RightBracket && !self.is_at_end() {
            let start = self.peek().span.start();
            let name = self.consume_ident()?;

            // Parse bound: T: int or T: (int, str)
            let bound = if self.match_token(TokenKind::Colon) {
                Some(self.parse_expression()?)
            } else {
                None
            };

            // Parse default: T = int
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
            // Allow newlines after comma
            self.consume_newline();
        }

        self.consume(TokenKind::RightBracket)?;

        Ok(self.arena.alloc_slice_vec(type_params))
    }

    // Delimiter tracking methods

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
            // Map opening to expected closing
            let expected_for_opening = match opening.kind {
                '(' => ')',
                '[' => ']',
                '{' => '}',
                _ => opening.kind,
            };

            if expected_for_opening == expected_closing {
                Ok(())
            } else {
                // Mismatched delimiter - put it back on the stack
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
            // No matching opening delimiter
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

    // ===== Error Recovery Methods =====

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

        // Skip tokens until we find a synchronization point or reach limit
        while !self.is_at_end() && tokens_skipped < max_skip {
            let token = self.peek();

            // Check if current token is a sync point
            if SyncPoint::Statement.is_sync_token(&token.kind) {
                // Record the recovery action
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

                // If we stopped at a newline or dedent, consume it to move to the next statement
                if matches!(token.kind, TokenKind::Newline | TokenKind::Dedent) {
                    self.advance();
                }

                return;
            }

            // Also stop at closing delimiters if we're inside a bracketed context
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

        // If we hit the limit or EOF without finding a sync point
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

    /// Get all errors collected during parsing.
    pub fn errors(&self) -> &[Error] {
        &self.errors
    }

    /// Get the recovery manager for statistics.
    pub fn recovery_manager(&self) -> &RecoveryManager {
        &self.recovery_manager
    }

    /// Check if any errors were encountered during parsing.
    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }
}
