use super::types::Parser;
use crate::ast::*;
use crate::error::{UnifiedError as Error, UnifiedErrorKind as ErrorKind, error};
use crate::lexer::TokenKind;
use smallvec::SmallVec;

pub type ParseResult<T> = Result<T, Box<Error>>;

impl<'a> Parser<'a> {
    pub(super) fn parse_decorated(&mut self) -> ParseResult<Stmt<'a>> {
        let decorators = self.parse_decorators()?;

        let is_async = self.match_token(TokenKind::Async);

        match self.peek().kind {
            TokenKind::Def => self.parse_func_def(decorators, is_async),
            TokenKind::Class => self.parse_class_def(decorators),
            _ => Err(error(
                ErrorKind::UnexpectedToken {
                    expected: None,
                    found: format!("{:?}", self.peek().kind),
                },
                self.peek().span,
            )),
        }
    }

    pub(super) fn parse_decorators(&mut self) -> ParseResult<&'a [Expr<'a>]> {
        // Most classes/functions have 0-4 decorators
        let mut decorators: SmallVec<[Expr<'a>; 4]> = SmallVec::new();

        while self.peek().kind == TokenKind::At {
            self.advance();
            let decorator = self.parse_expression()?;
            self.consume_newline();
            decorators.push(decorator);
        }

        Ok(self.arena.alloc_slice_iter(decorators))
    }
}
