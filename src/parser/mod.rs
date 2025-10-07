pub mod ast;

use crate::lexer::token::{Token, TokenType};
use ast::*;

pub struct Parser {
  tokens: Vec<Token>,
  current: usize,
}

impl Parser {
  pub fn new(tokens: Vec<Token>) -> Self {
    Self { tokens, current: 0 }
  }

  fn current_token(&self) -> &Token {
    &self.tokens[self.current]
  }

  fn is_at_end(&self) -> bool {
    self.current_token().token_type == TokenType::Eof
  }

  fn peek(&self) -> &Token {
    if self.current + 1 < self.tokens.len() {
      &self.tokens[self.current + 1]
    } else {
      &self.tokens[self.current]
    }
  }

  fn advance(&mut self) -> &Token {
    if !self.is_at_end() {
      self.current += 1;
    }
    &self.tokens[self.current - 1]
  }

  fn check(&self, token_type: TokenType) -> bool {
    if self.is_at_end() {
      return false;
    }
    self.current_token().token_type == token_type
  }

  fn match_token(&mut self, types: &[TokenType]) -> bool {
    for t in types {
      if self.check(t.clone()) {
        self.advance();
        return true;
      }
    }
    false
  }

  fn consume(&mut self, token_type: TokenType, message: &str) -> Result<&Token, String> {
    if self.check(token_type) {
      Ok(self.advance())
    } else {
      Err(format!(
        "{} at line {} column {}. Got {:?}",
        message,
        self.current_token().line,
        self.current_token().column,
        self.current_token().token_type
      ))
    }
  }

  // Main parsing entry point
  pub fn parse(&mut self) -> Result<Program, String> {
    let mut statements = Vec::new();

    while !self.is_at_end() {
      statements.push(self.statement()?);
    }

    Ok(Program { statements })
  }

  // Parse statement
  fn statement(&mut self) -> Result<Stmt, String> {
    // Module declaration
    if self.match_token(&[TokenType::Module]) {
      return self.module_statement();
    }

    // Import statement
    if self.match_token(&[TokenType::Import]) {
      return self.import_statement();
    }

    // Export statement
    if self.match_token(&[TokenType::Export]) {
      return self.export_statement();
    }

    // Function declaration
    if self.match_token(&[TokenType::Fn]) {
      return self.function_statement();
    }

    // Type definition
    if self.match_token(&[TokenType::Type]) {
      return self.type_statement();
    }

    // Variable declaration (mut or immutable)
    if self.check(TokenType::Ident) {
      let next = self.peek();
      if next.token_type == TokenType::Eq || next.token_type == TokenType::Colon {
        return self.variable_statement(false);
      }
    }

    if self.match_token(&[TokenType::Mut]) {
      return self.variable_statement(true);
    }

    // Return statement
    if self.match_token(&[TokenType::Return]) {
      return self.return_statement();
    }

    // For loop
    if self.match_token(&[TokenType::For]) {
      return self.for_statement();
    }

    // While loop
    if self.match_token(&[TokenType::While]) {
      return self.while_statement();
    }

    // Expression statement
    let expr = self.expression()?;
    Ok(Stmt::Expr(expr))
  }

  fn module_statement(&mut self) -> Result<Stmt, String> {
    let name_token = self.consume(TokenType::Ident, "Expected module name")?;
    let name = name_token.lexeme.clone();
    Ok(Stmt::Module(name))
  }

  fn import_statement(&mut self) -> Result<Stmt, String> {
    let mut items = Vec::new();

    // Check for "type" keyword
    let is_type_import = self.match_token(&[TokenType::Type]);

    // Parse import items
    loop {
      let name_token = self.consume(TokenType::Ident, "Expected identifier")?;
      let name = name_token.lexeme.clone();

      if self.match_token(&[TokenType::As]) {
        let alias_token = self.consume(TokenType::Ident, "Expected alias")?;
        let alias = alias_token.lexeme.clone();

        if is_type_import {
          items.push(ImportItem::TypeAs { name, alias });
        } else {
          items.push(ImportItem::ItemAs { name, alias });
        }
      } else if is_type_import {
        items.push(ImportItem::Type(name));
      } else if name.chars().next().unwrap().is_uppercase() {
        items.push(ImportItem::Module(name));
      } else {
        items.push(ImportItem::Item(name));
      }

      if !self.match_token(&[TokenType::Comma]) {
        break;
      }
    }

    self.consume(TokenType::From, "Expected 'from'")?;
    let path_token = self.consume(TokenType::String, "Expected import path")?;
    let from = path_token.lexeme.clone();

    Ok(Stmt::Import { items, from })
  }

  fn export_statement(&mut self) -> Result<Stmt, String> {
    let mut items = Vec::new();

    // Simple export list for now
    loop {
      let name_token = self.consume(TokenType::Ident, "Expected identifier")?;
      items.push(name_token.lexeme.clone());

      if !self.match_token(&[TokenType::Comma]) {
        break;
      }
    }

    Ok(Stmt::Export(items))
  }

  fn function_statement(&mut self) -> Result<Stmt, String> {
    let name_token = self.consume(TokenType::Ident, "Expected function name")?;
    let name = name_token.lexeme.clone();

    self.consume(TokenType::LParen, "Expected '(' after function name")?;

    let mut params = Vec::new();
    if !self.check(TokenType::RParen) {
      loop {
        let param_name_token = self.consume(TokenType::Ident, "Expected parameter name")?;
        let param_name = param_name_token.lexeme.clone();

        let type_annotation = if self.match_token(&[TokenType::Colon]) {
          Some(self.parse_type()?)
        } else {
          None
        };

        params.push(Param {
          name: param_name,
          type_annotation,
        });

        if !self.match_token(&[TokenType::Comma]) {
          break;
        }
      }
    }

    self.consume(TokenType::RParen, "Expected ')' after parameters")?;

    let return_type = if self.match_token(&[TokenType::Arrow]) {
      Some(self.parse_type()?)
    } else {
      None
    };

    self.consume(TokenType::Do, "Expected 'do' before function body")?;

    let mut body = Vec::new();
    while !self.check(TokenType::End) && !self.is_at_end() {
      body.push(self.statement()?);
    }

    self.consume(TokenType::End, "Expected 'end' after function body")?;

    Ok(Stmt::Function {
      name,
      params,
      return_type,
      body,
    })
  }

  fn type_statement(&mut self) -> Result<Stmt, String> {
    let name_token = self.consume(TokenType::Ident, "Expected type name")?;
    let name = name_token.lexeme.clone();

    self.consume(TokenType::Eq, "Expected '=' after type name")?;

    if self.match_token(&[TokenType::LBrace]) {
      let mut fields = Vec::new();
      while !self.check(TokenType::RBrace) && !self.is_at_end() {
        let field_name_token = self.consume(TokenType::Ident, "Expected field name")?;
        let field_name = field_name_token.lexeme.clone();

        self.consume(TokenType::Colon, "Expected ':' after field name")?;

        let field_type = self.parse_type()?;

        fields.push((field_name, field_type));

        if !self.match_token(&[TokenType::Comma]) {
          break;
        }
      }

      self.consume(TokenType::RBrace, "Expected '}' after struct fields")?;

      return Ok(Stmt::TypeDef {
        name,
        type_def: TypeDefinition::Struct { fields },
      });
    }

    if self.check(TokenType::Ident) && self.current_token().lexeme == "enum" {
      self.advance(); // consume 'enum'
      self.consume(TokenType::LBrace, "Expected '{' after enum keyword")?;

      let mut variants = Vec::new();
      while !self.check(TokenType::RBrace) && !self.is_at_end() {
        let variant_token = self.consume(TokenType::Ident, "Expected variant name")?;
        let variant_name = variant_token.lexeme.clone();

        let mut args = Vec::new();
        if self.match_token(&[TokenType::LParen]) {
          if !self.check(TokenType::RParen) {
            loop {
              args.push(self.parse_type()?);
              if !self.match_token(&[TokenType::Comma]) {
                break;
              }
            }
          }
          self.consume(TokenType::RParen, "Expected ')' after variant arguments")?;
        }

        variants.push((variant_name, args));

        if !self.match_token(&[TokenType::Comma]) {
          break;
        }
      }

      self.consume(TokenType::RBrace, "Expected '}' after enum variants")?;

      return Ok(Stmt::TypeDef {
        name,
        type_def: TypeDefinition::Enum { variants },
      });
    }

    let alias_type = self.parse_type()?;
    Ok(Stmt::TypeDef {
      name,
      type_def: TypeDefinition::Alias(alias_type),
    })
  }

  fn variable_statement(&mut self, mutable: bool) -> Result<Stmt, String> {
    let name_token = self.consume(TokenType::Ident, "Expected variable name")?;
    let name = name_token.lexeme.clone();

    let type_annotation = if self.match_token(&[TokenType::Colon]) {
      Some(self.parse_type()?)
    } else {
      None
    };

    self.consume(TokenType::Eq, "Expected '=' after variable name")?;

    let initializer = self.expression()?;

    Ok(Stmt::Let {
      name,
      mutable,
      type_annotation,
      initializer,
    })
  }

  fn return_statement(&mut self) -> Result<Stmt, String> {
    if self.check(TokenType::End) || self.is_at_end() {
      return Ok(Stmt::Return(None));
    }

    let value = self.expression()?;
    Ok(Stmt::Return(Some(value)))
  }

  fn for_statement(&mut self) -> Result<Stmt, String> {
    let var_token = self.consume(TokenType::Ident, "Expected variable name")?;
    let var = var_token.lexeme.clone();

    self.consume(TokenType::In, "Expected 'in' after variable")?;

    let iter = self.expression()?;

    self.consume(TokenType::Do, "Expected 'do' before loop body")?;

    let mut body = Vec::new();
    while !self.check(TokenType::End) && !self.is_at_end() {
      body.push(self.statement()?);
    }

    self.consume(TokenType::End, "Expected 'end' after loop body")?;

    Ok(Stmt::For { var, iter, body })
  }

  fn while_statement(&mut self) -> Result<Stmt, String> {
    let condition = self.expression()?;

    self.consume(TokenType::Do, "Expected 'do' before loop body")?;

    let mut body = Vec::new();
    while !self.check(TokenType::End) && !self.is_at_end() {
      body.push(self.statement()?);
    }

    self.consume(TokenType::End, "Expected 'end' after loop body")?;

    Ok(Stmt::While { condition, body })
  }

  // Expression parsing
  fn expression(&mut self) -> Result<Expr, String> {
    self.assignment()
  }

  fn assignment(&mut self) -> Result<Expr, String> {
    let expr = self.pipeline()?;

    // Check for assignment
    if self.check(TokenType::Eq) {
      if let Expr::Ident(name) = expr {
        self.advance(); // consume =
        let value = Box::new(self.assignment()?);
        return Ok(Expr::Assign { name, value });
      }
    }

    Ok(expr)
  }

  fn pipeline(&mut self) -> Result<Expr, String> {
    let mut expr = self.or()?;

    while self.match_token(&[TokenType::Dot]) {
      if self.check(TokenType::Ident) {
        let method_name = self.advance().lexeme.clone();

        // Check if it's a method call
        if self.match_token(&[TokenType::LParen]) {
          let mut args = Vec::new();

          if !self.check(TokenType::RParen) {
            loop {
              args.push(self.expression()?);
              if !self.match_token(&[TokenType::Comma]) {
                break;
              }
            }
          }

          self.consume(TokenType::RParen, "Expected ')' after arguments")?;

          expr = Expr::MethodCall {
            receiver: Box::new(expr),
            method: method_name,
            args,
          };
        } else {
          // Property access (treat as method call with no args for now)
          expr = Expr::MethodCall {
            receiver: Box::new(expr),
            method: method_name,
            args: vec![],
          };
        }
      }
    }

    Ok(expr)
  }

  fn or(&mut self) -> Result<Expr, String> {
    let mut expr = self.and()?;

    while self.match_token(&[TokenType::Or]) {
      let right = self.and()?;
      expr = Expr::Binary {
        left: Box::new(expr),
        op: BinaryOp::Or,
        right: Box::new(right),
      };
    }

    Ok(expr)
  }

  fn and(&mut self) -> Result<Expr, String> {
    let mut expr = self.equality()?;

    while self.match_token(&[TokenType::And]) {
      let right = self.equality()?;
      expr = Expr::Binary {
        left: Box::new(expr),
        op: BinaryOp::And,
        right: Box::new(right),
      };
    }

    Ok(expr)
  }

  fn equality(&mut self) -> Result<Expr, String> {
    let mut expr = self.comparison()?;

    while self.match_token(&[TokenType::EqEq, TokenType::NotEq]) {
      let op = match self.tokens[self.current - 1].token_type {
        TokenType::EqEq => BinaryOp::Equal,
        TokenType::NotEq => BinaryOp::NotEqual,
        _ => unreachable!(),
      };
      let right = self.comparison()?;
      expr = Expr::Binary {
        left: Box::new(expr),
        op,
        right: Box::new(right),
      };
    }

    Ok(expr)
  }

  fn comparison(&mut self) -> Result<Expr, String> {
    let mut expr = self.term()?;

    while self.match_token(&[
      TokenType::Lt,
      TokenType::LtEq,
      TokenType::Gt,
      TokenType::GtEq,
    ]) {
      let op = match self.tokens[self.current - 1].token_type {
        TokenType::Lt => BinaryOp::Less,
        TokenType::LtEq => BinaryOp::LessEqual,
        TokenType::Gt => BinaryOp::Greater,
        TokenType::GtEq => BinaryOp::GreaterEqual,
        _ => unreachable!(),
      };
      let right = self.term()?;
      expr = Expr::Binary {
        left: Box::new(expr),
        op,
        right: Box::new(right),
      };
    }

    Ok(expr)
  }

  fn term(&mut self) -> Result<Expr, String> {
    let mut expr = self.factor()?;

    while self.match_token(&[TokenType::Plus, TokenType::Minus]) {
      let op = match self.tokens[self.current - 1].token_type {
        TokenType::Plus => BinaryOp::Add,
        TokenType::Minus => BinaryOp::Subtract,
        _ => unreachable!(),
      };
      let right = self.factor()?;
      expr = Expr::Binary {
        left: Box::new(expr),
        op,
        right: Box::new(right),
      };
    }

    Ok(expr)
  }

  fn factor(&mut self) -> Result<Expr, String> {
    let mut expr = self.unary()?;

    while self.match_token(&[TokenType::Star, TokenType::Slash, TokenType::Percent]) {
      let op = match self.tokens[self.current - 1].token_type {
        TokenType::Star => BinaryOp::Multiply,
        TokenType::Slash => BinaryOp::Divide,
        TokenType::Percent => BinaryOp::Modulo,
        _ => unreachable!(),
      };
      let right = self.unary()?;
      expr = Expr::Binary {
        left: Box::new(expr),
        op,
        right: Box::new(right),
      };
    }

    Ok(expr)
  }

  fn unary(&mut self) -> Result<Expr, String> {
    if self.match_token(&[TokenType::Not, TokenType::Minus]) {
      let op = match self.tokens[self.current - 1].token_type {
        TokenType::Not => UnaryOp::Not,
        TokenType::Minus => UnaryOp::Minus,
        _ => unreachable!(),
      };
      let expr = self.unary()?;
      return Ok(Expr::Unary {
        op,
        expr: Box::new(expr),
      });
    }

    self.call()
  }

  fn call(&mut self) -> Result<Expr, String> {
    let mut expr = self.primary()?;

    loop {
      if self.match_token(&[TokenType::LParen]) {
        let mut args = Vec::new();

        if !self.check(TokenType::RParen) {
          loop {
            args.push(self.expression()?);
            if !self.match_token(&[TokenType::Comma]) {
              break;
            }
          }
        }

        self.consume(TokenType::RParen, "Expected ')' after arguments")?;

        expr = Expr::Call {
          callee: Box::new(expr),
          args,
        };
      } else if self.match_token(&[TokenType::LBracket]) {
        let index = Box::new(self.expression()?);
        self.consume(TokenType::RBracket, "Expected ']' after index")?;
        expr = Expr::Index {
          object: Box::new(expr),
          index,
        };
      } else {
        break;
      }
    }

    Ok(expr)
  }

  fn primary(&mut self) -> Result<Expr, String> {
    // Boolean literals
    if self.match_token(&[TokenType::True]) {
      return Ok(Expr::Bool(true));
    }
    if self.match_token(&[TokenType::False]) {
      return Ok(Expr::Bool(false));
    }

    // Number literals
    if self.match_token(&[TokenType::Int]) {
      let value = self.tokens[self.current - 1]
        .lexeme
        .parse::<i64>()
        .map_err(|e| format!("Invalid integer: {}", e))?;

      // Check for range after integer
      if self.match_token(&[TokenType::DotDot]) {
        let end = self.primary()?;
        return Ok(Expr::Range {
          start: Box::new(Expr::Int(value)),
          end: Box::new(end),
          inclusive: false,
        });
      }

      if self.match_token(&[TokenType::DotDotEq]) {
        let end = self.primary()?;
        return Ok(Expr::Range {
          start: Box::new(Expr::Int(value)),
          end: Box::new(end),
          inclusive: true,
        });
      }

      return Ok(Expr::Int(value));
    }

    if self.match_token(&[TokenType::Float]) {
      let value = self.tokens[self.current - 1]
        .lexeme
        .parse::<f64>()
        .map_err(|e| format!("Invalid float: {}", e))?;
      return Ok(Expr::Float(value));
    }

    // String literals
    if self.match_token(&[TokenType::String]) {
      let string = self.tokens[self.current - 1].lexeme.clone();

      // Check for string interpolation
      if string.contains('{') {
        return Ok(self.parse_string_interpolation(&string));
      }

      return Ok(Expr::String(string));
    }

    // Identifiers
    if self.match_token(&[TokenType::Ident]) {
      let name = self.tokens[self.current - 1].lexeme.clone();
      return Ok(Expr::Ident(name));
    }

    // List literal
    if self.match_token(&[TokenType::LBracket]) {
      let mut elements = Vec::new();

      if !self.check(TokenType::RBracket) {
        loop {
          elements.push(self.expression()?);
          if !self.match_token(&[TokenType::Comma]) {
            break;
          }
        }
      }

      self.consume(TokenType::RBracket, "Expected ']' after list elements")?;
      return Ok(Expr::List(elements));
    }

    // Map literal
    if self.match_token(&[TokenType::LBrace]) {
      let mut pairs = Vec::new();

      if !self.check(TokenType::RBrace) {
        loop {
          let key = self.expression()?;
          self.consume(TokenType::Colon, "Expected ':' after map key")?;
          let value = self.expression()?;
          pairs.push((key, value));

          if !self.match_token(&[TokenType::Comma]) {
            break;
          }
        }
      }

      self.consume(TokenType::RBrace, "Expected '}' after map literal")?;
      return Ok(Expr::Map(pairs));
    }

    // Grouping (parentheses)
    if self.match_token(&[TokenType::LParen]) {
      let expr = self.expression()?;
      self.consume(TokenType::RParen, "Expected ')' after expression")?;
      return Ok(Expr::Grouping(Box::new(expr)));
    }

    // If expression
    if self.match_token(&[TokenType::If]) {
      return self.if_expression();
    }

    // Match expression
    if self.match_token(&[TokenType::Match]) {
      return self.match_expression();
    }

    // Lambda
    if self.match_token(&[TokenType::Pipe]) {
      return self.lambda_expression();
    }

    Err(format!(
      "Unexpected token at line {} column {}: {:?}",
      self.current_token().line,
      self.current_token().column,
      self.current_token().token_type
    ))
  }

  fn if_expression(&mut self) -> Result<Expr, String> {
    let condition = self.expression()?;

    self.consume(TokenType::Do, "Expected 'do' after if condition")?;

    let mut then_branch = Vec::new();
    while !self.check(TokenType::Else) && !self.check(TokenType::End) && !self.is_at_end() {
      then_branch.push(self.statement()?);
    }

    let else_branch = if self.match_token(&[TokenType::Else]) {
      let mut else_stmts = Vec::new();
      while !self.check(TokenType::End) && !self.is_at_end() {
        else_stmts.push(self.statement()?);
      }
      Some(else_stmts)
    } else {
      None
    };

    self.consume(TokenType::End, "Expected 'end' after if expression")?;

    Ok(Expr::If {
      condition: Box::new(condition),
      then_branch,
      else_branch,
    })
  }

  fn match_expression(&mut self) -> Result<Expr, String> {
    let expr = self.expression()?;

    self.consume(TokenType::Do, "Expected 'do' after match expression")?;

    let mut arms = Vec::new();

    while !self.check(TokenType::End) && !self.is_at_end() {
      let pattern = self.parse_pattern()?;

      let guard = if self.match_token(&[TokenType::When]) {
        Some(Box::new(self.expression()?))
      } else {
        None
      };

      self.consume(TokenType::Arrow, "Expected '->' after pattern")?;

      // For now, single expression body
      let body_expr = self.expression()?;
      let body = vec![Stmt::Expr(body_expr)];

      arms.push(MatchArm {
        pattern,
        guard,
        body,
      });

      // Optional comma between arms
      self.match_token(&[TokenType::Comma]);
    }

    self.consume(TokenType::End, "Expected 'end' after match expression")?;

    Ok(Expr::Match {
      expr: Box::new(expr),
      arms,
    })
  }

  fn lambda_expression(&mut self) -> Result<Expr, String> {
    let mut params = Vec::new();

    if !self.check(TokenType::Pipe) {
      loop {
        let param_token = self.consume(TokenType::Ident, "Expected parameter name")?;
        params.push(param_token.lexeme.clone());

        if !self.match_token(&[TokenType::Comma]) {
          break;
        }
      }
    }

    self.consume(TokenType::Pipe, "Expected '|' after lambda parameters")?;

    // For now, single expression body
    let body_expr = self.expression()?;
    let body = vec![Stmt::Return(Some(body_expr))];

    Ok(Expr::Lambda { params, body })
  }

  fn parse_pattern(&mut self) -> Result<Pattern, String> {
    // Wildcard
    if self.check(TokenType::Ident) && self.current_token().lexeme == "_" {
      self.advance();
      return Ok(Pattern::Wildcard);
    }

    // Boolean
    if self.match_token(&[TokenType::True]) {
      return Ok(Pattern::Bool(true));
    }
    if self.match_token(&[TokenType::False]) {
      return Ok(Pattern::Bool(false));
    }

    // Number
    if self.match_token(&[TokenType::Int]) {
      let value = self.tokens[self.current - 1]
        .lexeme
        .parse::<i64>()
        .map_err(|e| format!("Invalid integer: {}", e))?;
      return Ok(Pattern::Int(value));
    }

    // String
    if self.match_token(&[TokenType::String]) {
      let value = self.tokens[self.current - 1].lexeme.clone();
      return Ok(Pattern::String(value));
    }

    // Identifier or variant
    if self.match_token(&[TokenType::Ident]) {
      let name = self.tokens[self.current - 1].lexeme.clone();

      // Check for variant with args
      if self.match_token(&[TokenType::LParen]) {
        let mut args = Vec::new();

        if !self.check(TokenType::RParen) {
          loop {
            args.push(self.parse_pattern()?);
            if !self.match_token(&[TokenType::Comma]) {
              break;
            }
          }
        }

        self.consume(TokenType::RParen, "Expected ')' after variant args")?;

        return Ok(Pattern::Variant { name, args });
      }

      return Ok(Pattern::Ident(name));
    }

    Err(format!(
      "Expected pattern at line {}",
      self.current_token().line
    ))
  }

  fn parse_type(&mut self) -> Result<Type, String> {
    let name_token = self.consume(TokenType::Ident, "Expected type name")?;
    let name = name_token.lexeme.clone();

    // Check for generic type arguments
    if self.match_token(&[TokenType::Lt]) {
      let mut args = Vec::new();

      loop {
        args.push(self.parse_type()?);
        if !self.match_token(&[TokenType::Comma]) {
          break;
        }
      }

      self.consume(TokenType::Gt, "Expected '>' after type arguments")?;

      return Ok(Type::Generic { name, args });
    }

    // Check for function type
    if self.match_token(&[TokenType::Arrow]) {
      let return_type = Box::new(self.parse_type()?);
      return Ok(Type::Function {
        params: vec![Type::Simple(name)],
        return_type,
      });
    }

    Ok(Type::Simple(name))
  }

  #[allow(clippy::while_let_on_iterator)]
  fn parse_string_interpolation(&self, string: &str) -> Expr {
    let mut parts = Vec::new();
    let mut current = String::new();
    let mut chars = string.chars().peekable();

    while let Some(ch) = chars.next() {
      if ch == '{' {
        // Save current text part
        if !current.is_empty() {
          parts.push(StringPart::Text(current.clone()));
          current.clear();
        }

        // Parse expression inside {}
        let mut expr_str = String::new();
        let mut depth = 1;

        while let Some(ch) = chars.next() {
          if ch == '{' {
            depth += 1;
            expr_str.push(ch);
          } else if ch == '}' {
            depth -= 1;
            if depth == 0 {
              break;
            }
            expr_str.push(ch);
          } else {
            expr_str.push(ch);
          }
        }

        // Create a mini lexer and parser for the expression
        let mut lexer = crate::lexer::Lexer::new(expr_str);
        if let Ok(tokens) = lexer.tokenize() {
          let mut parser = Parser::new(tokens);
          if let Ok(expr) = parser.expression() {
            parts.push(StringPart::Expr(Box::new(expr)));
          }
        }
      } else {
        current.push(ch);
      }
    }

    // Save remaining text
    if !current.is_empty() {
      parts.push(StringPart::Text(current));
    }

    Expr::StringInterpolation { parts }
  }
}
