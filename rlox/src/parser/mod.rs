//! This module implements the grammar of the lox programming language.
//! Operator precedences are directly encoded in the grammar and functions of [Parser]
//! corresponds to productions in the grammar.

mod context;
mod diagnostics;

use rlox_ast::{AstNodeId, BinaryOp, LogicalOp, SyntaxTree, SyntaxTreeBuilder, UnaryOp, INTERNER};

use crate::lexer::{lex_source, Lex, LexerError, Token, TokenKind};

use rlox_span::Span;

use self::{
  context::ParserContextFlags,
  diagnostics::{unexpected_token, ParserError},
};

type ParserResult<T, E = ParserError> = std::result::Result<T, E>;

#[derive(Debug)]
pub enum Parse {
  Success(SyntaxTree),
  ParseError {
    recovered: Vec<ParserError>,
    unrecoverrable: Option<ParserError>,
  },
  LexError(Vec<LexerError>),
}

pub fn parse_source_program(source: &str) -> Parse {
  let lex_result = lex_source(source);
  match lex_result {
    Lex::Success(tokens) => {
      let mut parser = Parser::new(tokens);
      match parser.program() {
        Ok(expr) => match parser.recovered_errors.is_empty() {
          true => {
            let tree = parser.builder.finish(expr);
            Parse::Success(tree)
          }
          false => Parse::ParseError {
            recovered: parser.recovered_errors,
            unrecoverrable: None,
          },
        },
        Err(error) => Parse::ParseError {
          recovered: parser.recovered_errors,
          unrecoverrable: Some(error),
        },
      }
    }
    Lex::Failure(errors) => Parse::LexError(errors),
  }
}

pub struct Parser {
  tokens: Vec<Token>,
  builder: SyntaxTreeBuilder,
  current_idx: usize,
  recovered_errors: Vec<ParserError>,
  context: ParserContextFlags,
}

impl Parser {
  pub fn new(tokens: Vec<Token>) -> Self {
    Self {
      tokens,
      builder: SyntaxTreeBuilder::default(),
      current_idx: 0,
      recovered_errors: vec![],
      context: ParserContextFlags::empty(),
    }
  }

  pub fn program(&mut self) -> ParserResult<AstNodeId> {
    let start = self.cur_span_start();
    let program_builder = self.builder.start_program(start);
    while self.cur_token().kind != TokenKind::Eof {
      let stmt = self.decl()?;
      self.builder.add_statement(&program_builder, stmt);
    }
    let end = self.cur_token().span.end;
    Ok(self.builder.finish_program(program_builder, end))
  }

  /// declaration → varDecl
  ///             | functionDecl
  ///             | statement ;
  pub fn decl(&mut self) -> ParserResult<AstNodeId> {
    match self.cur_token().kind {
      TokenKind::Var => self.var_decl(),
      TokenKind::Fun => self.func_decl(),
      _ => self.stmt(),
    }
  }

  /// funDecl → "fun" function ;
  pub fn func_decl(&mut self) -> ParserResult<AstNodeId> {
    self.with_context(ParserContextFlags::IN_FUNCTION_DECL, |parser| {
      let start = parser.cur_span_start();
      // advance "fun"
      parser.advance();
      let function = parser.func()?;
      Ok(parser.builder.re_span_start(function, start))
    })
  }

  /// function → IDENTIFIER "(" parameters ")" block ;
  pub fn func(&mut self) -> ParserResult<AstNodeId> {
    let start = self.cur_span_start();
    // function name
    let name = match self.cur_token().kind {
      TokenKind::Ident(symbol) => {
        self.advance();
        symbol
      }
      _ => return Err(unexpected_token(self.cur_token())),
    };
    // parameter list
    if !self.advance_if_match(TokenKind::LParen) {
      return Err(unexpected_token(self.cur_token()));
    }
    let parameters = self.parameter_list()?;
    if !self.advance_if_match(TokenKind::RParen) {
      return Err(unexpected_token(self.cur_token()));
    }
    // body
    let body = self.block()?;
    let end = self.prev_token().span.end;
    Ok(
      self
        .builder
        .function_declaration(Span::new(start, end), name, parameters, body),
    )
  }

  /// parameters → IDENTIFIER ( "," IDENTIFIER )* ;
  ///             | empty
  pub fn parameter_list(&mut self) -> ParserResult<AstNodeId> {
    const MAX_PARAMS: u32 = 255;

    let start = self.cur_span_start();
    let params = self.builder.start_parameter_list(start);
    // empty
    if matches!(self.cur_token().kind, TokenKind::RParen) {
      Ok(self.builder.finish_parameter_list(params, start))
    } else {
      let mut cnt = 0;
      loop {
        let param = self.ident_init()?;
        self.builder.add_parameter(&params, param);
        cnt += 1;
        if !self.advance_if_match(TokenKind::Comma) {
          break;
        }
      }
      let end = self.prev_token().span.end;
      if cnt > MAX_PARAMS {
        self
          .recovered_errors
          .push(ParserError::TooManyParameters(Span::new(start, end)));
      }
      Ok(self.builder.finish_parameter_list(params, end))
    }
  }

  /// varDecl →  "var" identInit ;
  pub fn var_decl(&mut self) -> ParserResult<AstNodeId> {
    let start = self.cur_span_start();
    // Advance var
    self.advance();
    let ident_init = self.ident_init()?;
    self.automatic_semicolon_insertion()?;
    let end = self.prev_token().span.end;

    Ok(self.builder.re_span(ident_init, Span::new(start, end)))
  }

  /// statement → exprStmt
  ///           | block
  ///           | ifStmt
  ///           | whileStmt
  ///           | breakStmt
  ///           | returnStmt
  pub fn stmt(&mut self) -> ParserResult<AstNodeId> {
    match self.cur_token().kind {
      TokenKind::LBrace => self.block(),
      TokenKind::If => self.if_stmt(),
      TokenKind::While => self.while_stmt(),
      TokenKind::Break => self.break_stmt(),
      TokenKind::Return => self.return_stmt(),
      _ => self.expr_stmt(),
    }
  }

  pub fn return_stmt(&mut self) -> ParserResult<AstNodeId> {
    let start = self.cur_span_start();
    self.advance();
    let returned =
      if !matches!(self.cur_token().kind, TokenKind::Semicolon) && self.cur_token_on_same_line() {
        self.expression()?
      } else {
        // Note that in our design, we search for return values eagerly including next lines.
        let start = self.prev_token().span.start;
        self.builder.nil(Span::new(start, start))
      };

    self.automatic_semicolon_insertion()?;
    let end = self.prev_token().span.end;
    let span = Span::new(start, end);

    if !self.in_function() {
      return Err(ParserError::ReturnOutsideFunction(span));
    }

    Ok(self.builder.return_statement(span, returned))
  }

  pub fn break_stmt(&mut self) -> ParserResult<AstNodeId> {
    let start = self.cur_span_start();
    self.advance();
    self.automatic_semicolon_insertion()?;
    let end = self.prev_token().span.end;
    let span = Span::new(start, end);
    if !self.in_loop() {
      self
        .recovered_errors
        .push(ParserError::BreakOutsideLoop(span));
    }
    Ok(self.builder.break_statement(span))
  }

  /// whileStmt → "while" expression block ;
  pub fn while_stmt(&mut self) -> ParserResult<AstNodeId> {
    self.with_context(ParserContextFlags::IN_LOOP, |parser| {
      let start = parser.cur_span_start();
      parser.advance();
      let pred = parser.expression()?;
      let body = parser.block()?;
      let end = parser.prev_token().span.end;
      Ok(
        parser
          .builder
          .while_statement(Span::new(start, end), pred, body),
      )
    })
  }

  /// ifStmt → "if" expression  block
  ///           ( "else" block )? ;
  pub fn if_stmt(&mut self) -> ParserResult<AstNodeId> {
    let start = self.cur_span_start();
    // Advance "if"
    self.advance();
    let pred = self.expression()?;
    let conseq = self.block()?;
    let alt = if self.advance_if_match(TokenKind::Else) {
      self.block()?
    } else {
      // build a dummy empty block
      let start = self.prev_token().span.end;
      let end = self.prev_token().span.end;
      let block = self.builder.start_block(start);
      self.builder.finish_block(block, end)
    };
    let end = self.prev_token().span.end;
    Ok(
      self
        .builder
        .if_statement(Span::new(start, end), pred, conseq, alt),
    )
  }

  /// block → "{" declaration* "}" ;
  pub fn block(&mut self) -> ParserResult<AstNodeId> {
    let start = self.cur_span_start();
    let block = self.builder.start_block(start);
    self.advance();
    while !matches!(self.cur_token().kind, TokenKind::RBrace | TokenKind::Eof) {
      let decl = self.decl()?;
      self.builder.add_block_statement(&block, decl);
    }
    if !self.advance_if_match(TokenKind::RBrace) {
      return Err(ParserError::UnexpectedToken(
        self.cur_token().span,
        self.cur_token().kind.to_str(),
      ));
    }
    let end = self.prev_token().span.end;
    let block = self.builder.finish_block(block, end);
    Ok(block)
  }

  /// exprStmt → expression ";" ;
  pub fn expr_stmt(&mut self) -> ParserResult<AstNodeId> {
    let start = self.cur_span_start();
    let expr = self.expression()?;

    self.automatic_semicolon_insertion()?;

    let end = self.prev_token().span.end;
    Ok(
      self
        .builder
        .expression_statement(Span::new(start, end), expr),
    )
  }

  /// expression → assignment
  pub fn expression(&mut self) -> ParserResult<AstNodeId> {
    self.assignment()
  }

  /// assignment → ASSIGN_TARGET "=" assignment
  ///             | ternary
  pub fn assignment(&mut self) -> ParserResult<AstNodeId> {
    let start = self.cur_span_start();
    let base = self.ternary()?;
    if matches!(self.cur_token().kind, TokenKind::Eq) {
      self.advance();
      let value = self.assignment()?;

      // Invalid Assignment is a recoverrable error
      if !self.builder.valid_assign_target(base) {
        self
          .recovered_errors
          .push(ParserError::InvalidAssignment(self.builder.get_span(base)));
      }

      let end = self.prev_token().span.end;
      Ok(
        self
          .builder
          .assignment_expression(Span::new(start, end), base, value),
      )
    } else {
      Ok(base)
    }
  }

  /// ternary → logicOr ;
  ///         | logicOr ? ternary : ternary;
  pub fn ternary(&mut self) -> ParserResult<AstNodeId> {
    let base = self.logic_or()?;
    // ternary expression
    if matches!(self.cur_token().kind, TokenKind::Question) {
      self.advance();
      let consequence = self.ternary()?;
      if !self.advance_if_match(TokenKind::Colon) {
        return Err(unexpected_token(self.cur_token()));
      }
      let alternative = self.ternary()?;
      Ok(
        self
          .builder
          .ternary_expression(base, consequence, alternative),
      )
    } else {
      Ok(base)
    }
  }

  /// logic_or → logic_and ( "or" logic_and )* ;
  pub fn logic_or(&mut self) -> ParserResult<AstNodeId> {
    let mut base = self.logic_and()?;
    while matches!(self.cur_token().kind, TokenKind::Or) {
      self.advance();
      let operand = self.logic_and()?;
      base = self
        .builder
        .logical_expression(base, LogicalOp::Or, operand);
    }
    Ok(base)
  }

  /// logic_and  → equality ( "and" equality )* ;
  pub fn logic_and(&mut self) -> ParserResult<AstNodeId> {
    let mut base = self.equality()?;
    while matches!(self.cur_token().kind, TokenKind::And) {
      self.advance();
      let operand = self.equality()?;
      base = self
        .builder
        .logical_expression(base, LogicalOp::And, operand);
    }
    Ok(base)
  }

  /// equality → comparison ( ( "!=" | "==" ) comparison )* ;
  pub fn equality(&mut self) -> ParserResult<AstNodeId> {
    let mut base = self.comparison()?;
    while matches!(self.cur_token().kind, TokenKind::BangEq | TokenKind::EqEq) {
      let op = token_to_binary_op(self.cur_token());
      self.advance();
      let term = self.comparison()?;
      base = self.builder.binary_expression(base, op, term);
    }
    Ok(base)
  }

  /// comparison → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
  pub fn comparison(&mut self) -> ParserResult<AstNodeId> {
    let mut base = self.term()?;
    while matches!(
      self.cur_token().kind,
      TokenKind::LessEq | TokenKind::Less | TokenKind::Greater | TokenKind::GreaterEq
    ) {
      let op = token_to_binary_op(self.cur_token());
      self.advance();
      let term = self.term()?;
      base = self.builder.binary_expression(base, op, term);
    }
    Ok(base)
  }

  /// term → factor ( ( "-" | "+" ) factor )* ;
  pub fn term(&mut self) -> ParserResult<AstNodeId> {
    let mut base = self.factor()?;
    while matches!(self.cur_token().kind, TokenKind::Plus | TokenKind::Minus) {
      let op = token_to_binary_op(self.cur_token());
      self.advance();
      let term = self.factor()?;
      base = self.builder.binary_expression(base, op, term);
    }
    Ok(base)
  }

  /// factor → unary ( ( "/" | "*" ) unary )* ;
  pub fn factor(&mut self) -> ParserResult<AstNodeId> {
    let mut base = self.unary()?;
    while matches!(self.cur_token().kind, TokenKind::Star | TokenKind::Slash) {
      let op = token_to_binary_op(self.cur_token());
      self.advance();
      let term = self.unary()?;
      base = self.builder.binary_expression(base, op, term);
    }
    Ok(base)
  }

  ///unary → ( "!" | "-" ) unary
  ///        | call;  
  pub fn unary(&mut self) -> ParserResult<AstNodeId> {
    match self.cur_token().kind {
      TokenKind::Bang | TokenKind::Minus => {
        let start = self.cur_span_start();
        let op = token_to_unary_op(self.cur_token());
        self.advance();
        let arg = self.unary()?;
        let end = self.prev_token().span.end;
        Ok(
          self
            .builder
            .unary_expression(Span::new(start, end), op, arg),
        )
      }
      _ => self.call(),
    }
  }

  /// call → primary ( "(" arguments ")" )* ;
  pub fn call(&mut self) -> ParserResult<AstNodeId> {
    let start = self.cur_span_start();
    let mut base = self.primary()?;
    while self.advance_if_match(TokenKind::LParen) {
      let arguments = self.arguments()?;
      if !self.advance_if_match(TokenKind::RParen) {
        return Err(unexpected_token(self.cur_token()));
      }
      let end = self.prev_token().span.end;
      base = self
        .builder
        .function_call(Span::new(start, end), base, arguments);
    }
    Ok(base)
  }

  /// arguments → expression ( "," expression )*;
  ///            | empty
  pub fn arguments(&mut self) -> ParserResult<AstNodeId> {
    const MAX_ARGUMENTS: u32 = 255;

    let start = self.cur_span_start();

    let builder = self.builder.start_argument_list(start);
    let empty_argument = matches!(self.cur_token().kind, TokenKind::RParen);

    if empty_argument {
      Ok(self.builder.finish_argument_list(builder, start))
    } else {
      let mut cnt = 0;
      loop {
        let argument = self.expression()?;
        self.builder.add_argument(&builder, argument);
        cnt += 1;
        if !self.advance_if_match(TokenKind::Comma) {
          break;
        }
      }

      let end: u32 = self.prev_token().span.end;
      if cnt > MAX_ARGUMENTS {
        self
          .recovered_errors
          .push(ParserError::TooManyArguments(Span::new(start, end)));
      }

      Ok(self.builder.finish_argument_list(builder, end))
    }
  }

  /// primary → NUMBER | STRING | "true" | "false" | "nil"
  ///   | "(" expression ")" ;
  pub fn primary(&mut self) -> ParserResult<AstNodeId> {
    let span = self.cur_token().span;
    match self.cur_token().kind {
      TokenKind::Number(symbol, value) => {
        self.advance();
        Ok(self.builder.numeric_literal(span, symbol, value))
      }
      TokenKind::Str(symbol) => {
        self.advance();
        let value = INTERNER.with_borrow(|interner| interner.get(symbol));
        let value = value.strip_prefix('"').unwrap().strip_suffix('"').unwrap();
        Ok(self.builder.string_literal(span, symbol, value))
      }
      TokenKind::True => {
        self.advance();
        Ok(self.builder.bool_literal(span, true))
      }
      TokenKind::False => {
        self.advance();
        Ok(self.builder.bool_literal(span, false))
      }
      TokenKind::Nil => {
        self.advance();
        Ok(self.builder.nil(span))
      }
      TokenKind::LParen => {
        self.advance();
        let expr = self.expression()?;
        if !self.advance_if_match(TokenKind::RParen) {
          return Err(unexpected_token(self.cur_token()));
        }
        let end = self.prev_token().span.end;
        let paren_span = Span::new(span.start, end);
        Ok(self.builder.re_span(expr, paren_span))
      }
      TokenKind::Ident(symbol) => {
        self.advance();
        Ok(self.builder.variable_reference(span, symbol))
      }
      _ => Err(unexpected_token(self.cur_token())),
    }
  }

  /// identInit → IDENTIFIER ( "=" expression )? ";" ;
  pub fn ident_init(&mut self) -> ParserResult<AstNodeId> {
    let start = self.cur_span_start();

    let identifier = match self.cur_token().kind {
      TokenKind::Ident(symbol) => symbol,
      _ => return Err(unexpected_token(self.cur_token())),
    };
    self.advance();

    let init = if self.advance_if_match(TokenKind::Eq) {
      self.expression()?
    } else {
      let end = self.prev_token().span.end;
      self.builder.nil(Span::new(end, end))
    };

    let end = self.prev_token().span.end;
    Ok(
      self
        .builder
        .variable_declaration(Span::new(start, end), identifier, init),
    )
  }

  pub fn cur_token(&self) -> &Token {
    &self.tokens[self.current_idx]
  }

  pub fn prev_token(&self) -> &Token {
    &self.tokens[self.current_idx - 1]
  }

  pub fn cur_span_start(&self) -> u32 {
    self.cur_token().span.start
  }

  pub fn advance(&mut self) {
    self.current_idx += 1;
  }

  pub fn advance_if_match(&mut self, kind: TokenKind) -> bool {
    if self.cur_token().kind == kind {
      self.advance();
      true
    } else {
      false
    }
  }

  /// Performs automatic semicolon insertion at statement boundary
  fn automatic_semicolon_insertion(&mut self) -> ParserResult<()> {
    // already a semicolon, good.
    if self.advance_if_match(TokenKind::Semicolon) {
      return Ok(());
    }
    // reached the end
    if matches!(self.cur_token().kind, TokenKind::Eof) {
      return Ok(());
    }
    // Current token is in another line
    if self.cur_token().line > self.prev_token().line {
      return Ok(());
    }

    Err(ParserError::UnexpectedToken(
      self.cur_token().span,
      self.cur_token().kind.to_str(),
    ))
  }

  fn cur_token_on_same_line(&self) -> bool {
    self.cur_token().line == self.prev_token().line
  }
}

fn token_to_binary_op(token: &Token) -> BinaryOp {
  match &token.kind {
    TokenKind::BangEq => BinaryOp::BangEq,
    TokenKind::EqEq => BinaryOp::EqEq,
    TokenKind::Less => BinaryOp::Less,
    TokenKind::Greater => BinaryOp::Greater,
    TokenKind::LessEq => BinaryOp::LessEq,
    TokenKind::GreaterEq => BinaryOp::GreaterEq,
    TokenKind::Plus => BinaryOp::Plus,
    TokenKind::Minus => BinaryOp::Minus,
    TokenKind::Star => BinaryOp::Mult,
    TokenKind::Slash => BinaryOp::Div,
    _ => unreachable!(),
  }
}

fn token_to_unary_op(token: &Token) -> UnaryOp {
  match &token.kind {
    TokenKind::Bang => UnaryOp::Not,
    TokenKind::Minus => UnaryOp::Neg,
    _ => unreachable!(),
  }
}
