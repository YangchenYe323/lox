mod diagnostics;

use crate::{
  ast::{AstNodeId, BinaryOp, SyntaxTree, SyntaxTreeBuilder, UnaryOp},
  lexer::{lex_source, Lex, LexerError, Token, TokenKind},
  INTERNER,
};

use rlox_span::Span;

use self::diagnostics::{unexpected_token, ParserError};

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
}

impl Parser {
  pub fn new(tokens: Vec<Token>) -> Self {
    Self {
      tokens,
      builder: SyntaxTreeBuilder::default(),
      current_idx: 0,
      recovered_errors: vec![],
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
  ///             | statement ;
  pub fn decl(&mut self) -> ParserResult<AstNodeId> {
    match self.cur_token().kind {
      TokenKind::Var => self.var_decl(),
      _ => self.stmt(),
    }
  }

  /// varDecl → "var" IDENTIFIER ( "=" expression )? ";" ;
  pub fn var_decl(&mut self) -> ParserResult<AstNodeId> {
    let start = self.cur_span_start();
    // Advance var
    self.advance();
    let identifier = match self.cur_token().kind {
      TokenKind::Ident(symbol) => symbol,
      _ => return Err(unexpected_token(self.cur_token())),
    };
    self.advance();

    let init = if self.advance_if_match(TokenKind::Eq) {
      Some(self.expression()?)
    } else {
      None
    };

    self.automatic_semicolon_insertion()?;
    let end = self.prev_token().span.end;

    Ok(
      self
        .builder
        .variable_declaration(Span::new(start, end), identifier, init),
    )
  }

  /// statement → exprStmt
  ///           | printStmt
  ///           | block ;
  ///           | ifStmt;
  pub fn stmt(&mut self) -> ParserResult<AstNodeId> {
    match self.cur_token().kind {
      TokenKind::Print => self.print_stmt(),
      TokenKind::LBrace => self.block(),
      TokenKind::If => self.if_stmt(),
      _ => self.expr_stmt(),
    }
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

  /// printStmt → print expression ";" ;
  pub fn print_stmt(&mut self) -> ParserResult<AstNodeId> {
    let start = self.cur_span_start();
    // Advance over print
    self.advance();
    let expr = self.expression()?;

    self.automatic_semicolon_insertion()?;

    let end = self.prev_token().span.end;
    Ok(self.builder.print_statement(Span::new(start, end), expr))
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
        .binary_expression(base, BinaryOp::LogicOr, operand);
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
        .binary_expression(base, BinaryOp::LogicAnd, operand);
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
  ///        | primary ;  
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
      _ => self.primary(),
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
