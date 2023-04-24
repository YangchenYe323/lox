mod diagnostics;

use crate::{
  ast::{AstNodeId, BinaryOp, SyntaxTree, SyntaxTreeBuilder, UnaryOp},
  common::span::Span,
  lexer::{lex_source, Lex, LexerError, Token, TokenKind},
  INTERNER,
};

use self::diagnostics::{unexpected_token, ParserError};

type ParserResult<T, E = ParserError> = std::result::Result<T, E>;

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
      let stmt = self.stmt()?;
      self.builder.add_statement(&program_builder, stmt);
    }
    let end = self.cur_token().span.end;
    Ok(self.builder.finish_program(program_builder, end))
  }

  pub fn stmt(&mut self) -> ParserResult<AstNodeId> {
    match self.cur_token().kind {
      TokenKind::Print => self.print_stmt(),
      _ => self.expr_stmt(),
    }
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

  /// expression → ternary
  pub fn expression(&mut self) -> ParserResult<AstNodeId> {
    self.ternary()
  }

  /// ternary → equality ;
  ///         | equality ? ternary : ternary;
  pub fn ternary(&mut self) -> ParserResult<AstNodeId> {
    let base = self.equality()?;
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
        Ok(self.builder.unary_expression(start, op, arg))
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
        Ok(expr)
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
