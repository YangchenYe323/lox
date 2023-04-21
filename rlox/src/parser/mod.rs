use crate::{
  ast::{AstNode, AstNodeId, BinaryOp, SyntaxTreeBuilder, UnaryOp},
  lexer::{Token, TokenKind},
  INTERNER,
};

pub struct Parser {
  tokens: Vec<Token>,
  builder: SyntaxTreeBuilder,
  current_idx: usize,
}

impl Parser {
  pub fn new(tokens: Vec<Token>) -> Self {
    Self {
      tokens,
      builder: SyntaxTreeBuilder::default(),
      current_idx: 0,
    }
  }

  pub(crate) fn arena(&self) -> &indextree::Arena<AstNode> {
    &self.builder.arena
  }

  /// expression → equality ;
  pub fn expression(&mut self) -> AstNodeId {
    self.equality()
  }

  /// equality → comparison ( ( "!=" | "==" ) comparison )* ;
  pub fn equality(&mut self) -> AstNodeId {
    let mut base = self.comparison();
    while matches!(self.cur_token().kind, TokenKind::BangEq | TokenKind::EqEq) {
      let op = token_to_binary_op(self.cur_token());
      self.advance();
      let term = self.comparison();
      base = self.builder.binary_expression(base, op, term);
    }
    base
  }

  /// comparison → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
  pub fn comparison(&mut self) -> AstNodeId {
    let mut base = self.term();
    while matches!(
      self.cur_token().kind,
      TokenKind::LessEq | TokenKind::Less | TokenKind::Greater | TokenKind::GreaterEq
    ) {
      let op = token_to_binary_op(self.cur_token());
      self.advance();
      let term = self.term();
      base = self.builder.binary_expression(base, op, term);
    }
    base
  }

  /// term → factor ( ( "-" | "+" ) factor )* ;
  pub fn term(&mut self) -> AstNodeId {
    let mut base = self.factor();
    while matches!(self.cur_token().kind, TokenKind::Plus | TokenKind::Minus) {
      let op = token_to_binary_op(self.cur_token());
      self.advance();
      let term = self.factor();
      base = self.builder.binary_expression(base, op, term);
    }
    base
  }

  /// factor → unary ( ( "/" | "*" ) unary )* ;
  pub fn factor(&mut self) -> AstNodeId {
    let mut base = self.unary();
    while matches!(self.cur_token().kind, TokenKind::Star | TokenKind::Slash) {
      let op = token_to_binary_op(self.cur_token());
      self.advance();
      let term = self.unary();
      base = self.builder.binary_expression(base, op, term);
    }
    base
  }

  ///unary → ( "!" | "-" ) unary
  ///        | primary ;  
  pub fn unary(&mut self) -> AstNodeId {
    match self.cur_token().kind {
      TokenKind::Bang | TokenKind::Minus => {
        let op = token_to_unary_op(self.cur_token());
        self.advance();
        let arg = self.unary();
        self
          .builder
          .unary_expression(self.cur_span_start(), op, arg)
      }
      _ => self.primary(),
    }
  }

  /// primary → NUMBER | STRING | "true" | "false" | "nil"
  ///   | "(" expression ")" ;
  pub fn primary(&mut self) -> AstNodeId {
    let span = self.cur_token().span;
    match self.cur_token().kind {
      TokenKind::Number(symbol, value) => {
        self.advance();
        self.builder.numeric_literal(span, symbol, value)
      }
      TokenKind::Str(symbol) => {
        self.advance();
        let value = INTERNER.with_borrow(|interner| interner.get(symbol));
        self.builder.string_literal(span, symbol, value)
      }
      TokenKind::True => {
        self.advance();
        self.builder.bool_literal(span, true)
      }
      TokenKind::False => {
        self.advance();
        self.builder.bool_literal(span, false)
      }
      TokenKind::Nil => {
        self.advance();
        self.builder.nil(span)
      }
      TokenKind::LParen => {
        self.advance();
        let expr = self.expression();
        self.advance_expect(TokenKind::RParen);
        expr
      }
      _ => {
        unimplemented!()
      }
    }
  }

  pub fn cur_token(&self) -> &Token {
    &self.tokens[self.current_idx]
  }

  pub fn cur_span_start(&self) -> u32 {
    self.cur_token().span.start
  }

  pub fn advance(&mut self) {
    self.current_idx += 1;
  }

  pub fn advance_expect(&mut self, kind: TokenKind) {
    if self.cur_token().kind != kind {
      panic!("Expect kind {:?}, got {:?}", kind, self.cur_token());
    }
    self.advance();
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
