use std::{iter::Peekable, str::Chars};

use miette::Report;

use self::{
  diagnostics::UnexpectedCharacter,
  tokens::{Token, TokenKind, TokenKind::*},
};

mod diagnostics;
mod tokens;

/// The output of lexing, returning either a stream of tokens
/// or a stream of error diagnostics
#[derive(Debug)]
pub enum Lex {
  /// On success, return a vector of tokens and the last line scanned
  Success(Vec<Token>, u32),
  /// On failure, return all the error diagnostics
  Failure(Vec<Report>),
}

/// [Lexer] accepts a raw stream of characters and output a stream of [Token]
pub struct Lexer<'a> {
  /// Source string cursor
  source: LookaheadCharIter<'a>,
  line: u32,
  tokens: Vec<Token>,
  errors: Vec<Report>,
}

impl<'a> Lexer<'a> {
  pub fn new(source: &'a str, line: u32) -> Lexer<'a> {
    Self {
      source: LookaheadCharIter::new(source),
      line,
      tokens: vec![],
      errors: vec![],
    }
  }

  pub fn scan_tokens(&mut self) {
    while !self.at_end() {
      self.scan_token();
    }
  }

  pub fn into_result(self) -> Lex {
    if self.errors.is_empty() {
      Lex::Success(self.tokens, self.line)
    } else {
      Lex::Failure(self.errors)
    }
  }

  fn scan_token(&mut self) {
    self.advance();

    if self.at_end() {
      self.tokens.push(self.finish_token(Eof));
      return;
    }

    let kind = match self.cur() {
      '(' => LParen,
      ')' => RParen,
      '{' => LBrace,
      '}' => RBrace,
      ',' => Comma,
      '.' => Dot,
      '-' => Minus,
      '+' => Plus,
      ';' => Semicolon,
      '*' => Star,
      '/' if self.peek() == Some('/') => {
        self.skip_comments();
        return;
      }
      '/' => Slash,
      '!' => {
        if self.advance_if_match('=') {
          BangEq
        } else {
          Bang
        }
      }
      '=' => {
        if self.advance_if_match('=') {
          EqEq
        } else {
          Eq
        }
      }
      '<' => {
        if self.advance_if_match('=') {
          LessEq
        } else {
          Less
        }
      }
      '>' => {
        if self.advance_if_match('=') {
          GreaterEq
        } else {
          Greater
        }
      }
      c if c.is_whitespace() => {
        self.skip_whitespaces();
        return;
      }
      c => {
        self.errors.push(UnexpectedCharacter(c, self.line).into());
        return;
      }
    };

    self.tokens.push(self.finish_token(kind));
  }

  #[inline(always)]
  fn at_end(&mut self) -> bool {
    self.source.at_end()
  }

  #[inline(always)]
  fn cur(&self) -> char {
    self.source.current().unwrap()
  }

  #[inline(always)]
  fn advance(&mut self) {
    self.source.advance();
  }

  fn advance_if_match(&mut self, target: char) -> bool {
    if self.peek() == Some(target) {
      self.advance();
      true
    } else {
      false
    }
  }

  #[inline(always)]
  fn peek(&mut self) -> Option<char> {
    self.source.peek()
  }

  #[inline(always)]
  fn finish_token(&self, kind: TokenKind) -> Token {
    Token::new(kind, self.line)
  }

  /// Skip line comments until (not consuming) the '\n' or eof after the comments
  fn skip_comments(&mut self) {
    // we are at //
    if self.cur() == '/' && self.peek() == Some('/') {
      while self.peek().map_or(false, |c| c != '\n') {
        self.advance();
      }
    }
  }

  /// Skip all whitespace characters.
  /// Precondition: we are currently looking at a whitespace
  fn skip_whitespaces(&mut self) {
    while self.cur().is_whitespace() {
      if self.cur() == '\n' {
        self.line += 1;
      }
      if self.at_end() {
        break;
      }
      self.advance();
    }
  }
}

#[derive(Debug)]
pub struct LookaheadCharIter<'a> {
  chars: Peekable<Chars<'a>>,
  current: Option<char>,
}

impl<'a> LookaheadCharIter<'a> {
  pub fn new(source: &'a str) -> LookaheadCharIter<'a> {
    Self {
      chars: source.chars().peekable(),
      current: None,
    }
  }

  pub fn advance(&mut self) {
    self.current = self.chars.next();
  }

  pub fn at_end(&mut self) -> bool {
    matches!((self.current, self.chars.peek()), (None, None))
  }

  pub fn peek(&mut self) -> Option<char> {
    self.chars.peek().cloned()
  }

  pub fn current(&self) -> Option<char> {
    self.current
  }
}
