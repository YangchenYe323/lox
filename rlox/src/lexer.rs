use std::{iter::Peekable, str::Chars};

use miette::Report;

use self::{
  diagnostics::{Span, UnexpectedCharacter},
  tokens::{Token, TokenKind, TokenKind::*},
};

mod diagnostics;
mod tokens;

/// Utility functions that produce a lex result directly
/// from a source string
pub fn lex<'a>(source: &'a str) -> Lex {
  let mut lexer = Lexer::new(source);
  lexer.scan_tokens();
  lexer.into_result()
}

/// The output of lexing, returning either a stream of tokens
/// or a stream of error diagnostics
#[derive(Debug)]
pub enum Lex {
  /// On success, return a vector of tokens and the last line scanned
  Success(Vec<Token>),
  /// On failure, return all the error diagnostics
  Failure(Vec<Report>),
}

/// [Lexer] accepts a raw stream of characters and output a stream of [Token]
pub struct Lexer<'a> {
  /// Source string cursor
  source: LookaheadCharIter<'a>,
  /// Current line number
  line: u32,
  tokens: Vec<Token>,
  errors: Vec<Report>,
}

impl<'a> Lexer<'a> {
  pub fn new(source: &'a str) -> Lexer<'a> {
    Self {
      source: LookaheadCharIter::new(source),
      line: 1,
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
      Lex::Success(self.tokens)
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
        // This is an unexpected character
        let span = Span::new(self.source.current_pos() - 1, self.source.current_pos());
        self
          .errors
          .push(UnexpectedCharacter(c, self.line, span).into());
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
  current_position: u32,
}

impl<'a> LookaheadCharIter<'a> {
  pub fn new(source: &'a str) -> LookaheadCharIter<'a> {
    Self {
      chars: source.chars().peekable(),
      current: None,
      current_position: 0,
    }
  }

  pub fn advance(&mut self) {
    self.current = self.chars.next();
    self.current_position += 1;
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

  /// The current cursor position. This points at the character AFTER the current
  /// character we're looking at.
  pub fn current_pos(&self) -> u32 {
    self.current_position
  }
}
