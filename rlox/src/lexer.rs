use std::str::Chars;

use miette::Report;

use crate::INTERNER;

use self::{
  diagnostics::{Span, UnexpectedCharacter, UnterminatedString},
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
  char_reader: LookaheadCharIter<'a>,
  /// Current line number
  line: u32,
  tokens: Vec<Token>,
  errors: Vec<Report>,
}

impl<'a> Lexer<'a> {
  pub fn new(source: &'a str) -> Lexer<'a> {
    Self {
      char_reader: LookaheadCharIter::new(source),
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

    // After matching, the cursor should fall on the last character WITHIN the matched token
    // so that next time scan_token is called, the leading `advance` skips that token
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
      '"' => {
        let s = self.char_reader.chars.as_str();
        // start_pos points at the first character after the leading "
        let start_pos = self.char_reader.next_pos();
        // Pass the leading "
        self.advance();

        while !self.at_end() && self.cur() != '"' {
          if self.cur() == '\n' {
            self.line += 1;
          }
          self.advance();
        }

        // end_pos points at the first character after the trailing " or the length of the source text
        // if end is reached
        let end_pos = self.char_reader.next_pos() - 1;
        if self.at_end() {
          self
            .errors
            .push(UnterminatedString(Span::new(start_pos, end_pos)).into());
          return;
        }

        let string_range = ..(end_pos - start_pos) as usize;
        let string = &s[string_range];
        let symbol = INTERNER.with_borrow_mut(|interner| interner.intern(string));

        Str(symbol)
      }
      c if c.is_whitespace() => {
        self.skip_whitespaces();
        return;
      }
      c => {
        // This is an unexpected character
        let span = Span::new(self.char_reader.next_pos() - 1, self.char_reader.next_pos());
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
    self.char_reader.at_end()
  }

  #[inline(always)]
  fn cur(&self) -> char {
    self.char_reader.current().unwrap()
  }

  #[inline(always)]
  fn advance(&mut self) {
    self.char_reader.advance();
  }

  #[inline(always)]
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
    self.char_reader.peek()
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
    if self.cur() == '\n' {
      self.line += 1;
    }
    while self.peek().map_or(false, |c| c.is_whitespace()) {
      self.advance();
      if self.cur() == '\n' {
        self.line += 1;
      }
    }
  }
}

#[derive(Debug)]
pub struct LookaheadCharIter<'a> {
  chars: Chars<'a>,
  current: Option<char>,
  next_cursor_position: u32,
}

impl<'a> LookaheadCharIter<'a> {
  pub fn new(source: &'a str) -> LookaheadCharIter<'a> {
    Self {
      chars: source.chars(),
      current: None,
      next_cursor_position: 0,
    }
  }

  pub fn advance(&mut self) {
    self.current = self.chars.next();
    self.next_cursor_position += 1;
  }

  pub fn at_end(&mut self) -> bool {
    matches!((self.current, self.peek()), (None, None))
  }

  pub fn peek(&mut self) -> Option<char> {
    self.chars.clone().next()
  }

  /// The current character we are looking at. None if haven't start looking
  /// or the source is ended
  pub fn current(&self) -> Option<char> {
    self.current
  }

  /// The next cursor position. This points at the character AFTER the current
  /// character we're looking at. Initially this is 0
  pub fn next_pos(&self) -> u32 {
    self.next_cursor_position
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_peek() {
    let mut reader = LookaheadCharIter::new("123");
    assert_eq!(Some('1'), reader.peek());
    reader.advance();
    assert_eq!(Some('1'), reader.current());
    assert_eq!(Some('2'), reader.peek());
  }
}
