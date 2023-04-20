use std::str::Chars;

use miette::Report;

use crate::INTERNER;

use self::{
  diagnostics::{Span, UnexpectedCharacter, UnterminatedComments, UnterminatedString},
  tokens::{Token, TokenKind, TokenKind::*, valid_token_part, valid_token_start},
};

mod diagnostics;
mod tokens;

/// Utility functions that produce a lex result directly
/// from a source string
pub fn lex_source(source: &'_ str) -> Lex {
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

  /// Perform a complete pass over the source code and generate all
  /// the tokens and errors along the way
  pub fn scan_tokens(&mut self) {
    while !self.at_end() {
      self.scan_token();
    }
  }

  /// Consume lexer and produce lexing result
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
      '/' if self.peek() == Some('*') => {
        self.skip_block_comments();
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
        let end_pos = self.char_reader.current_pos();
        if self.at_end() {
          self
            .errors
            .push(UnterminatedString(Span::new(start_pos, end_pos)).into());
          return;
        }

        let string = self.char_reader.text_at(start_pos, end_pos);
        let symbol = INTERNER.with_borrow_mut(|interner| interner.intern(string));

        Str(symbol)
      }
      c if c.is_ascii_digit() => {
        let start_pos = self.char_reader.current_pos();
        while self.peek().map_or(false, |c| c.is_ascii_digit()) {
          self.advance();
        }

        // Here we are sitting at the last digit before potential dot if any
        if self.peek().map_or(false, |c| c == '.')
          && self.peek_second().map_or(false, |c| c.is_ascii_digit())
        {
          // Match floating point numbers
          // advance dot
          self.advance();
          while self.peek().map_or(false, |c| c.is_ascii_digit()) {
            self.advance();
          }
        }

        let end_pos = self.char_reader.next_pos();
        let string = self.char_reader.text_at(start_pos, end_pos);
        let value: f64 = string.parse().unwrap();
        let symbol = INTERNER.with_borrow_mut(|interner| interner.intern(string));
        Number(symbol, value)
      }
      c if valid_token_start(c) => {
        let start_pos = self.char_reader.current_pos();
        while self.peek().map_or(false, valid_token_part) {
          self.advance();
        }
        let end_pos = self.char_reader.next_pos();
        let s = self.char_reader.text_at(start_pos, end_pos);
        Self::keyword_or_identifier(s)
      }
      c if c.is_whitespace() => {
        self.skip_whitespaces();
        return;
      }
      c => {
        // This is an unexpected character
        let span = Span::new(self.char_reader.current_pos(), self.char_reader.next_pos());
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
  fn peek_second(&mut self) -> Option<char> {
    self.char_reader.peek_second()
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

  /// Skip block comments.
  /// We are currently looking at the / character in a /*
  fn skip_block_comments(&mut self) {
    let mut stack = 1;
    let start_pos = self.char_reader.current_pos();
    // skip /
    self.advance();
    loop {
      let Some(peek) = self.peek() else { break; };
      match peek {
        '\n' => self.line += 1,
        '/' => {
          if self.peek_second() == Some('*') {
            stack += 1;
            self.advance();
          }
        }
        '*' => {
          if self.peek_second() == Some('/') {
            stack -= 1;
            self.advance();
            if stack == 0 {
              self.advance();
              break;
            }
          }
        }
        _ => (),
      }
      self.advance();
    }

    let end_pos = self.char_reader.current_pos();

    if stack != 0 {
      self
        .errors
        .push(UnterminatedComments(Span::new(start_pos, end_pos)).into());
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

  fn keyword_or_identifier(s: &str) -> TokenKind {
    match s {
      // Keywords
      "and" => And,
      "class" => Class,
      "else" => Else,
      "false" => False,
      "fun" => Fun,
      "for" => For,
      "if" => If,
      "nil" => Nil,
      "or" => Or,
      "print" => Print,
      "return" => Return,
      "super" => Super,
      "this" => This,
      "true" => True,
      "var" => Var,
      "while" => While,
      s => {
        let symbol = INTERNER.with_borrow_mut(|interner| interner.intern(s));
        Ident(symbol)
      }
    }
  }
}

#[derive(Debug)]
pub struct LookaheadCharIter<'a> {
  text: &'a str,
  chars: Chars<'a>,
  current: Option<char>,
  next_cursor_position: u32,
}

impl<'a> LookaheadCharIter<'a> {
  pub fn new(source: &'a str) -> LookaheadCharIter<'a> {
    Self {
      text: source,
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

  pub fn peek_second(&mut self) -> Option<char> {
    let mut it = self.chars.clone();
    it.next();
    it.next()
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

  /// The current cursor position. This points at the current character we are looking at
  ///
  /// # panic
  /// If called before the first advance
  pub fn current_pos(&self) -> u32 {
    self.next_cursor_position.checked_sub(1).unwrap()
  }

  pub fn text_at(&self, start: u32, end: u32) -> &'a str {
    &self.text[start as usize..end as usize]
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
