use crate::common::{span::Span, symbol::SymbolId};

#[derive(Debug)]
pub struct Token {
  pub kind: TokenKind,
  pub span: Span,
  pub line: u32,
}

impl std::fmt::Display for Token {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    std::fmt::write(
      f,
      format_args!(
        "Token[{:?}(line-{},{}-{})]",
        self.kind, self.line, self.span.start, self.span.end
      ),
    )
  }
}

impl Token {
  pub fn new(kind: TokenKind, line: u32, span: Span) -> Self {
    Self { kind, span, line }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
  // Single Character Tokens
  LParen,
  RParen,
  LBrace,
  RBrace,
  Comma,
  Dot,
  Minus,
  Plus,
  Semicolon,
  Slash,
  Star,

  // One or two character tokens
  Bang,
  BangEq,
  Eq,
  EqEq,
  Greater,
  GreaterEq,
  Less,
  LessEq,

  // Identifier
  Ident(SymbolId),
  // Literals
  Str(SymbolId),
  Number(SymbolId, f64),

  // Keywords
  And,
  Class,
  Else,
  False,
  Fun,
  For,
  If,
  Nil,
  Or,
  Print,
  Return,
  Super,
  This,
  True,
  Var,
  While,

  // End of file
  Eof,
}

/// Valid characters to be the start of token in lox:
/// [a-z][A-Z]_
#[inline(always)]
pub fn valid_token_start(c: char) -> bool {
  c.is_ascii_alphabetic() || c == '_'
}

/// Valid characters to be part of token (not start) in lox:
/// [a-z][A-Z][0-9]_
#[inline(always)]
pub fn valid_token_part(c: char) -> bool {
  c.is_ascii_alphanumeric() || c == '_'
}
