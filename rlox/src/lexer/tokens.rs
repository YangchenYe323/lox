use crate::symbol::SymbolId;

#[derive(Debug)]
pub struct Token {
  kind: TokenKind,
  line: u32,
}

impl Token {
  pub fn new(kind: TokenKind, line: u32) -> Self {
    Self { kind, line }
  }
}

#[derive(Debug)]
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
