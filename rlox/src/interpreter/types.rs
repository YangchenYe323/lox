use crate::ast::AstNodeId;

#[derive(Debug)]
pub enum LoxValueKind {
  Number(f64),
  String(String),
  Boolean(bool),
  Nil,
}

impl LoxValueKind {
  pub fn is_truthful(&self) -> bool {
    match self {
      LoxValueKind::Number(n) => *n != 0.0,
      LoxValueKind::String(s) => !s.is_empty(),
      LoxValueKind::Boolean(b) => *b,
      LoxValueKind::Nil => false,
    }
  }
}

#[derive(Debug)]
pub struct LoxValue {
  /// The AST Node this value is evaluated for
  pub node: AstNodeId,
  /// Inner types of the value
  pub value: LoxValueKind,
}
