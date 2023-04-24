#[derive(Debug, Clone)]
pub enum LoxValueKind {
  Number(f64),
  String(String),
  Boolean(bool),
  Nil,
}

impl std::fmt::Display for LoxValueKind {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      LoxValueKind::Number(number) => number.fmt(f),
      LoxValueKind::String(s) => s.fmt(f),
      LoxValueKind::Boolean(b) => b.fmt(f),
      LoxValueKind::Nil => "nil".fmt(f),
    }
  }
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

  pub fn type_name(&self) -> &'static str {
    match self {
      LoxValueKind::Number(_) => "Number",
      LoxValueKind::String(_) => "String",
      LoxValueKind::Boolean(_) => "Boolean",
      LoxValueKind::Nil => "Nil",
    }
  }
}
