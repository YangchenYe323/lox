use std::num::NonZeroUsize;

/// [LoxValueKind] describes the available values of a lox object stored in a variable.
/// Since lox is dynamically typed, every variable is mapped to an [ObjectId], which stores
/// [LoxValueKind]
#[derive(Debug, Clone)]
pub enum LoxValueKind {
  Number(f64),
  String(String),
  Boolean(bool),
  Object(ObjectId),
}

impl std::fmt::Display for LoxValueKind {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      LoxValueKind::Number(number) => number.fmt(f),
      LoxValueKind::String(s) => s.fmt(f),
      LoxValueKind::Boolean(b) => b.fmt(f),
      LoxValueKind::Object(object) => object.fmt(f),
    }
  }
}

impl LoxValueKind {
  #[inline(always)]
  pub fn nil() -> Self {
    Self::Object(ObjectId::Nil)
  }

  pub fn is_truthful(&self) -> bool {
    match self {
      LoxValueKind::Number(n) => *n != 0.0,
      LoxValueKind::String(s) => !s.is_empty(),
      LoxValueKind::Boolean(b) => *b,
      LoxValueKind::Object(o) => !matches!(o, ObjectId::Nil),
    }
  }

  pub fn type_name(&self) -> &'static str {
    match self {
      LoxValueKind::Number(_) => "Number",
      LoxValueKind::String(_) => "String",
      LoxValueKind::Boolean(_) => "Boolean",
      LoxValueKind::Object(o) => match o {
        ObjectId::Nil => "nil",
        ObjectId::Id(_) => "object",
      },
    }
  }
}

/// Lox's abstraction of the concept of "Memory Reference". Each variable is mapped to an [ObjectId],
/// and a lox object might store references to other objects, i.e., they might store [ObjectID]s internally.
/// Note that Nil is a special case of [ObjectId]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ObjectId {
  Nil,
  Id(NonZeroUsize),
}

impl std::fmt::Display for ObjectId {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      ObjectId::Nil => "nil".fmt(f),
      ObjectId::Id(id) => format!("object located at: {}", id).fmt(f),
    }
  }
}
