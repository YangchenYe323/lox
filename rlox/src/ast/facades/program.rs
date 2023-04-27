use rlox_span::{Span, Spanned};
use serde::{ser::SerializeSeq, Serialize};

use crate::ast::AstNodeKind;

use super::{stmt::Stmt, AstNodePtr};

#[derive(Debug, Clone, Copy)]
pub struct Program(AstNodePtr);

impl Program {
  pub fn new(ptr: AstNodePtr) -> Self {
    match &ptr.get().inner {
      AstNodeKind::Program => Self(ptr),
      _ => unreachable!(),
    }
  }

  pub fn stmts(&self) -> Box<dyn Iterator<Item = Stmt>> {
    let children = self.0.children();
    Box::new(children.map(Stmt::new))
  }
}

impl Serialize for Program {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: serde::Serializer,
  {
    let mut seq = serializer.serialize_seq(None)?;
    for stmt in self.stmts() {
      seq.serialize_element(&stmt)?;
    }
    seq.end()
  }
}

impl Spanned for Program {
  fn span(&self) -> Span {
    self.0.span()
  }
}
