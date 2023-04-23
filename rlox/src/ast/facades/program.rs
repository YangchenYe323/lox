use serde::{ser::SerializeSeq, Serialize};

use crate::ast::AstNodeKind;

use super::{stmt::Stmt, AstNodePtr};

pub struct Program<'a>(AstNodePtr<'a>);

impl<'a> Program<'a> {
  pub fn new(ptr: AstNodePtr<'a>) -> Self {
    match &ptr.get().inner {
      AstNodeKind::Program => Self(ptr),
      _ => unreachable!(),
    }
  }

  pub fn stmts(&self) -> Box<dyn Iterator<Item = Stmt<'a>> + 'a> {
    let children = self.0.children();
    Box::new(children.map(Stmt::new))
  }
}

impl<'a> Serialize for Program<'a> {
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
