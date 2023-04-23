use serde::{ser::SerializeStruct, Serialize};

use crate::ast::AstNodeKind;

use super::{AstNodePtr, Expr};

#[derive(Debug, Serialize)]
pub enum Stmt<'a> {
  ExprStmt(ExprStmt<'a>),
  PrintStmt(PrintStmt<'a>),
}

impl<'a> Stmt<'a> {
  pub fn new(ptr: AstNodePtr<'a>) -> Self {
    match &ptr.get().inner {
      AstNodeKind::ExprStmt => Self::ExprStmt(ExprStmt(ptr)),
      AstNodeKind::PrintStmt => Self::PrintStmt(PrintStmt(ptr)),
      _ => unreachable!(),
    }
  }
}

#[derive(Debug)]
pub struct ExprStmt<'a>(AstNodePtr<'a>);

impl<'a> ExprStmt<'a> {
  pub fn expr(&self) -> Expr<'a> {
    Expr::new(self.0.nth_child(0).unwrap())
  }
}

impl<'a> Serialize for ExprStmt<'a> {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: serde::Serializer,
  {
    let mut state = serializer.serialize_struct("ExprStmt", 1)?;
    let expr = self.expr();
    state.serialize_field("expr", &expr)?;
    state.end()
  }
}

#[derive(Debug)]
pub struct PrintStmt<'a>(AstNodePtr<'a>);

impl<'a> PrintStmt<'a> {
  pub fn expr(&self) -> Expr<'a> {
    Expr::new(self.0.nth_child(0).unwrap())
  }
}

impl<'a> Serialize for PrintStmt<'a> {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: serde::Serializer,
  {
    let mut state = serializer.serialize_struct("PrintStmt", 1)?;
    let expr = self.expr();
    state.serialize_field("expr", &expr)?;
    state.end()
  }
}
