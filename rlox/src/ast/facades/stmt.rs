use serde::{ser::SerializeStruct, Serialize};

use crate::ast::AstNodeKind;

use super::{AstNodePtr, Expr};

use rlox_span::SymbolId;

#[derive(Debug, Serialize)]
pub enum Stmt<'a> {
  Expr(ExprStmt<'a>),
  Print(PrintStmt<'a>),
  VarDecl(VarDecl<'a>),
}

impl<'a> Stmt<'a> {
  pub fn new(ptr: AstNodePtr<'a>) -> Self {
    match &ptr.get().inner {
      AstNodeKind::ExprStmt => Self::Expr(ExprStmt(ptr)),
      AstNodeKind::PrintStmt => Self::Print(PrintStmt(ptr)),
      AstNodeKind::VarDecl(_) => Self::VarDecl(VarDecl(ptr)),
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

#[derive(Debug, Clone, Copy)]
pub struct VarDecl<'a>(AstNodePtr<'a>);

impl<'a> VarDecl<'a> {
  pub fn var_symbol(&self) -> SymbolId {
    let node = self.0.get();
    match &node.inner {
      AstNodeKind::VarDecl(symbol) => *symbol,
      _ => unreachable!(),
    }
  }

  pub fn init_expr(&self) -> Option<Expr<'a>> {
    self.0.nth_child(0).map(Expr::new)
  }
}

impl<'a> Serialize for VarDecl<'a> {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: serde::Serializer,
  {
    let mut state = serializer.serialize_struct("VarDecl", 2)?;
    let ident = self.var_symbol();
    let init = self.init_expr();

    state.serialize_field("ident", &ident)?;
    state.serialize_field("init", &init)?;

    state.end()
  }
}
