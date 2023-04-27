use serde::{
  ser::{SerializeSeq, SerializeStruct},
  Serialize,
};

use crate::ast::AstNodeKind;

use super::{AstNodePtr, Expr};

use rlox_span::{Span, Spanned, SymbolId};

#[derive(Debug, Clone, Copy, Serialize)]
pub enum Stmt {
  Expr(ExprStmt),
  VarDecl(VarDecl),
  Block(Block),
  If(IfStmt),
  While(WhileStmt),
  Break(BreakStmt),
}

impl Stmt {
  pub fn new(ptr: AstNodePtr) -> Self {
    match &ptr.get().inner {
      AstNodeKind::ExprStmt => Self::Expr(ExprStmt(ptr)),
      AstNodeKind::VarDecl(_) => Self::VarDecl(VarDecl(ptr)),
      AstNodeKind::Block => Self::Block(Block(ptr)),
      AstNodeKind::IfStmt => Self::If(IfStmt(ptr)),
      AstNodeKind::WhileStmt => Self::While(WhileStmt(ptr)),
      AstNodeKind::Break => Self::Break(BreakStmt(ptr)),
      k => {
        unreachable!(
          "Shouldn't construct statement out of AST Node kind: {:?}",
          k
        );
      }
    }
  }
}

impl Spanned for Stmt {
  fn span(&self) -> Span {
    match self {
      Stmt::Expr(s) => s.span(),
      Stmt::VarDecl(s) => s.span(),
      Stmt::Block(s) => s.span(),
      Stmt::If(s) => s.span(),
      Stmt::While(s) => s.span(),
      Stmt::Break(s) => s.span(),
    }
  }
}

#[derive(Debug, Clone, Copy)]
pub struct ExprStmt(AstNodePtr);

impl ExprStmt {
  pub fn expr(&self) -> Expr {
    Expr::new(self.0.nth_child(0).unwrap())
  }
}

impl Serialize for ExprStmt {
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

impl Spanned for ExprStmt {
  fn span(&self) -> Span {
    self.0.span()
  }
}

#[derive(Debug, Clone, Copy)]
pub struct VarDecl(AstNodePtr);

impl VarDecl {
  pub fn var_symbol(&self) -> SymbolId {
    let node = self.0.get();
    match &node.inner {
      AstNodeKind::VarDecl(symbol) => *symbol,
      _ => unreachable!(),
    }
  }

  pub fn init_expr(&self) -> Option<Expr> {
    self.0.nth_child(0).map(Expr::new)
  }
}

impl Serialize for VarDecl {
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

impl Spanned for VarDecl {
  fn span(&self) -> Span {
    self.0.span()
  }
}

#[derive(Debug, Clone, Copy)]
pub struct Block(AstNodePtr);

impl Block {
  pub fn statements(&self) -> Box<dyn Iterator<Item = Stmt>> {
    Box::new(self.0.children().map(Stmt::new))
  }
}

impl Serialize for Block {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: serde::Serializer,
  {
    let mut state = serializer.serialize_seq(None)?;
    for stmt in self.statements() {
      state.serialize_element(&stmt)?;
    }
    state.end()
  }
}

impl Spanned for Block {
  fn span(&self) -> Span {
    self.0.span()
  }
}

#[derive(Debug, Clone, Copy)]
pub struct IfStmt(AstNodePtr);

impl IfStmt {
  pub fn pred(&self) -> Expr {
    let ptr = self.0.nth_child(0).unwrap();
    Expr::new(ptr)
  }

  pub fn then_block(&self) -> Stmt {
    let ptr = self.0.nth_child(1).unwrap();
    Stmt::new(ptr)
  }

  pub fn else_block(&self) -> Stmt {
    let ptr = self.0.nth_child(2).unwrap();
    Stmt::new(ptr)
  }
}

impl Serialize for IfStmt {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: serde::Serializer,
  {
    let p = self.pred();
    let t = self.then_block();
    let e = self.else_block();

    let mut state = serializer.serialize_struct("IfStmt", 3)?;
    state.serialize_field("predicate", &p)?;
    state.serialize_field("then", &t)?;
    state.serialize_field("else", &e)?;

    state.end()
  }
}

impl Spanned for IfStmt {
  fn span(&self) -> Span {
    self.0.span()
  }
}

#[derive(Debug, Clone, Copy)]
pub struct WhileStmt(AstNodePtr);

impl WhileStmt {
  pub fn pred(&self) -> Expr {
    let ptr = self.0.nth_child(0).unwrap();
    Expr::new(ptr)
  }

  pub fn body(&self) -> Stmt {
    let ptr = self.0.nth_child(1).unwrap();
    Stmt::new(ptr)
  }
}

impl Serialize for WhileStmt {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: serde::Serializer,
  {
    let p = self.pred();
    let b = self.body();

    let mut state = serializer.serialize_struct("WhileStmt", 2)?;
    state.serialize_field("predicate", &p)?;
    state.serialize_field("body", &b)?;

    state.end()
  }
}

impl Spanned for WhileStmt {
  fn span(&self) -> Span {
    self.0.span()
  }
}

#[derive(Debug, Clone, Copy)]
pub struct BreakStmt(AstNodePtr);

impl Serialize for BreakStmt {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: serde::Serializer,
  {
    serializer.serialize_str("break")
  }
}

impl Spanned for BreakStmt {
  fn span(&self) -> Span {
    self.0.span()
  }
}
