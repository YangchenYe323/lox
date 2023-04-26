use serde::{
  ser::{SerializeSeq, SerializeStruct},
  Serialize,
};

use crate::ast::AstNodeKind;

use super::{AstNodePtr, Expr};

use rlox_span::{Span, Spanned, SymbolId};

#[derive(Debug, Clone, Copy, Serialize)]
pub enum Stmt<'a> {
  Expr(ExprStmt<'a>),
  Print(PrintStmt<'a>),
  VarDecl(VarDecl<'a>),
  Block(Block<'a>),
  If(IfStmt<'a>),
  While(WhileStmt<'a>),
}

impl<'a> Stmt<'a> {
  pub fn new(ptr: AstNodePtr<'a>) -> Self {
    match &ptr.get().inner {
      AstNodeKind::ExprStmt => Self::Expr(ExprStmt(ptr)),
      AstNodeKind::PrintStmt => Self::Print(PrintStmt(ptr)),
      AstNodeKind::VarDecl(_) => Self::VarDecl(VarDecl(ptr)),
      AstNodeKind::Block => Self::Block(Block(ptr)),
      AstNodeKind::IfStmt => Self::If(IfStmt(ptr)),
      AstNodeKind::WhileStmt => Self::While(WhileStmt(ptr)),
      k => {
        unreachable!(
          "Shouldn't construct statement out of AST Node kind: {:?}",
          k
        );
      }
    }
  }
}

impl<'a> Spanned for Stmt<'a> {
  fn span(&self) -> Span {
    match self {
      Stmt::Expr(s) => s.span(),
      Stmt::Print(s) => s.span(),
      Stmt::VarDecl(s) => s.span(),
      Stmt::Block(s) => s.span(),
      Stmt::If(s) => s.span(),
      Stmt::While(s) => s.span(),
    }
  }
}

#[derive(Debug, Clone, Copy)]
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

impl<'a> Spanned for ExprStmt<'a> {
  fn span(&self) -> Span {
    self.0.span()
  }
}

#[derive(Debug, Clone, Copy)]
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

impl<'a> Spanned for PrintStmt<'a> {
  fn span(&self) -> Span {
    self.0.span()
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

impl<'a> Spanned for VarDecl<'a> {
  fn span(&self) -> Span {
    self.0.span()
  }
}

#[derive(Debug, Clone, Copy)]
pub struct Block<'a>(AstNodePtr<'a>);

impl<'a> Block<'a> {
  pub fn statements(&self) -> Box<dyn Iterator<Item = Stmt<'a>> + 'a> {
    Box::new(self.0.children().map(Stmt::new))
  }
}

impl<'a> Serialize for Block<'a> {
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

impl<'a> Spanned for Block<'a> {
  fn span(&self) -> Span {
    self.0.span()
  }
}

#[derive(Debug, Clone, Copy)]
pub struct IfStmt<'a>(AstNodePtr<'a>);

impl<'a> IfStmt<'a> {
  pub fn pred(&self) -> Expr<'a> {
    let ptr = self.0.nth_child(0).unwrap();
    Expr::new(ptr)
  }

  pub fn then_block(&self) -> Stmt<'a> {
    let ptr = self.0.nth_child(1).unwrap();
    Stmt::new(ptr)
  }

  pub fn else_block(&self) -> Stmt<'a> {
    let ptr = self.0.nth_child(2).unwrap();
    Stmt::new(ptr)
  }
}

impl<'a> Serialize for IfStmt<'a> {
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

impl<'a> Spanned for IfStmt<'a> {
  fn span(&self) -> Span {
    self.0.span()
  }
}

#[derive(Debug, Clone, Copy)]
pub struct WhileStmt<'a>(AstNodePtr<'a>);

impl<'a> WhileStmt<'a> {
  pub fn pred(&self) -> Expr<'a> {
    let ptr = self.0.nth_child(0).unwrap();
    Expr::new(ptr)
  }

  pub fn body(&self) -> Stmt<'a> {
    let ptr = self.0.nth_child(1).unwrap();
    Stmt::new(ptr)
  }
}

impl<'a> Serialize for WhileStmt<'a> {
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

impl<'a> Spanned for WhileStmt<'a> {
  fn span(&self) -> Span {
    self.0.span()
  }
}
