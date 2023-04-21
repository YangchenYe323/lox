//! Immutable facades of the Abstract Syntax Trees. They are typed views into the raw Syntax Tree
//! facade types provides functinalities like pretty-printing the grammar-aware structure of the
//! syntax tree and grammar-aware access into the syntax tree.

use serde::{ser::SerializeStruct, Serialize};
use thiserror::Error;

use super::{AstNode, AstNodeId, AstNodeKind, BinaryOp, UnaryOp};

/// A shared reference of an [AstNode], which packs together the arena
/// and the id.
#[derive(Debug, Clone, Copy)]
pub struct AstNodePtr<'a> {
  pub arena: &'a indextree::Arena<AstNode>,
  pub cursor: AstNodeId,
}

#[derive(Debug, Error)]
#[error("No such child at index {0}")]
pub struct NoSuchChildError(usize);

impl<'a> AstNodePtr<'a> {
  pub fn new(arena: &'a indextree::Arena<AstNode>, cursor: AstNodeId) -> Self {
    Self { arena, cursor }
  }

  pub fn get(&self) -> &AstNode {
    self.arena[self.cursor.into()].get()
  }

  /// Nth child of the current node, 0-indexed
  pub fn nth_child(&self, n: usize) -> std::result::Result<AstNodePtr<'a>, NoSuchChildError> {
    let mut it = self.cursor.children(self.arena);
    for i in 0..n {
      if it.next().is_none() {
        return Err(NoSuchChildError(i + 1));
      }
    }
    match it.next() {
      Some(id) => Ok(Self::new(self.arena, AstNodeId::from(id))),
      None => Err(NoSuchChildError(n)),
    }
  }
}

#[derive(Debug, Serialize)]
pub enum Expr<'a> {
  Binary(BinaryExpr<'a>),
  Unary(UnaryExpr<'a>),
  String(StringLit<'a>),
  Number(NumericLit<'a>),
  Bool(BoolLit<'a>),
  Nil(NilLit<'a>),
}

impl<'a> Expr<'a> {
  pub fn new(ptr: AstNodePtr<'a>) -> Self {
    match ptr.get().inner {
      AstNodeKind::BinaryExpr(_) => Self::Binary(BinaryExpr(ptr)),
      AstNodeKind::StrLiteral(_, _) => Self::String(StringLit(ptr)),
      AstNodeKind::NumLiteral(_, _) => Self::Number(NumericLit(ptr)),
      AstNodeKind::UnaryExpr(_) => Self::Unary(UnaryExpr(ptr)),
      AstNodeKind::BoolLiteral(_) => Self::Bool(BoolLit(ptr)),
      AstNodeKind::Nil => Self::Nil(NilLit(ptr)),
      // _ => unreachable!(),
    }
  }
}

#[derive(Debug)]
pub struct BinaryExpr<'a>(AstNodePtr<'a>);

impl<'a> BinaryExpr<'a> {
  #[inline(always)]
  pub fn ast_node(&self) -> &AstNode {
    self.0.get()
  }

  pub fn operator(&self) -> BinaryOp {
    match &self.ast_node().inner {
      AstNodeKind::BinaryExpr(op) => *op,
      _ => unreachable!(),
    }
  }

  pub fn left_operand(&self) -> Expr<'a> {
    let left = self.0.nth_child(0).unwrap();
    Expr::new(left)
  }

  pub fn right_operand(&self) -> Expr<'a> {
    let right = self.0.nth_child(1).unwrap();
    Expr::new(right)
  }
}

impl<'a> Serialize for BinaryExpr<'a> {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: serde::Serializer,
  {
    let mut state = serializer.serialize_struct("BinaryExpression", 3)?;
    let op = self.operator();
    let left = self.left_operand();
    let right = self.right_operand();
    state.serialize_field("op", &op)?;
    state.serialize_field("left_operand", &left)?;
    state.serialize_field("right_operand", &right)?;
    state.end()
  }
}

#[derive(Debug)]
pub struct UnaryExpr<'a>(AstNodePtr<'a>);

impl<'a> UnaryExpr<'a> {
  pub fn operator(&self) -> UnaryOp {
    let node = self.0.get();
    let AstNodeKind::UnaryExpr(op) = &node.inner else { unreachable!() };
    *op
  }

  pub fn arg(&self) -> Expr<'a> {
    let arg = self.0.nth_child(0).unwrap();
    Expr::new(arg)
  }
}

impl<'a> Serialize for UnaryExpr<'a> {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: serde::Serializer,
  {
    let mut state = serializer.serialize_struct("UnaryExpr", 2)?;
    let op = self.operator();
    let arg = self.arg();
    state.serialize_field("op", &op)?;
    state.serialize_field("arg", &arg)?;
    state.end()
  }
}

#[derive(Debug)]
pub struct StringLit<'a>(AstNodePtr<'a>);

impl<'a> Serialize for StringLit<'a> {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: serde::Serializer,
  {
    let node = self.0.get();
    let AstNodeKind::StrLiteral(_, value) = node.inner else { unreachable!() };
    serializer.serialize_str(value)
  }
}

#[derive(Debug)]
pub struct NumericLit<'a>(AstNodePtr<'a>);

impl<'a> Serialize for NumericLit<'a> {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: serde::Serializer,
  {
    let node = self.0.get();
    let AstNodeKind::NumLiteral(_, value) = node.inner else { unreachable!() };
    serializer.serialize_f64(value)
  }
}

#[derive(Debug)]
pub struct BoolLit<'a>(AstNodePtr<'a>);

impl<'a> Serialize for BoolLit<'a> {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: serde::Serializer,
  {
    let node = self.0.get();
    let AstNodeKind::BoolLiteral(value) = node.inner else { unreachable!() };
    serializer.serialize_bool(value)
  }
}

#[derive(Debug)]
pub struct NilLit<'a>(AstNodePtr<'a>);

impl<'a> Serialize for NilLit<'a> {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: serde::Serializer,
  {
    serializer.serialize_unit_struct("Nil")
  }
}
