//! This module implements facades, which are immutable views of the [SyntaxTree].
//! They understand the grammatical structure of the lox programming language (in contrast,
//! [AstNode] is just data structure, where every node can have arbitrary number of children
//! and any kind of node can be parent of any other kind)
//! Facades provides a checked handle to the raw data ndoes, e.g., a [BinaryExpr] has two children, both are [Expression]
//! and are the left and right operand of the binary expression.
//!
//! Facades are building blocks to functinalities like pretty-printing the syntax tree, implementing visitor patterns, etc.

use serde::{ser::SerializeStruct, Serialize};
use thiserror::Error;

use crate::common::span::{Span, Spanned};

use super::{AstNode, AstNodeId, AstNodeKind, BinaryOp, UnaryOp};

/// A shared reference of an [AstNode], which packs together the arena
/// and the id.
#[derive(Debug, Clone, Copy)]
pub struct AstNodePtr<'a> {
  pub arena: &'a indextree::Arena<AstNode>,
  pub cursor: AstNodeId,
}

impl<'a> Spanned for AstNodePtr<'a> {
  fn span(&self) -> Span {
    self.get().span
  }
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
  Ternary(TernaryExpr<'a>),
  Binary(BinaryExpr<'a>),
  Unary(UnaryExpr<'a>),
  String(StringLit<'a>),
  Number(NumericLit<'a>),
  Bool(BoolLit<'a>),
  Nil(NilLit<'a>),
}

impl<'a> Spanned for Expr<'a> {
  fn span(&self) -> Span {
    match self {
      Expr::Ternary(e) => e.span(),
      Expr::Binary(e) => e.span(),
      Expr::Unary(e) => e.span(),
      Expr::String(e) => e.span(),
      Expr::Number(e) => e.span(),
      Expr::Bool(e) => e.span(),
      Expr::Nil(e) => e.span(),
    }
  }
}

impl<'a> Expr<'a> {
  pub fn new(ptr: AstNodePtr<'a>) -> Self {
    use self::Expr::*;
    match ptr.get().inner {
      AstNodeKind::TernaryExpr => Ternary(TernaryExpr(ptr)),
      AstNodeKind::BinaryExpr(_) => Binary(BinaryExpr(ptr)),
      AstNodeKind::StrLiteral(_, _) => String(StringLit(ptr)),
      AstNodeKind::NumLiteral(_, _) => Number(NumericLit(ptr)),
      AstNodeKind::UnaryExpr(_) => Unary(UnaryExpr(ptr)),
      AstNodeKind::BoolLiteral(_) => Bool(BoolLit(ptr)),
      AstNodeKind::Nil => Nil(NilLit(ptr)),
      // _ => unreachable!(),
    }
  }
}

#[derive(Debug)]
pub struct BinaryExpr<'a>(AstNodePtr<'a>);

impl<'a> BinaryExpr<'a> {
  pub fn operator(&self) -> BinaryOp {
    match &self.0.get().inner {
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

impl<'a> Spanned for BinaryExpr<'a> {
  fn span(&self) -> Span {
    self.0.span()
  }
}

impl<'a> Serialize for BinaryExpr<'a> {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: serde::Serializer,
  {
    let mut state = serializer.serialize_struct("BinaryExpr", 3)?;
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

impl<'a> Spanned for UnaryExpr<'a> {
  fn span(&self) -> Span {
    self.0.span()
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

impl<'a> StringLit<'a> {
  pub fn value(&self) -> &'static str {
    let node = self.0.get();
    match node.inner {
      AstNodeKind::StrLiteral(_, value) => value,
      _ => unreachable!(),
    }
  }
}

impl<'a> Spanned for StringLit<'a> {
  fn span(&self) -> Span {
    self.0.span()
  }
}

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

impl<'a> NumericLit<'a> {
  pub fn value(&self) -> f64 {
    let node = self.0.get();
    match node.inner {
      AstNodeKind::NumLiteral(_, value) => value,
      _ => unreachable!(),
    }
  }
}

impl<'a> Spanned for NumericLit<'a> {
  fn span(&self) -> Span {
    self.0.span()
  }
}

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

impl<'a> BoolLit<'a> {
  pub fn value(&self) -> bool {
    let node = self.0.get();
    match node.inner {
      AstNodeKind::BoolLiteral(value) => value,
      _ => unreachable!(),
    }
  }
}

impl<'a> Spanned for BoolLit<'a> {
  fn span(&self) -> Span {
    self.0.span()
  }
}

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

impl<'a> Spanned for NilLit<'a> {
  fn span(&self) -> Span {
    self.0.span()
  }
}

impl<'a> Serialize for NilLit<'a> {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: serde::Serializer,
  {
    serializer.serialize_unit_struct("Nil")
  }
}

#[derive(Debug)]
pub struct TernaryExpr<'a>(AstNodePtr<'a>);

impl<'a> TernaryExpr<'a> {
  pub fn predicate(&self) -> Expr<'a> {
    let ptr = self.0.nth_child(0).unwrap();
    Expr::new(ptr)
  }

  pub fn consequence(&self) -> Expr<'a> {
    let ptr = self.0.nth_child(1).unwrap();
    Expr::new(ptr)
  }

  pub fn alternative(&self) -> Expr<'a> {
    let ptr = self.0.nth_child(2).unwrap();
    Expr::new(ptr)
  }
}

impl<'a> Spanned for TernaryExpr<'a> {
  fn span(&self) -> Span {
    self.0.span()
  }
}

impl<'a> Serialize for TernaryExpr<'a> {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: serde::Serializer,
  {
    let p = self.predicate();
    let c = self.consequence();
    let a = self.alternative();

    let mut state = serializer.serialize_struct("TernaryExpr", 3)?;
    state.serialize_field("predicate", &p)?;
    state.serialize_field("consequence", &c)?;
    state.serialize_field("alternative", &a)?;
    state.end()
  }
}
