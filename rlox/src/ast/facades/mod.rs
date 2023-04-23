//! This module implements facades, which are immutable views of the [SyntaxTree].
//! They understand the grammatical structure of the lox programming language (in contrast,
//! [AstNode] is just data structure, where every node can have arbitrary number of children
//! and any kind of node can be parent of any other kind)
//! Facades provides a checked handle to the raw data ndoes, e.g., a [BinaryExpr] has two children, both are [Expression]
//! and are the left and right operand of the binary expression.
//!
//! Facades are building blocks to functinalities like pretty-printing the syntax tree, implementing visitor patterns, etc.

mod expr;
mod program;
mod stmt;

use thiserror::Error;

use crate::common::span::{Span, Spanned};

use super::{AstNode, AstNodeId};

pub use self::expr::{
  BinaryExpr, BoolLit, Expr, NilLit, NumericLit, StringLit, TernaryExpr, UnaryExpr,
};
pub use self::program::Program;
pub use self::stmt::{ExprStmt, PrintStmt, Stmt};

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

  pub fn children(&self) -> Box<dyn Iterator<Item = AstNodePtr<'a>> + 'a> {
    let iter = self
      .cursor
      .children(self.arena)
      .map(|id| AstNodePtr::new(self.arena, AstNodeId::from(id)));
    Box::new(iter)
  }
}
