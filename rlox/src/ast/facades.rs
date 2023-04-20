//! Immutable facades of the Abstract Syntax Trees. They are typed views into the raw Syntax Tree
//! facade types provides functinalities like pretty-printing the grammar-aware structure of the
//! syntax tree and grammar-aware access into the syntax tree.

use thiserror::Error;

use super::{AstNode, AstNodeId, AstNodeKind, BinaryOp};

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

  /// Nth child of the current node, 1-indexed
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

#[derive(Debug)]
pub enum Expression<'a> {
  BinaryExpression(BinaryExpression<'a>),
  StringLiteral(StringLiteral<'a>),
  NumberLiteral(NumericLiteral<'a>),
}

impl<'a> Expression<'a> {
  pub fn new(ptr: AstNodePtr<'a>) -> Self {
    match ptr.get().inner {
      AstNodeKind::BinaryExpr => Self::BinaryExpression(BinaryExpression(ptr)),
      AstNodeKind::StrLiteral(_) => Self::StringLiteral(StringLiteral(ptr)),
      AstNodeKind::NumLiteral(_) => Self::NumberLiteral(NumericLiteral(ptr)),
      _ => unreachable!(),
    }
  }
}

#[derive(Debug)]
pub struct BinaryExpression<'a>(AstNodePtr<'a>);

impl<'a> BinaryExpression<'a> {
  pub fn new(arena: &'a indextree::Arena<AstNode>, cursor: AstNodeId) -> Self {
    Self(AstNodePtr::new(arena, cursor))
  }

  pub fn operator(&self) -> BinaryOp {
    let op = self.0.nth_child(2).unwrap();
    let node = op.get();
    match &node.inner {
      AstNodeKind::BinaryOp(op) => *op,
      other => {
        panic!(
          "Binary expression has an invalid operator node: {:?}",
          other
        );
      }
    }
  }

  pub fn left_operand(&self) -> Expression<'a> {
    let left = self.0.nth_child(1).unwrap();
    Expression::new(left)
  }

  pub fn right_operand(&self) -> Expression<'a> {
    let right = self.0.nth_child(3).unwrap();
    Expression::new(right)
  }
}

#[derive(Debug)]
pub struct StringLiteral<'a>(AstNodePtr<'a>);

#[derive(Debug)]
pub struct NumericLiteral<'a>(AstNodePtr<'a>);
