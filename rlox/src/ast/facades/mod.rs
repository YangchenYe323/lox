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

use rlox_span::{Span, Spanned};

use super::{AstNode, AstNodeId};

pub use self::expr::{
  AssignExpr, AssignTarget, BinaryExpr, BoolLit, Expr, LogicExpr, NilLit, NumericLit, StringLit,
  TernaryExpr, UnaryExpr, Var,
};
pub use self::program::Program;
pub use self::stmt::{Block, ExprStmt, IfStmt, PrintStmt, Stmt, VarDecl, WhileStmt};

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

impl<'a> AstNodePtr<'a> {
  pub fn new(arena: &'a indextree::Arena<AstNode>, cursor: AstNodeId) -> Self {
    Self { arena, cursor }
  }

  pub fn get(&self) -> &AstNode {
    self.arena[self.cursor.into()].get()
  }

  /// Nth child of the current node, 0-indexed
  pub fn nth_child(&self, n: usize) -> Option<AstNodePtr<'a>> {
    let mut it = self.cursor.children(self.arena).skip(n);
    it.next()
      .map(|id| Self::new(self.arena, AstNodeId::from(id)))
  }

  pub fn children(&self) -> Box<dyn Iterator<Item = AstNodePtr<'a>> + 'a> {
    let iter = self
      .cursor
      .children(self.arena)
      .map(|id| AstNodePtr::new(self.arena, AstNodeId::from(id)));
    Box::new(iter)
  }
}
