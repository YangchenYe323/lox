//! This module implements facades, which are immutable views of the [SyntaxTree].
//! They understand the grammatical structure of the lox programming language (in contrast,
//! [AstNode] is just data structure, where every node can have arbitrary number of children
//! and any kind of node can be parent of any other kind)
//! Facades provides a checked handle to the raw data ndoes, e.g., a [BinaryExpr] has two children, both are [Expression]
//! and are the left and right operand of the binary expression.
//!
//! Facades are building blocks to functinalities like pretty-printing the syntax tree, implementing visitor patterns, etc.

pub mod expr;
pub mod program;
pub mod stmt;

use rlox_span::{Span, Spanned};

use super::{AstNode, AstNodeId, NODE_ARENA};

pub use self::expr::{
  Args, AssignExpr, AssignTarget, BinaryExpr, BoolLit, CallExpr, Expr, LogicExpr, MemberExpr,
  NilLit, NumericLit, StringLit, TernaryExpr, UnaryExpr, Var,
};
pub use self::program::Program;
pub use self::stmt::{
  Block, BreakStmt, ClassDecl, ExprStmt, FnDecl, IfStmt, Methods, Params, ReturnStmt, Stmt,
  VarDecl, WhileStmt,
};

#[derive(Debug, Clone, Copy)]
pub struct AstNodePtr {
  pub cursor: AstNodeId,
}

impl Spanned for AstNodePtr {
  fn span(&self) -> Span {
    self.get().span
  }
}

impl AstNodePtr {
  pub fn new(cursor: AstNodeId) -> Self {
    Self { cursor }
  }

  pub fn get(&self) -> AstNode {
    NODE_ARENA.with_borrow(|arena| arena[self.cursor.into()].get().clone())
  }

  /// Nth child of the current node, 0-indexed
  pub fn nth_child(&self, n: usize) -> Option<AstNodePtr> {
    NODE_ARENA.with_borrow(|arena| {
      let mut it = self.cursor.children(arena).skip(n);
      it.next().map(|id| Self::new(AstNodeId::from(id)))
    })
  }

  pub fn children(&self) -> impl Iterator<Item = AstNodePtr> {
    NODE_ARENA.with_borrow(|arena| {
      self
        .cursor
        .children(arena)
        .map(|id| AstNodePtr::new(AstNodeId::from(id)))
        .collect::<Vec<_>>()
        .into_iter()
    })
  }
}
