//! This module implements the core [SyntaxTree] and [AstNode] types. They are easily
//! composable and mutable, arena-backed tree structrues, where every node has an arbitrary of
//! child nodes. Note also [AstNode] is untyped, i.e., an expression and a class declaration all have
//! the same time and can be child/parent of each other technically.
//!
//! The design goal is to materialize lox-specific grammar lazily so that the tree can be easily mutated for
//! optimization.

pub mod facades;

use std::ops::Deref;

use serde::Serialize;

use crate::common::{span::Span, symbol::SymbolId};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct AstNodeId(indextree::NodeId);

impl From<indextree::NodeId> for AstNodeId {
  fn from(value: indextree::NodeId) -> Self {
    Self(value)
  }
}

impl From<AstNodeId> for indextree::NodeId {
  fn from(value: AstNodeId) -> Self {
    value.0
  }
}

impl Deref for AstNodeId {
  type Target = indextree::NodeId;

  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

#[derive(Debug)]
pub struct AstNode {
  span: Span,
  inner: AstNodeKind,
}

#[derive(Debug)]
pub enum AstNodeKind {
  BinaryExpr(BinaryOp),
  UnaryExpr(UnaryOp),
  StrLiteral(SymbolId, &'static str),
  NumLiteral(SymbolId, f64),
  BoolLiteral(bool),
  Nil,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
pub enum BinaryOp {
  EqEq,
  BangEq,
  Less,
  LessEq,
  Greater,
  GreaterEq,
  Plus,
  Minus,
  Mult,
  Div,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
pub enum UnaryOp {
  Not,
  Neg,
}

pub struct SyntaxTree {
  arena: indextree::Arena<AstNode>,
  root: AstNodeId,
}

#[derive(Default)]
pub struct SyntaxTreeBuilder {
  pub arena: indextree::Arena<AstNode>,
}

impl SyntaxTreeBuilder {
  pub fn string_literal(&mut self, span: Span, raw: SymbolId, value: &'static str) -> AstNodeId {
    let inner = AstNode {
      span,
      inner: AstNodeKind::StrLiteral(raw, value),
    };
    AstNodeId::from(self.arena.new_node(inner))
  }

  pub fn numeric_literal(&mut self, span: Span, raw: SymbolId, value: f64) -> AstNodeId {
    let inner = AstNode {
      span,
      inner: AstNodeKind::NumLiteral(raw, value),
    };
    AstNodeId::from(self.arena.new_node(inner))
  }

  pub fn bool_literal(&mut self, span: Span, value: bool) -> AstNodeId {
    let inner = AstNode {
      span,
      inner: AstNodeKind::BoolLiteral(value),
    };
    AstNodeId::from(self.arena.new_node(inner))
  }

  pub fn nil(&mut self, span: Span) -> AstNodeId {
    let inner = AstNode {
      span,
      inner: AstNodeKind::Nil,
    };
    AstNodeId::from(self.arena.new_node(inner))
  }

  pub fn binary_expression(
    &mut self,
    left: AstNodeId,
    op: BinaryOp,
    right: AstNodeId,
  ) -> AstNodeId {
    let start = self.get_span(left).start;
    let end = self.get_span(right).end;

    let inner = AstNode {
      span: Span::new(start, end),
      inner: AstNodeKind::BinaryExpr(op),
    };

    let binary_expr = AstNodeId::from(self.arena.new_node(inner));
    binary_expr.append(indextree::NodeId::from(left), &mut self.arena);
    binary_expr.append(indextree::NodeId::from(right), &mut self.arena);
    binary_expr
  }

  pub fn unary_expression(&mut self, start: u32, op: UnaryOp, arg: AstNodeId) -> AstNodeId {
    let end = self.get_span(arg).end;
    let inner = AstNode {
      span: Span::new(start, end),
      inner: AstNodeKind::UnaryExpr(op),
    };

    let unary_expr = AstNodeId::from(self.arena.new_node(inner));
    unary_expr.append(indextree::NodeId::from(arg), &mut self.arena);
    unary_expr
  }

  fn get_span(&self, id: AstNodeId) -> Span {
    self.arena[indextree::NodeId::from(id)].get().span
  }
}
