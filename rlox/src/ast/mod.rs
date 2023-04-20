mod facades;

use std::ops::Deref;

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
  BinaryExpr,
  BinaryOp(BinaryOp),
  UnaryExpr,
  StrLiteral(StrLiteral),
  NumLiteral(NumLiteral),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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

#[derive(Debug)]
pub struct StrLiteral(SymbolId);

#[derive(Debug)]
pub struct NumLiteral(SymbolId, f64);

pub struct SyntaxTree {
  arena: indextree::Arena<AstNode>,
  root: AstNodeId,
}

#[derive(Default)]
pub struct SyntaxTreeBuilder {
  arena: indextree::Arena<AstNode>,
}

impl SyntaxTreeBuilder {
  pub fn string_literal(&mut self, span: Span, value: SymbolId) -> AstNodeId {
    let inner = AstNode {
      span,
      inner: AstNodeKind::StrLiteral(StrLiteral(value)),
    };
    AstNodeId::from(self.arena.new_node(inner))
  }

  pub fn numeric_literal(&mut self, span: Span, raw: SymbolId, value: f64) -> AstNodeId {
    let inner = AstNode {
      span,
      inner: AstNodeKind::NumLiteral(NumLiteral(raw, value)),
    };
    AstNodeId::from(self.arena.new_node(inner))
  }

  pub fn binary_operator(&mut self, span: Span, op: BinaryOp) -> AstNodeId {
    let inner = AstNode {
      span,
      inner: AstNodeKind::BinaryOp(op),
    };
    AstNodeId::from(self.arena.new_node(inner))
  }

  pub fn binary_expression(
    &mut self,
    span: Span,
    left: AstNodeId,
    op: AstNodeId,
    right: AstNodeId,
  ) -> AstNodeId {
    let inner = AstNode {
      span,
      inner: AstNodeKind::BinaryExpr,
    };
    let binary_expr = AstNodeId::from(self.arena.new_node(inner));
    binary_expr.append(indextree::NodeId::from(left), &mut self.arena);
    binary_expr.append(indextree::NodeId::from(op), &mut self.arena);
    binary_expr.append(indextree::NodeId::from(right), &mut self.arena);
    binary_expr
  }
}
