//! This module implements the core [SyntaxTree] and [AstNode] types. They are easily
//! composable and mutable, arena-backed tree structrues, where every node has an arbitrary of
//! child nodes. Note also [AstNode] is untyped, i.e., an expression and a class declaration all have
//! the same time and can be child/parent of each other technically.
//!
//! The design goal is to materialize lox-specific grammar lazily so that the tree can be easily mutated for
//! optimization.

pub mod facades;
pub mod visit;

use std::ops::Deref;

use serde::Serialize;

use self::facades::AstNodePtr;

use rlox_span::{Span, SymbolId};

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
  // Program
  Program,
  // Declaration
  VarDecl(SymbolId),
  // Statements
  ExprStmt,
  PrintStmt,
  Block,
  // Expressions
  TernaryExpr,
  Assign,
  BinaryExpr(BinaryOp),
  UnaryExpr(UnaryOp),
  StrLiteral(SymbolId, &'static str),
  NumLiteral(SymbolId, f64),
  BoolLiteral(bool),
  Var(SymbolId),
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

#[derive(Debug)]
pub struct SyntaxTree {
  arena: indextree::Arena<AstNode>,
  root: AstNodeId,
}

impl SyntaxTree {
  pub fn new(arena: indextree::Arena<AstNode>, root: AstNodeId) -> Self {
    Self { arena, root }
  }

  pub fn root_ptr(&self) -> AstNodePtr<'_> {
    AstNodePtr::new(&self.arena, self.root)
  }
}

#[derive(Default)]
pub struct SyntaxTreeBuilder {
  pub arena: indextree::Arena<AstNode>,
}

impl SyntaxTreeBuilder {
  pub fn finish(self, root: AstNodeId) -> SyntaxTree {
    SyntaxTree::new(self.arena, root)
  }

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

  pub fn unary_expression(&mut self, span: Span, op: UnaryOp, arg: AstNodeId) -> AstNodeId {
    let inner = AstNode {
      span,
      inner: AstNodeKind::UnaryExpr(op),
    };

    let unary_expr = AstNodeId::from(self.arena.new_node(inner));
    unary_expr.append(indextree::NodeId::from(arg), &mut self.arena);
    unary_expr
  }

  pub fn ternary_expression(
    &mut self,
    pred: AstNodeId,
    conseq: AstNodeId,
    alt: AstNodeId,
  ) -> AstNodeId {
    let start = self.get_span(pred).start;
    let end = self.get_span(alt).end;
    let inner = AstNode {
      span: Span::new(start, end),
      inner: AstNodeKind::TernaryExpr,
    };

    let ternary = AstNodeId::from(self.arena.new_node(inner));
    ternary.append(indextree::NodeId::from(pred), &mut self.arena);
    ternary.append(indextree::NodeId::from(conseq), &mut self.arena);
    ternary.append(indextree::NodeId::from(alt), &mut self.arena);

    ternary
  }

  pub fn expression_statement(&mut self, span: Span, expr: AstNodeId) -> AstNodeId {
    let inner = AstNode {
      span,
      inner: AstNodeKind::ExprStmt,
    };
    let stmt = AstNodeId::from(self.arena.new_node(inner));
    stmt.append(indextree::NodeId::from(expr), &mut self.arena);

    stmt
  }

  pub fn print_statement(&mut self, span: Span, expr: AstNodeId) -> AstNodeId {
    let inner = AstNode {
      span,
      inner: AstNodeKind::PrintStmt,
    };
    let stmt = AstNodeId::from(self.arena.new_node(inner));
    stmt.append(indextree::NodeId::from(expr), &mut self.arena);

    stmt
  }

  pub fn start_program(&mut self, start: u32) -> ProgramBuilder {
    let inner = AstNode {
      span: Span::new(start, u32::MAX),
      inner: AstNodeKind::Program,
    };
    ProgramBuilder(AstNodeId::from(self.arena.new_node(inner)))
  }

  pub fn add_statement(&mut self, builder: &ProgramBuilder, stmt: AstNodeId) {
    builder
      .0
      .append(indextree::NodeId::from(stmt), &mut self.arena);
  }

  pub fn finish_program(&mut self, builder: ProgramBuilder, end: u32) -> AstNodeId {
    let node = &mut self.arena[*builder.0];
    node.get_mut().span.end = end;
    builder.0
  }

  pub fn variable_declaration(
    &mut self,
    span: Span,
    variable: SymbolId,
    init: Option<AstNodeId>,
  ) -> AstNodeId {
    let inner = AstNode {
      span,
      inner: AstNodeKind::VarDecl(variable),
    };
    let decl = AstNodeId::from(self.arena.new_node(inner));
    if let Some(init) = init {
      decl.append(indextree::NodeId::from(init), &mut self.arena);
    }
    decl
  }

  pub fn variable_reference(&mut self, span: Span, variable: SymbolId) -> AstNodeId {
    let inner = AstNode {
      span,
      inner: AstNodeKind::Var(variable),
    };
    AstNodeId::from(self.arena.new_node(inner))
  }

  pub fn assignment_expression(
    &mut self,
    span: Span,
    target: AstNodeId,
    value: AstNodeId,
  ) -> AstNodeId {
    let inner = AstNode {
      span,
      inner: AstNodeKind::Assign,
    };
    let assign = AstNodeId::from(self.arena.new_node(inner));
    assign.append(indextree::NodeId::from(target), &mut self.arena);
    assign.append(indextree::NodeId::from(value), &mut self.arena);
    assign
  }

  pub fn start_block(&mut self, start: u32) -> BlockBuilder {
    let inner = AstNode {
      span: Span::new(start, u32::MAX),
      inner: AstNodeKind::Block,
    };
    BlockBuilder(AstNodeId::from(self.arena.new_node(inner)))
  }

  pub fn add_block_statement(&mut self, builder: &BlockBuilder, stmt: AstNodeId) {
    let BlockBuilder(block_id) = builder;
    block_id.append(indextree::NodeId::from(stmt), &mut self.arena);
  }

  pub fn finish_block(&mut self, BlockBuilder(block): BlockBuilder, end: u32) -> AstNodeId {
    self.arena[indextree::NodeId::from(block)]
      .get_mut()
      .span
      .end = end;
    block
  }

  pub fn re_span(&mut self, node: AstNodeId, new_span: Span) -> AstNodeId {
    self.arena[indextree::NodeId::from(node)].get_mut().span = new_span;
    node
  }

  pub fn get_span(&self, id: AstNodeId) -> Span {
    self.arena[indextree::NodeId::from(id)].get().span
  }

  #[allow(clippy::match_like_matches_macro)]
  pub fn valid_assign_target(&self, node: AstNodeId) -> bool {
    let node = &self.arena[indextree::NodeId::from(node)];
    match &node.get().inner {
      AstNodeKind::Var(_) => true,
      _ => false,
    }
  }
}

#[derive(Debug)]
pub struct ProgramBuilder(AstNodeId);

#[derive(Debug)]
pub struct BlockBuilder(AstNodeId);
