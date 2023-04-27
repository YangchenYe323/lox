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
  Block,
  IfStmt,
  WhileStmt,
  Break,
  // Expressions
  TernaryExpr,
  Assign,
  FnCall,
  CallArgList,
  BinaryExpr(BinaryOp),
  LogicExpr(LogicalOp),
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
pub enum LogicalOp {
  And,
  Or,
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

macro_rules! append_child {
  ($parent:expr, $arena:expr, $($child:expr),*) => {
      $($parent.append(indextree::NodeId::from($child), $arena);)*
  };
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

  pub fn logical_expression(
    &mut self,
    left: AstNodeId,
    op: LogicalOp,
    right: AstNodeId,
  ) -> AstNodeId {
    let start = self.get_span(left).start;
    let end = self.get_span(right).end;

    let inner = AstNode {
      span: Span::new(start, end),
      inner: AstNodeKind::LogicExpr(op),
    };

    let logical_expr = AstNodeId::from(self.arena.new_node(inner));
    append_child!(logical_expr, &mut self.arena, left, right);
    logical_expr
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
    append_child!(binary_expr, &mut self.arena, left, right);
    binary_expr
  }

  pub fn unary_expression(&mut self, span: Span, op: UnaryOp, arg: AstNodeId) -> AstNodeId {
    let inner = AstNode {
      span,
      inner: AstNodeKind::UnaryExpr(op),
    };

    let unary_expr = AstNodeId::from(self.arena.new_node(inner));
    append_child!(unary_expr, &mut self.arena, arg);
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
    append_child!(ternary, &mut self.arena, pred, conseq, alt);

    ternary
  }

  pub fn expression_statement(&mut self, span: Span, expr: AstNodeId) -> AstNodeId {
    let inner = AstNode {
      span,
      inner: AstNodeKind::ExprStmt,
    };
    let stmt = AstNodeId::from(self.arena.new_node(inner));
    append_child!(stmt, &mut self.arena, expr);

    stmt
  }

  pub fn start_program(&mut self, start: u32) -> ProgramBuilder {
    let inner = AstNode {
      span: Span::new(start, u32::MAX),
      inner: AstNodeKind::Program,
    };
    ProgramBuilder(AstNodeId::from(self.arena.new_node(inner)))
  }

  pub fn add_statement(&mut self, ProgramBuilder(program): &ProgramBuilder, stmt: AstNodeId) {
    append_child!(program, &mut self.arena, stmt);
  }

  pub fn finish_program(&mut self, ProgramBuilder(program): ProgramBuilder, end: u32) -> AstNodeId {
    let node = &mut self.arena[indextree::NodeId::from(program)];
    node.get_mut().span.end = end;
    program
  }

  pub fn variable_declaration(
    &mut self,
    span: Span,
    variable: SymbolId,
    init: AstNodeId,
  ) -> AstNodeId {
    let inner = AstNode {
      span,
      inner: AstNodeKind::VarDecl(variable),
    };
    let decl = AstNodeId::from(self.arena.new_node(inner));
    append_child!(decl, &mut self.arena, init);
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
    append_child!(assign, &mut self.arena, target, value);
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
    append_child!(block_id, &mut self.arena, stmt);
  }

  pub fn finish_block(&mut self, BlockBuilder(block): BlockBuilder, end: u32) -> AstNodeId {
    self.arena[indextree::NodeId::from(block)]
      .get_mut()
      .span
      .end = end;
    block
  }

  pub fn break_statement(&mut self, span: Span) -> AstNodeId {
    let inner = AstNode {
      span,
      inner: AstNodeKind::Break,
    };
    AstNodeId::from(self.arena.new_node(inner))
  }

  pub fn if_statement(
    &mut self,
    span: Span,
    pred: AstNodeId,
    conseq: AstNodeId,
    alt: AstNodeId,
  ) -> AstNodeId {
    let inner = AstNode {
      span,
      inner: AstNodeKind::IfStmt,
    };
    let if_stmt = AstNodeId::from(self.arena.new_node(inner));
    append_child!(if_stmt, &mut self.arena, pred, conseq, alt);
    if_stmt
  }

  pub fn while_statement(&mut self, span: Span, pred: AstNodeId, body: AstNodeId) -> AstNodeId {
    let inner = AstNode {
      span,
      inner: AstNodeKind::WhileStmt,
    };
    let while_stmt = AstNodeId::from(self.arena.new_node(inner));
    append_child!(while_stmt, &mut self.arena, pred, body);
    while_stmt
  }

  pub fn function_call(
    &mut self,
    span: Span,
    callee: AstNodeId,
    arguments: AstNodeId,
  ) -> AstNodeId {
    let inner = AstNode {
      span,
      inner: AstNodeKind::FnCall,
    };
    let call = AstNodeId::from(self.arena.new_node(inner));
    append_child!(call, &mut self.arena, callee, arguments);
    call
  }

  pub fn start_argument_list(&mut self, start: u32) -> ArgListBuilder {
    let span = Span::new(start, u32::MAX);
    let inner = AstNode {
      span,
      inner: AstNodeKind::CallArgList,
    };
    ArgListBuilder(AstNodeId::from(self.arena.new_node(inner)))
  }

  pub fn add_argument(&mut self, ArgListBuilder(node): &ArgListBuilder, arguments: AstNodeId) {
    node.append(indextree::NodeId::from(arguments), &mut self.arena)
  }

  pub fn finish_argument_list(
    &mut self,
    ArgListBuilder(node): ArgListBuilder,
    end: u32,
  ) -> AstNodeId {
    self.arena[indextree::NodeId::from(node)].get_mut().span.end = end;
    node
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

pub struct ProgramBuilder(AstNodeId);

pub struct BlockBuilder(AstNodeId);

pub struct ArgListBuilder(AstNodeId);
