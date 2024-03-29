//! This module implements the core [SyntaxTree] and [AstNode] types. They are easily
//! composable and mutable, arena-backed tree structrues, where every node has an arbitrary of
//! child nodes. Note also [AstNode] is untyped, i.e., an expression and a class declaration all have
//! the same time and can be child/parent of each other technically.
//!
//! The design goal is to materialize lox-specific grammar lazily so that the tree can be easily mutated for
//! optimization.

#![feature(negative_impls)]

pub mod facades;
pub mod visit;

use std::cell::RefCell;
use std::ops::Deref;

use serde::Serialize;

use self::facades::AstNodePtr;

use rlox_span::{Interner, Span, SymbolId};

// Global variables in the parsing context.
// We don't support multi-threaded parser so a thread local is enough.
std::thread_local! {
  // Interner stores mapping from [SymbolId] -> Str of the symbol
  pub static INTERNER: RefCell<Interner>  = RefCell::new(Interner::default());
  /// Node Arena stores mapping from [AstNodeId] -> [AstNode]
  pub static NODE_ARENA: RefCell<indextree::Arena<AstNode>> = RefCell::new(indextree::Arena::new());
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct AstNodeId(indextree::NodeId);

/*
 [AstNodeId] do not implement Send or Sync because it is tied to a thread local arena. If you send an [AstNodeId] across
 thread boundary, you can no longer retrieve the corresponding [AstNode] because you no longer have access to the arena from
 which it is born.
*/
impl !Send for AstNodeId {}
impl !Sync for AstNodeId {}

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

/// Wraps [AstNodeKind] with the span in the source text.
#[derive(Debug, Clone)]
pub struct AstNode {
  span: Span,
  inner: AstNodeKind,
}

impl AstNode {
  pub fn new(span: Span, inner: AstNodeKind) -> Self {
    Self { span, inner }
  }
}

/// Inner data belonging to each Ast Node. This type is intended to be easily cloneable and copiable.
/// It encodes only the local data, not child/parent relations.
#[derive(Debug, Clone, Copy)]
pub enum AstNodeKind {
  // Program
  Program,
  // Declaration
  VarDecl(SymbolId),
  ClassDecl(SymbolId, Option<SymbolId>),
  Methods,
  // Statements
  ExprStmt,
  Block,
  IfStmt,
  WhileStmt,
  Break,
  Return,
  FnDecl(SymbolId),
  FnParamList,
  // Expressions
  TernaryExpr,
  Assign,
  FnCall,
  Member(SymbolId),
  CallArgList,
  BinaryExpr(BinaryOp),
  LogicExpr(LogicalOp),
  UnaryExpr(UnaryOp),
  StrLiteral(SymbolId, &'static str),
  NumLiteral(SymbolId, f64),
  BoolLiteral(bool),
  Var(SymbolId),
  Super,
  Nil,
}

#[test]
fn ast_kind_size() {
  // 24 = size_of::<(SymbolId, &'static str)>
  assert_eq!(24, std::mem::size_of::<AstNodeKind>());
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
  root: AstNodeId,
}

impl SyntaxTree {
  pub fn new(root: AstNodeId) -> Self {
    Self { root }
  }

  pub fn root_ptr(&self) -> AstNodePtr {
    AstNodePtr::new(self.root)
  }
}

macro_rules! append_child {
  ($parent:expr, $($child:expr),*) => {
      $(NODE_ARENA.with_borrow_mut(|arena| {
        $parent.append(indextree::NodeId::from($child), arena);
      });)*
  };
}

#[derive(Default)]
pub struct SyntaxTreeBuilder;

impl SyntaxTreeBuilder {
  pub fn new_node(&self, inner: AstNode) -> AstNodeId {
    AstNodeId::from(NODE_ARENA.with_borrow_mut(|arena| arena.new_node(inner)))
  }

  pub fn finish(self, root: AstNodeId) -> SyntaxTree {
    SyntaxTree::new(root)
  }

  pub fn string_literal(&mut self, span: Span, raw: SymbolId, value: &'static str) -> AstNodeId {
    let inner = AstNode::new(span, AstNodeKind::StrLiteral(raw, value));
    self.new_node(inner)
  }

  pub fn numeric_literal(&mut self, span: Span, raw: SymbolId, value: f64) -> AstNodeId {
    let inner = AstNode::new(span, AstNodeKind::NumLiteral(raw, value));
    self.new_node(inner)
  }

  pub fn bool_literal(&mut self, span: Span, value: bool) -> AstNodeId {
    let inner = AstNode::new(span, AstNodeKind::BoolLiteral(value));
    self.new_node(inner)
  }

  pub fn nil(&mut self, span: Span) -> AstNodeId {
    let inner = AstNode::new(span, AstNodeKind::Nil);
    self.new_node(inner)
  }

  pub fn logical_expression(
    &mut self,
    left: AstNodeId,
    op: LogicalOp,
    right: AstNodeId,
  ) -> AstNodeId {
    let start = self.get_span(left).start;
    let end = self.get_span(right).end;

    let inner = AstNode::new(Span::new(start, end), AstNodeKind::LogicExpr(op));

    let logical_expr = self.new_node(inner);
    append_child!(logical_expr, left, right);
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

    let inner = AstNode::new(Span::new(start, end), AstNodeKind::BinaryExpr(op));

    let binary_expr = self.new_node(inner);
    append_child!(binary_expr, left, right);
    binary_expr
  }

  pub fn unary_expression(&mut self, span: Span, op: UnaryOp, arg: AstNodeId) -> AstNodeId {
    let inner = AstNode::new(span, AstNodeKind::UnaryExpr(op));

    let unary_expr = self.new_node(inner);
    append_child!(unary_expr, arg);
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
    let inner = AstNode::new(Span::new(start, end), AstNodeKind::TernaryExpr);

    let ternary = self.new_node(inner);
    append_child!(ternary, pred, conseq, alt);

    ternary
  }

  pub fn expression_statement(&mut self, span: Span, expr: AstNodeId) -> AstNodeId {
    let inner = AstNode::new(span, AstNodeKind::ExprStmt);
    let stmt = self.new_node(inner);
    append_child!(stmt, expr);

    stmt
  }

  pub fn start_program(&mut self, start: u32) -> ProgramBuilder {
    let inner = AstNode::new(Span::new(start, u32::MAX), AstNodeKind::Program);
    ProgramBuilder(self.new_node(inner))
  }

  pub fn add_statement(&mut self, ProgramBuilder(program): &ProgramBuilder, stmt: AstNodeId) {
    append_child!(program, stmt);
  }

  pub fn finish_program(&mut self, ProgramBuilder(program): ProgramBuilder, end: u32) -> AstNodeId {
    NODE_ARENA.with_borrow_mut(|arena| {
      let node = &mut arena[indextree::NodeId::from(program)];
      node.get_mut().span.end = end;
      program
    })
  }

  pub fn variable_declaration(
    &mut self,
    span: Span,
    variable: SymbolId,
    init: AstNodeId,
  ) -> AstNodeId {
    let inner = AstNode::new(span, AstNodeKind::VarDecl(variable));
    let decl = self.new_node(inner);
    append_child!(decl, init);
    decl
  }

  pub fn variable_reference(&mut self, span: Span, variable: SymbolId) -> AstNodeId {
    let inner = AstNode::new(span, AstNodeKind::Var(variable));
    self.new_node(inner)
  }

  pub fn assignment_expression(
    &mut self,
    span: Span,
    target: AstNodeId,
    value: AstNodeId,
  ) -> AstNodeId {
    let inner = AstNode::new(span, AstNodeKind::Assign);
    let assign = self.new_node(inner);
    append_child!(assign, target, value);
    assign
  }

  pub fn start_block(&mut self, start: u32) -> BlockBuilder {
    let inner = AstNode::new(Span::new(start, u32::MAX), AstNodeKind::Block);
    BlockBuilder(self.new_node(inner))
  }

  pub fn add_block_statement(&mut self, builder: &BlockBuilder, stmt: AstNodeId) {
    let BlockBuilder(block_id) = builder;
    append_child!(block_id, stmt);
  }

  pub fn finish_block(&mut self, BlockBuilder(block): BlockBuilder, end: u32) -> AstNodeId {
    NODE_ARENA.with_borrow_mut(|arena| {
      let node = &mut arena[indextree::NodeId::from(block)];
      node.get_mut().span.end = end;
      block
    })
  }

  pub fn break_statement(&mut self, span: Span) -> AstNodeId {
    let inner = AstNode::new(span, AstNodeKind::Break);
    self.new_node(inner)
  }

  pub fn if_statement(
    &mut self,
    span: Span,
    pred: AstNodeId,
    conseq: AstNodeId,
    alt: AstNodeId,
  ) -> AstNodeId {
    let inner = AstNode::new(span, AstNodeKind::IfStmt);
    let if_stmt = self.new_node(inner);
    append_child!(if_stmt, pred, conseq, alt);
    if_stmt
  }

  pub fn while_statement(&mut self, span: Span, pred: AstNodeId, body: AstNodeId) -> AstNodeId {
    let inner = AstNode::new(span, AstNodeKind::WhileStmt);
    let while_stmt = self.new_node(inner);
    append_child!(while_stmt, pred, body);
    while_stmt
  }

  pub fn function_call(
    &mut self,
    span: Span,
    callee: AstNodeId,
    arguments: AstNodeId,
  ) -> AstNodeId {
    let inner = AstNode::new(span, AstNodeKind::FnCall);
    let call = self.new_node(inner);
    append_child!(call, callee, arguments);
    call
  }

  pub fn start_argument_list(&mut self, start: u32) -> ArgListBuilder {
    let span = Span::new(start, u32::MAX);
    let inner = AstNode::new(span, AstNodeKind::CallArgList);
    ArgListBuilder(self.new_node(inner))
  }

  pub fn add_argument(&mut self, ArgListBuilder(node): &ArgListBuilder, arguments: AstNodeId) {
    append_child!(node, arguments);
  }

  pub fn finish_argument_list(
    &mut self,
    ArgListBuilder(arguments): ArgListBuilder,
    end: u32,
  ) -> AstNodeId {
    NODE_ARENA.with_borrow_mut(|arena| {
      let node = &mut arena[indextree::NodeId::from(arguments)];
      node.get_mut().span.end = end;
      arguments
    })
  }

  pub fn function_declaration(
    &mut self,
    span: Span,
    name: SymbolId,
    parameters: AstNodeId,
    body: AstNodeId,
  ) -> AstNodeId {
    let inner = AstNode::new(span, AstNodeKind::FnDecl(name));
    let fn_decl = self.new_node(inner);
    append_child!(fn_decl, parameters, body);
    fn_decl
  }

  pub fn start_parameter_list(&mut self, start: u32) -> ParamListBuilder {
    let inner = AstNode::new(Span::new(start, u32::MAX), AstNodeKind::FnParamList);
    ParamListBuilder(self.new_node(inner))
  }

  pub fn add_parameter(&mut self, ParamListBuilder(node): &ParamListBuilder, parameter: AstNodeId) {
    append_child!(node, parameter);
  }

  pub fn finish_parameter_list(
    &mut self,
    ParamListBuilder(param): ParamListBuilder,
    end: u32,
  ) -> AstNodeId {
    NODE_ARENA.with_borrow_mut(|arena| {
      let node = &mut arena[indextree::NodeId::from(param)];
      node.get_mut().span.end = end;
      param
    })
  }

  pub fn return_statement(&mut self, span: Span, returned: AstNodeId) -> AstNodeId {
    let inner = AstNode::new(span, AstNodeKind::Return);
    let return_stmt = self.new_node(inner);
    append_child!(return_stmt, returned);
    return_stmt
  }

  pub fn class_declaration(
    &mut self,
    span: Span,
    name: SymbolId,
    parent: Option<SymbolId>,
    methods: AstNodeId,
    static_methods: AstNodeId,
  ) -> AstNodeId {
    let inner = AstNode::new(span, AstNodeKind::ClassDecl(name, parent));
    let class_decl = self.new_node(inner);
    append_child!(class_decl, methods, static_methods);
    class_decl
  }

  pub fn start_method_list(&mut self, start: u32) -> MethodsBuilder {
    let inner = AstNode::new(Span::new(start, u32::MAX), AstNodeKind::Methods);
    MethodsBuilder(self.new_node(inner))
  }

  pub fn add_method(&mut self, MethodsBuilder(node): &MethodsBuilder, method: AstNodeId) {
    append_child!(node, method);
  }

  pub fn finish_method_list(
    &mut self,
    MethodsBuilder(methods): MethodsBuilder,
    end: u32,
  ) -> AstNodeId {
    NODE_ARENA.with_borrow_mut(|arena| {
      let node = &mut arena[indextree::NodeId::from(methods)];
      node.get_mut().span.end = end;
      methods
    })
  }

  pub fn member_access(&mut self, span: Span, object: AstNodeId, property: SymbolId) -> AstNodeId {
    let inner = AstNode::new(span, AstNodeKind::Member(property));
    let member = self.new_node(inner);
    append_child!(member, object);
    member
  }

  pub fn super_expression(&mut self, span: Span) -> AstNodeId {
    let inner = AstNode::new(span, AstNodeKind::Super);
    self.new_node(inner)
  }

  pub fn re_span(&mut self, node_id: AstNodeId, new_span: Span) -> AstNodeId {
    NODE_ARENA.with_borrow_mut(|arena| {
      let node = &mut arena[indextree::NodeId::from(node_id)];
      node.get_mut().span = new_span;
      node_id
    })
  }

  pub fn re_span_start(&mut self, node_id: AstNodeId, new_start: u32) -> AstNodeId {
    NODE_ARENA.with_borrow_mut(|arena| {
      let node = &mut arena[indextree::NodeId::from(node_id)];
      node.get_mut().span.start = new_start;
      node_id
    })
  }

  pub fn get_span(&self, id: AstNodeId) -> Span {
    NODE_ARENA.with_borrow(|arena| {
      let node = &arena[indextree::NodeId::from(id)];
      node.get().span
    })
  }

  #[allow(clippy::match_like_matches_macro)]
  pub fn valid_assign_target(&self, node_id: AstNodeId) -> bool {
    NODE_ARENA.with_borrow(|arena| {
      let node = &arena[indextree::NodeId::from(node_id)];
      match &node.get().inner {
        AstNodeKind::Var(_) => true,
        AstNodeKind::Member(_) => true,
        _ => false,
      }
    })
  }
}

pub struct ProgramBuilder(AstNodeId);
pub struct BlockBuilder(AstNodeId);
pub struct ArgListBuilder(AstNodeId);
pub struct ParamListBuilder(AstNodeId);
pub struct MethodsBuilder(AstNodeId);
