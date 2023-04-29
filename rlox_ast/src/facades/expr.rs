use serde::{
  ser::{SerializeSeq, SerializeStruct, SerializeTupleStruct},
  Serialize,
};

use crate::{AstNodeKind, BinaryOp, LogicalOp, UnaryOp};

use super::AstNodePtr;

use rlox_span::{Span, Spanned, SymbolId};

#[derive(Debug, Serialize, Clone, Copy)]
pub enum Expr {
  Member(MemberExpr),
  Call(CallExpr),
  Assign(AssignExpr),
  Ternary(TernaryExpr),
  Logic(LogicExpr),
  Binary(BinaryExpr),
  Unary(UnaryExpr),
  String(StringLit),
  Number(NumericLit),
  Bool(BoolLit),
  Var(Var),
  Super(SuperExpr),
  Nil(NilLit),
}

impl Spanned for Expr {
  fn span(&self) -> Span {
    match self {
      Expr::Call(e) => e.span(),
      Expr::Ternary(e) => e.span(),
      Expr::Binary(e) => e.span(),
      Expr::Unary(e) => e.span(),
      Expr::String(e) => e.span(),
      Expr::Number(e) => e.span(),
      Expr::Bool(e) => e.span(),
      Expr::Nil(e) => e.span(),
      Expr::Var(e) => e.span(),
      Expr::Assign(e) => e.span(),
      Expr::Logic(e) => e.span(),
      Expr::Member(e) => e.span(),
      Expr::Super(e) => e.span(),
    }
  }
}

impl Expr {
  pub fn new(ptr: AstNodePtr) -> Self {
    use self::Expr::*;
    match ptr.get().inner {
      AstNodeKind::FnCall => Call(CallExpr(ptr)),
      AstNodeKind::TernaryExpr => Ternary(TernaryExpr(ptr)),
      AstNodeKind::Assign => Assign(AssignExpr(ptr)),
      AstNodeKind::BinaryExpr(_) => Binary(BinaryExpr(ptr)),
      AstNodeKind::LogicExpr(_) => Logic(LogicExpr(ptr)),
      AstNodeKind::StrLiteral(_, _) => String(StringLit(ptr)),
      AstNodeKind::NumLiteral(_, _) => Number(NumericLit(ptr)),
      AstNodeKind::UnaryExpr(_) => Unary(UnaryExpr(ptr)),
      AstNodeKind::BoolLiteral(_) => Bool(BoolLit(ptr)),
      AstNodeKind::Var(_) => Var(self::Var(ptr)),
      AstNodeKind::Nil => Nil(NilLit(ptr)),
      AstNodeKind::Member(_) => Member(MemberExpr(ptr)),
      AstNodeKind::Super => Super(SuperExpr(ptr)),
      _ => unreachable!(),
    }
  }
}

#[derive(Debug, Clone, Copy)]
pub struct BinaryExpr(AstNodePtr);

impl BinaryExpr {
  pub fn operator(&self) -> BinaryOp {
    match &self.0.get().inner {
      AstNodeKind::BinaryExpr(op) => *op,
      _ => unreachable!(),
    }
  }

  pub fn left_operand(&self) -> Expr {
    let left = self.0.nth_child(0).unwrap();
    Expr::new(left)
  }

  pub fn right_operand(&self) -> Expr {
    let right = self.0.nth_child(1).unwrap();
    Expr::new(right)
  }
}

impl Spanned for BinaryExpr {
  fn span(&self) -> Span {
    self.0.span()
  }
}

impl Serialize for BinaryExpr {
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

#[derive(Debug, Clone, Copy)]
pub struct UnaryExpr(AstNodePtr);

impl UnaryExpr {
  pub fn operator(&self) -> UnaryOp {
    let node = self.0.get();
    let AstNodeKind::UnaryExpr(op) = &node.inner else { unreachable!() };
    *op
  }

  pub fn arg(&self) -> Expr {
    let arg = self.0.nth_child(0).unwrap();
    Expr::new(arg)
  }
}

impl Spanned for UnaryExpr {
  fn span(&self) -> Span {
    self.0.span()
  }
}

impl Serialize for UnaryExpr {
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

#[derive(Debug, Clone, Copy)]
pub struct StringLit(AstNodePtr);

impl StringLit {
  pub fn value(&self) -> &'static str {
    let node = self.0.get();
    match node.inner {
      AstNodeKind::StrLiteral(_, value) => value,
      _ => unreachable!(),
    }
  }
}

impl Spanned for StringLit {
  fn span(&self) -> Span {
    self.0.span()
  }
}

impl Serialize for StringLit {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: serde::Serializer,
  {
    let node = self.0.get();
    let AstNodeKind::StrLiteral(_, value) = node.inner else { unreachable!() };
    serializer.serialize_str(value)
  }
}

#[derive(Debug, Clone, Copy)]
pub struct NumericLit(AstNodePtr);

impl NumericLit {
  pub fn value(&self) -> f64 {
    let node = self.0.get();
    match node.inner {
      AstNodeKind::NumLiteral(_, value) => value,
      _ => unreachable!(),
    }
  }
}

impl Spanned for NumericLit {
  fn span(&self) -> Span {
    self.0.span()
  }
}

impl Serialize for NumericLit {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: serde::Serializer,
  {
    let node = self.0.get();
    let AstNodeKind::NumLiteral(_, value) = node.inner else { unreachable!() };
    serializer.serialize_f64(value)
  }
}

#[derive(Debug, Clone, Copy)]
pub struct BoolLit(AstNodePtr);

impl BoolLit {
  pub fn value(&self) -> bool {
    let node = self.0.get();
    match node.inner {
      AstNodeKind::BoolLiteral(value) => value,
      _ => unreachable!(),
    }
  }
}

impl Spanned for BoolLit {
  fn span(&self) -> Span {
    self.0.span()
  }
}

impl Serialize for BoolLit {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: serde::Serializer,
  {
    let node = self.0.get();
    let AstNodeKind::BoolLiteral(value) = node.inner else { unreachable!() };
    serializer.serialize_bool(value)
  }
}

#[derive(Debug, Clone, Copy)]
pub struct NilLit(AstNodePtr);

impl Spanned for NilLit {
  fn span(&self) -> Span {
    self.0.span()
  }
}

impl Serialize for NilLit {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: serde::Serializer,
  {
    serializer.serialize_unit_struct("Nil")
  }
}

#[derive(Debug, Clone, Copy)]
pub struct TernaryExpr(AstNodePtr);

impl TernaryExpr {
  pub fn predicate(&self) -> Expr {
    let ptr = self.0.nth_child(0).unwrap();
    Expr::new(ptr)
  }

  pub fn consequence(&self) -> Expr {
    let ptr = self.0.nth_child(1).unwrap();
    Expr::new(ptr)
  }

  pub fn alternative(&self) -> Expr {
    let ptr = self.0.nth_child(2).unwrap();
    Expr::new(ptr)
  }
}

impl Spanned for TernaryExpr {
  fn span(&self) -> Span {
    self.0.span()
  }
}

impl Serialize for TernaryExpr {
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

#[derive(Debug, Clone, Copy)]
pub struct Var(AstNodePtr);

impl Var {
  pub fn var_symbol(&self) -> SymbolId {
    let node = self.0.get();
    match &node.inner {
      AstNodeKind::Var(symbol) => *symbol,
      _ => unreachable!(),
    }
  }
}

impl Serialize for Var {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: serde::Serializer,
  {
    let mut state = serializer.serialize_tuple_struct("Var", 1)?;
    let symbol = self.var_symbol();
    state.serialize_field(&symbol)?;
    state.end()
  }
}

impl Spanned for Var {
  fn span(&self) -> Span {
    self.0.span()
  }
}

#[derive(Debug, Clone, Copy)]
pub struct AssignExpr(AstNodePtr);

impl AssignExpr {
  pub fn target(&self) -> AssignTarget {
    let ptr = self.0.nth_child(0).unwrap();
    AssignTarget::new(ptr)
  }

  pub fn value(&self) -> Expr {
    let ptr = self.0.nth_child(1).unwrap();
    Expr::new(ptr)
  }
}

impl Serialize for AssignExpr {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: serde::Serializer,
  {
    let target = self.target();
    let value = self.value();
    let mut state = serializer.serialize_struct("AssignExpr", 2)?;
    state.serialize_field("target", &target)?;
    state.serialize_field("value", &value)?;
    state.end()
  }
}

impl Spanned for AssignExpr {
  fn span(&self) -> Span {
    self.0.span()
  }
}

#[derive(Debug, Clone, Copy, Serialize)]
pub enum AssignTarget {
  Ident(Var),
  Member(MemberExpr),
}

impl AssignTarget {
  pub fn new(ptr: AstNodePtr) -> Self {
    match &ptr.get().inner {
      AstNodeKind::Var(_) => Self::Ident(Var(ptr)),
      AstNodeKind::Member(_) => Self::Member(MemberExpr(ptr)),
      _ => unreachable!(),
    }
  }
}

#[derive(Debug, Clone, Copy)]
pub struct LogicExpr(AstNodePtr);

impl LogicExpr {
  pub fn operator(&self) -> LogicalOp {
    match &self.0.get().inner {
      AstNodeKind::LogicExpr(op) => *op,
      _ => unreachable!(),
    }
  }

  pub fn left_operand(&self) -> Expr {
    let left = self.0.nth_child(0).unwrap();
    Expr::new(left)
  }

  pub fn right_operand(&self) -> Expr {
    let right = self.0.nth_child(1).unwrap();
    Expr::new(right)
  }
}

impl Spanned for LogicExpr {
  fn span(&self) -> Span {
    self.0.span()
  }
}

impl Serialize for LogicExpr {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: serde::Serializer,
  {
    let mut state = serializer.serialize_struct("LogicExpr", 3)?;
    let op = self.operator();
    let left = self.left_operand();
    let right = self.right_operand();
    state.serialize_field("op", &op)?;
    state.serialize_field("left_operand", &left)?;
    state.serialize_field("right_operand", &right)?;
    state.end()
  }
}

#[derive(Debug, Clone, Copy)]
pub struct CallExpr(AstNodePtr);

impl CallExpr {
  pub fn callee(&self) -> Expr {
    Expr::new(self.0.nth_child(0).unwrap())
  }

  pub fn argument_list(&self) -> Args {
    Args(self.0.nth_child(1).unwrap())
  }
}

impl Serialize for CallExpr {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: serde::Serializer,
  {
    let mut state = serializer.serialize_struct("CallExpr", 2)?;
    let callee = self.callee();
    let arg_list = self.argument_list();
    state.serialize_field("callee", &callee)?;
    state.serialize_field("argument_list", &arg_list)?;
    state.end()
  }
}

impl Spanned for CallExpr {
  fn span(&self) -> Span {
    self.0.span()
  }
}

#[derive(Debug, Clone, Copy)]
pub struct Args(AstNodePtr);

impl Args {
  pub fn arguments(&self) -> Box<dyn Iterator<Item = Expr>> {
    Box::new(self.0.children().map(Expr::new))
  }
}

impl Serialize for Args {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: serde::Serializer,
  {
    let mut seq = serializer.serialize_seq(None)?;
    for arg in self.arguments() {
      seq.serialize_element(&arg)?;
    }
    seq.end()
  }
}

impl Spanned for Args {
  fn span(&self) -> Span {
    self.0.span()
  }
}

#[derive(Debug, Clone, Copy)]
pub struct MemberExpr(AstNodePtr);

impl MemberExpr {
  pub fn object(&self) -> Expr {
    self.0.nth_child(0).map(Expr::new).unwrap()
  }

  pub fn property(&self) -> SymbolId {
    match self.0.get().inner {
      AstNodeKind::Member(p) => p,
      _ => unreachable!(),
    }
  }
}

impl Serialize for MemberExpr {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: serde::Serializer,
  {
    let mut state = serializer.serialize_struct("MemberExpr", 2)?;
    state.serialize_field("object", &self.object())?;
    state.serialize_field("property", &self.property())?;
    state.end()
  }
}

impl Spanned for MemberExpr {
  fn span(&self) -> Span {
    self.0.span()
  }
}

#[derive(Debug, Clone, Copy)]
pub struct SuperExpr(AstNodePtr);

impl Serialize for SuperExpr {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: serde::Serializer,
  {
    serializer.serialize_str("super")
  }
}

impl Spanned for SuperExpr {
  fn span(&self) -> Span {
    self.0.span()
  }
}
