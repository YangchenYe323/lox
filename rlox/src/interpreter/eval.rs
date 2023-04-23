use crate::ast::{BinaryOp, UnaryOp};

use super::types::LoxValueKind;

pub trait BinaryEval {
  fn evaluate(&self, left_operand: &LoxValueKind, right_operand: &LoxValueKind) -> LoxValueKind;
}

impl BinaryEval for BinaryOp {
  fn evaluate(&self, left_operand: &LoxValueKind, right_operand: &LoxValueKind) -> LoxValueKind {
    match self {
      BinaryOp::EqEq => equals(left_operand, right_operand),
      BinaryOp::BangEq => not_equals(left_operand, right_operand),
      BinaryOp::Less => less(left_operand, right_operand),
      BinaryOp::LessEq => less_equals(left_operand, right_operand),
      BinaryOp::Greater => greater(left_operand, right_operand),
      BinaryOp::GreaterEq => greater_equals(left_operand, right_operand),
      BinaryOp::Plus => add(left_operand, right_operand),
      BinaryOp::Minus => sub(left_operand, right_operand),
      BinaryOp::Mult => mult(left_operand, right_operand),
      BinaryOp::Div => div(left_operand, right_operand),
    }
  }
}

fn add(left_operand: &LoxValueKind, right_operand: &LoxValueKind) -> LoxValueKind {
  match (left_operand, right_operand) {
    (LoxValueKind::Number(l), LoxValueKind::Number(r)) => LoxValueKind::Number(l + r),
    (LoxValueKind::String(l), LoxValueKind::String(r)) => LoxValueKind::String(l.to_string() + r),
    _ => unimplemented!(),
  }
}

fn sub(left_operand: &LoxValueKind, right_operand: &LoxValueKind) -> LoxValueKind {
  match (left_operand, right_operand) {
    (LoxValueKind::Number(l), LoxValueKind::Number(r)) => LoxValueKind::Number(l - r),
    _ => unimplemented!(),
  }
}

fn mult(left_operand: &LoxValueKind, right_operand: &LoxValueKind) -> LoxValueKind {
  match (left_operand, right_operand) {
    (LoxValueKind::Number(l), LoxValueKind::Number(r)) => LoxValueKind::Number(l * r),
    _ => unimplemented!(),
  }
}

fn div(left_operand: &LoxValueKind, right_operand: &LoxValueKind) -> LoxValueKind {
  match (left_operand, right_operand) {
    // TODO(yangchen): div by zero
    (LoxValueKind::Number(l), LoxValueKind::Number(r)) => LoxValueKind::Number(l / r),
    _ => unimplemented!(),
  }
}

fn equals(left_operand: &LoxValueKind, right_operand: &LoxValueKind) -> LoxValueKind {
  match (left_operand, right_operand) {
    (LoxValueKind::Number(l), LoxValueKind::Number(r)) => LoxValueKind::Boolean(l == r),
    (LoxValueKind::String(l), LoxValueKind::String(r)) => LoxValueKind::Boolean(l == r),
    _ => LoxValueKind::Boolean(false),
  }
}

fn not_equals(left_operand: &LoxValueKind, right_operand: &LoxValueKind) -> LoxValueKind {
  match (left_operand, right_operand) {
    (LoxValueKind::Number(l), LoxValueKind::Number(r)) => LoxValueKind::Boolean(l != r),
    (LoxValueKind::String(l), LoxValueKind::String(r)) => LoxValueKind::Boolean(l != r),
    _ => LoxValueKind::Boolean(false),
  }
}

fn less(left_operand: &LoxValueKind, right_operand: &LoxValueKind) -> LoxValueKind {
  match (left_operand, right_operand) {
    (LoxValueKind::Number(l), LoxValueKind::Number(r)) => LoxValueKind::Boolean(l < r),
    (LoxValueKind::String(l), LoxValueKind::String(r)) => LoxValueKind::Boolean(l < r),
    _ => LoxValueKind::Boolean(false),
  }
}

fn less_equals(left_operand: &LoxValueKind, right_operand: &LoxValueKind) -> LoxValueKind {
  match (left_operand, right_operand) {
    (LoxValueKind::Number(l), LoxValueKind::Number(r)) => LoxValueKind::Boolean(l <= r),
    (LoxValueKind::String(l), LoxValueKind::String(r)) => LoxValueKind::Boolean(l <= r),
    _ => LoxValueKind::Boolean(false),
  }
}

fn greater(left_operand: &LoxValueKind, right_operand: &LoxValueKind) -> LoxValueKind {
  match (left_operand, right_operand) {
    (LoxValueKind::Number(l), LoxValueKind::Number(r)) => LoxValueKind::Boolean(l > r),
    (LoxValueKind::String(l), LoxValueKind::String(r)) => LoxValueKind::Boolean(l > r),
    _ => LoxValueKind::Boolean(false),
  }
}

fn greater_equals(left_operand: &LoxValueKind, right_operand: &LoxValueKind) -> LoxValueKind {
  match (left_operand, right_operand) {
    (LoxValueKind::Number(l), LoxValueKind::Number(r)) => LoxValueKind::Boolean(l >= r),
    (LoxValueKind::String(l), LoxValueKind::String(r)) => LoxValueKind::Boolean(l >= r),
    _ => LoxValueKind::Boolean(false),
  }
}

pub trait UnaryEval {
  fn evaluate(&self, operand: &LoxValueKind) -> LoxValueKind;
}

impl UnaryEval for UnaryOp {
  fn evaluate(&self, operand: &LoxValueKind) -> LoxValueKind {
    match self {
      UnaryOp::Not => logical_not(operand),
      UnaryOp::Neg => negate(operand),
    }
  }
}

fn logical_not(operand: &LoxValueKind) -> LoxValueKind {
  match operand {
    LoxValueKind::Boolean(b) => LoxValueKind::Boolean(!b),
    _ => unreachable!(),
  }
}

fn negate(operand: &LoxValueKind) -> LoxValueKind {
  match operand {
    LoxValueKind::Number(n) => LoxValueKind::Number(-n),
    _ => unreachable!(),
  }
}
