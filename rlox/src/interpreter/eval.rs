use crate::ast::{facades::Expr, visit::AstVisitor, BinaryOp, UnaryOp};

use super::{
  diagnostics::{LoxRuntimeError, SpannedLoxRuntimeError},
  types::LoxValueKind,
  Evaluator,
};

pub type EvalResult<T, E = LoxRuntimeError> = std::result::Result<T, E>;

pub trait BinaryEval {
  fn evaluate(
    &self,
    left_operand: &LoxValueKind,
    right_operand: &LoxValueKind,
  ) -> EvalResult<LoxValueKind>;
}

impl BinaryEval for BinaryOp {
  fn evaluate(
    &self,
    left_operand: &LoxValueKind,
    right_operand: &LoxValueKind,
  ) -> EvalResult<LoxValueKind> {
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

fn add(left_operand: &LoxValueKind, right_operand: &LoxValueKind) -> EvalResult<LoxValueKind> {
  match (left_operand, right_operand) {
    (LoxValueKind::Number(l), LoxValueKind::Number(r)) => Ok(LoxValueKind::Number(l + r)),
    (LoxValueKind::String(l), LoxValueKind::String(r)) => {
      Ok(LoxValueKind::String(l.to_string() + r))
    }
    (LoxValueKind::String(l), val) => Ok(LoxValueKind::String(format!("{}{}", l, val))),
    (val, LoxValueKind::String(l)) => Ok(LoxValueKind::String(format!("{}{}", val, l))),
    (left, right) => Err(LoxRuntimeError::BinaryOpTypeError(
      "+",
      left.type_name(),
      right.type_name(),
    )),
  }
}

fn sub(left_operand: &LoxValueKind, right_operand: &LoxValueKind) -> EvalResult<LoxValueKind> {
  match (left_operand, right_operand) {
    (LoxValueKind::Number(l), LoxValueKind::Number(r)) => Ok(LoxValueKind::Number(l - r)),
    (left, right) => Err(LoxRuntimeError::BinaryOpTypeError(
      "-",
      left.type_name(),
      right.type_name(),
    )),
  }
}

fn mult(left_operand: &LoxValueKind, right_operand: &LoxValueKind) -> EvalResult<LoxValueKind> {
  match (left_operand, right_operand) {
    (LoxValueKind::Number(l), LoxValueKind::Number(r)) => Ok(LoxValueKind::Number(l * r)),
    (left, right) => Err(LoxRuntimeError::BinaryOpTypeError(
      "*",
      left.type_name(),
      right.type_name(),
    )),
  }
}

fn div(left_operand: &LoxValueKind, right_operand: &LoxValueKind) -> EvalResult<LoxValueKind> {
  match (left_operand, right_operand) {
    // TODO(yangchen): div by zero
    (LoxValueKind::Number(l), LoxValueKind::Number(r)) => match *r == 0.0 {
      true => Err(LoxRuntimeError::DivideByZero),
      false => Ok(LoxValueKind::Number(l / r)),
    },
    (left, right) => Err(LoxRuntimeError::BinaryOpTypeError(
      "/",
      left.type_name(),
      right.type_name(),
    )),
  }
}

fn equals(left_operand: &LoxValueKind, right_operand: &LoxValueKind) -> EvalResult<LoxValueKind> {
  match (left_operand, right_operand) {
    (LoxValueKind::Number(l), LoxValueKind::Number(r)) => Ok(LoxValueKind::Boolean(l == r)),
    (LoxValueKind::String(l), LoxValueKind::String(r)) => Ok(LoxValueKind::Boolean(l == r)),
    _ => Ok(LoxValueKind::Boolean(false)),
  }
}

fn not_equals(
  left_operand: &LoxValueKind,
  right_operand: &LoxValueKind,
) -> EvalResult<LoxValueKind> {
  match (left_operand, right_operand) {
    (LoxValueKind::Number(l), LoxValueKind::Number(r)) => Ok(LoxValueKind::Boolean(l != r)),
    (LoxValueKind::String(l), LoxValueKind::String(r)) => Ok(LoxValueKind::Boolean(l != r)),
    _ => Ok(LoxValueKind::Boolean(false)),
  }
}

fn less(left_operand: &LoxValueKind, right_operand: &LoxValueKind) -> EvalResult<LoxValueKind> {
  match (left_operand, right_operand) {
    (LoxValueKind::Number(l), LoxValueKind::Number(r)) => Ok(LoxValueKind::Boolean(l < r)),
    (LoxValueKind::String(l), LoxValueKind::String(r)) => Ok(LoxValueKind::Boolean(l < r)),
    (left, right) => Err(LoxRuntimeError::BinaryOpTypeError(
      "<",
      left.type_name(),
      right.type_name(),
    )),
  }
}

fn less_equals(
  left_operand: &LoxValueKind,
  right_operand: &LoxValueKind,
) -> EvalResult<LoxValueKind> {
  match (left_operand, right_operand) {
    (LoxValueKind::Number(l), LoxValueKind::Number(r)) => Ok(LoxValueKind::Boolean(l <= r)),
    (LoxValueKind::String(l), LoxValueKind::String(r)) => Ok(LoxValueKind::Boolean(l <= r)),
    (left, right) => Err(LoxRuntimeError::BinaryOpTypeError(
      "<=",
      left.type_name(),
      right.type_name(),
    )),
  }
}

fn greater(left_operand: &LoxValueKind, right_operand: &LoxValueKind) -> EvalResult<LoxValueKind> {
  match (left_operand, right_operand) {
    (LoxValueKind::Number(l), LoxValueKind::Number(r)) => Ok(LoxValueKind::Boolean(l > r)),
    (LoxValueKind::String(l), LoxValueKind::String(r)) => Ok(LoxValueKind::Boolean(l > r)),
    (left, right) => Err(LoxRuntimeError::BinaryOpTypeError(
      ">",
      left.type_name(),
      right.type_name(),
    )),
  }
}

fn greater_equals(
  left_operand: &LoxValueKind,
  right_operand: &LoxValueKind,
) -> EvalResult<LoxValueKind> {
  match (left_operand, right_operand) {
    (LoxValueKind::Number(l), LoxValueKind::Number(r)) => Ok(LoxValueKind::Boolean(l >= r)),
    (LoxValueKind::String(l), LoxValueKind::String(r)) => Ok(LoxValueKind::Boolean(l >= r)),
    (left, right) => Err(LoxRuntimeError::BinaryOpTypeError(
      ">=",
      left.type_name(),
      right.type_name(),
    )),
  }
}

pub(crate) fn logical_and(
  evaluator: &mut Evaluator,
  left: Expr<'_>,
  right: Expr<'_>,
) -> Result<LoxValueKind, SpannedLoxRuntimeError> {
  let left_value = evaluator.visit_expression(left)?;
  if left_value.is_truthful() {
    Ok(evaluator.visit_expression(right)?)
  } else {
    Ok(left_value)
  }
}

pub(crate) fn logical_or(
  evaluator: &mut Evaluator,
  left: Expr<'_>,
  right: Expr<'_>,
) -> Result<LoxValueKind, SpannedLoxRuntimeError> {
  let left_value = evaluator.visit_expression(left)?;
  if !left_value.is_truthful() {
    Ok(evaluator.visit_expression(right)?)
  } else {
    Ok(left_value)
  }
}

pub trait UnaryEval {
  fn evaluate(&self, operand: &LoxValueKind) -> EvalResult<LoxValueKind>;
}

impl UnaryEval for UnaryOp {
  fn evaluate(&self, operand: &LoxValueKind) -> EvalResult<LoxValueKind> {
    match self {
      UnaryOp::Not => logical_not(operand),
      UnaryOp::Neg => negate(operand),
    }
  }
}

fn logical_not(operand: &LoxValueKind) -> EvalResult<LoxValueKind> {
  match operand {
    LoxValueKind::Boolean(b) => Ok(LoxValueKind::Boolean(!b)),
    operand => Err(LoxRuntimeError::UnaryOpTypeError("!", operand.type_name())),
  }
}

fn negate(operand: &LoxValueKind) -> EvalResult<LoxValueKind> {
  match operand {
    LoxValueKind::Number(n) => Ok(LoxValueKind::Number(-n)),
    operand => Err(LoxRuntimeError::UnaryOpTypeError("!", operand.type_name())),
  }
}
