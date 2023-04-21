use super::facades::{BinaryExpression, Expression, NumericLiteral, StringLiteral};

pub trait Visiter<'a> {
  type Ret;

  fn visit_expression(&mut self, expr: Expression<'a>) -> Self::Ret;

  fn visit_binary_expression(&mut self, binary_expr: BinaryExpression<'a>) -> Self::Ret;

  fn visit_string_literal(&mut self, string_literal: StringLiteral<'a>) -> Self::Ret;

  fn visit_numeric_literal(&mut self, numeric_literal: NumericLiteral<'a>) -> Self::Ret;
}
