use super::facades::{
  AssignExpr, BinaryExpr, Block, BoolLit, BreakStmt, CallExpr, Expr, ExprStmt, FnDecl, IfStmt,
  LogicExpr, NilLit, NumericLit, Program, ReturnStmt, Stmt, StringLit, TernaryExpr, UnaryExpr, Var,
  VarDecl, WhileStmt,
};

pub trait AstVisitor {
  type Ret;

  fn visit_program(&mut self, program: Program) -> Self::Ret;

  fn visit_variable_declaration(&mut self, var_decl: VarDecl) -> Self::Ret;

  fn visit_function_declaration(&mut self, fn_decl: FnDecl) -> Self::Ret;

  fn visit_statement(&mut self, stmt: Stmt) -> Self::Ret;

  fn visit_break_statement(&mut self, break_stmt: BreakStmt) -> Self::Ret;

  fn visit_return_statement(&mut self, return_stmt: ReturnStmt) -> Self::Ret;

  fn visit_expression_statement(&mut self, expr_stmt: ExprStmt) -> Self::Ret;

  fn visit_block(&mut self, block: Block) -> Self::Ret;

  fn visit_while_statement(&mut self, while_stmt: WhileStmt) -> Self::Ret;

  fn visit_if_statement(&mut self, if_stmt: IfStmt) -> Self::Ret;

  fn visit_expression(&mut self, expr: Expr) -> Self::Ret;

  fn visit_call_expression(&mut self, call_expr: CallExpr) -> Self::Ret;

  fn visit_assignment_expression(&mut self, expr: AssignExpr) -> Self::Ret;

  fn visit_binary_expression(&mut self, binary_expr: BinaryExpr) -> Self::Ret;

  fn visit_logic_expression(&mut self, logic_expr: LogicExpr) -> Self::Ret;

  fn visit_ternary_expression(&mut self, ternary_expr: TernaryExpr) -> Self::Ret;

  fn visit_unary_expression(&mut self, unary_expr: UnaryExpr) -> Self::Ret;

  fn visit_string_literal(&mut self, string_literal: StringLit) -> Self::Ret;

  fn visit_numeric_literal(&mut self, numeric_literal: NumericLit) -> Self::Ret;

  fn visit_bool_literal(&mut self, bool_literal: BoolLit) -> Self::Ret;

  fn visit_var_reference(&mut self, var_reference: Var) -> Self::Ret;

  fn visit_nil(&mut self, nil: NilLit) -> Self::Ret;
}
