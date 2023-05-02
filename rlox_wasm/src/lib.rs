use miette::{Diagnostic, GraphicalReportHandler, GraphicalTheme, Report};
use rlox::{
  interpreter::{Interpreter, SpannedLoxRuntimeError},
  lexer::LexerError,
  parser::{Parse, ParserError},
};
use rlox_ast::{facades::Program, visit::AstVisitor};
use wasm_bindgen::prelude::*;

use crate::types::InterpreterResult;

mod types;

#[wasm_bindgen]
pub struct InterpreterHandle {
  interpreter: Box<Interpreter>,
  reporter: Box<GraphicalReportHandler>,
}

impl Default for InterpreterHandle {
  fn default() -> Self {
    Self::new()
  }
}

#[wasm_bindgen]
impl InterpreterHandle {
  #[wasm_bindgen(constructor)]
  pub fn new() -> InterpreterHandle {
    InterpreterHandle {
      interpreter: Box::<Interpreter>::default(),
      reporter: Box::new(GraphicalReportHandler::new_themed(
        GraphicalTheme::unicode_nocolor(),
      )),
    }
  }

  #[wasm_bindgen]
  pub fn interprete(&mut self, source: &str) -> InterpreterResult {
    let parse = rlox::parser::parse_source_program(source);

    let program = match parse {
      Parse::Success(tree) => Program::new(tree.root_ptr()),
      Parse::ParseError {
        recovered,
        unrecoverrable,
      } => {
        let error = self.report_parse_error(source, recovered, unrecoverrable);
        return InterpreterResult::new(false, error);
      }
      Parse::LexError(errors) => {
        let error = self.report_lex_error(source, errors);
        return InterpreterResult::new(false, error);
      }
    };

    let result = self.interpreter.visit_program(program);

    match result {
      Ok(returned) => {
        let returned = returned.to_string();
        let stdout = self.interpreter.drain_output();
        let output = stdout + &returned;
        InterpreterResult::new(true, output)
      }
      Err(e) => {
        let error = self.report_runtime_error(source, e);
        InterpreterResult::new(false, error)
      }
    }
  }

  fn report_parse_error(
    &self,
    source: &str,
    recovered: Vec<ParserError>,
    unrecoverrable: Option<ParserError>,
  ) -> String {
    let mut buf = String::new();
    for recovered_error in recovered {
      self.render_error(&mut buf, recovered_error, source);
    }
    if let Some(error) = unrecoverrable {
      self.render_error(&mut buf, error, source);
    }
    buf
  }

  fn report_lex_error(&self, source: &str, errors: Vec<LexerError>) -> String {
    let mut buf = String::new();
    for err in errors {
      self.render_error(&mut buf, err, source);
    }
    buf
  }

  fn report_runtime_error(&self, source: &str, error: SpannedLoxRuntimeError) -> String {
    let mut buf = String::new();
    self.render_error(&mut buf, error, source);
    buf
  }

  fn render_error(
    &self,
    buf: &mut String,
    error: impl Diagnostic + Send + Sync + 'static,
    source: &str,
  ) {
    let report = Report::from(error).with_source_code(source.to_string());
    self.reporter.render_report(buf, report.as_ref()).unwrap();
  }
}
