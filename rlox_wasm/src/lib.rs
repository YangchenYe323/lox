use miette::{Diagnostic, GraphicalReportHandler, GraphicalTheme, Report};
use rlox::{
  interpreter::{Interpreter, SpannedLoxRuntimeError},
  lexer::LexerError,
  parser::{Parse, ParserError},
};
use rlox_ast::{facades::Program, visit::AstVisitor};
use serde::Serialize;
use wasm_bindgen::prelude::*;
use wasm_typescript_definition::TypescriptDefinition;

#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[derive(Serialize, TypescriptDefinition)]
pub enum Error {
  LexError(LexError),
  ParseError(ParseError),
  RuntimeError(String),
}

#[derive(Serialize, TypescriptDefinition)]
pub struct LexError(Vec<String>);

#[derive(Serialize, TypescriptDefinition)]
pub struct ParseError {
  recovered: Vec<String>,
  unrecoverable: Option<String>,
}

#[derive(Serialize, TypescriptDefinition)]
#[serde(tag = "code")]
pub enum InterpretResult {
  Error(Error),
  Success {
    return_value: String,
    stdout: String,
  },
}

#[wasm_bindgen]
pub struct InterpreterHandle {
  interpreter: Box<Interpreter>,
  reporter: Box<GraphicalReportHandler>,
}

#[wasm_bindgen]
impl InterpreterHandle {
  #[wasm_bindgen(constructor)]
  pub fn new() -> InterpreterHandle {
    InterpreterHandle {
      interpreter: Box::new(Interpreter::default()),
      reporter: Box::new(GraphicalReportHandler::new_themed(
        GraphicalTheme::unicode_nocolor(),
      )),
    }
  }

  #[wasm_bindgen]
  pub fn interprete(&mut self, source: &str) -> JsValue {
    let parse = rlox::parser::parse_source_program(source);

    let program = match parse {
      Parse::Success(tree) => Program::new(tree.root_ptr()),
      Parse::ParseError {
        recovered,
        unrecoverrable,
      } => {
        let error = self.report_parse_error(source, recovered, unrecoverrable);
        let value = InterpretResult::Error(error);
        return serde_wasm_bindgen::to_value(&value).unwrap();
      }
      Parse::LexError(errors) => {
        let error = self.report_lex_error(source, errors);
        let value = InterpretResult::Error(error);
        return serde_wasm_bindgen::to_value(&value).unwrap();
      }
    };

    let result = self.interpreter.visit_program(program);

    match result {
      Ok(returned) => {
        let returned = self.interpreter.value_to_string(&returned);
        let stdout = self.interpreter.drain_output();
        let value = InterpretResult::Success {
          return_value: returned,
          stdout,
        };
        serde_wasm_bindgen::to_value(&value).unwrap()
      }
      Err(e) => {
        let error = self.report_runtime_error(source, e);
        let value = InterpretResult::Error(error);
        serde_wasm_bindgen::to_value(&value).unwrap()
      }
    }
  }

  fn report_parse_error(
    &self,
    source: &str,
    recovered: Vec<ParserError>,
    unrecoverrable: Option<ParserError>,
  ) -> Error {
    let recovered = recovered
      .into_iter()
      .map(|error| self.render_error(error, source))
      .collect();
    let unrecoverable = unrecoverrable.map(|error| self.render_error(error, source));
    Error::ParseError(ParseError {
      recovered,
      unrecoverable,
    })
  }

  fn report_lex_error(&self, source: &str, errors: Vec<LexerError>) -> Error {
    let errors = errors
      .into_iter()
      .map(|e| self.render_error(e, source))
      .collect();
    Error::LexError(LexError(errors))
  }

  fn report_runtime_error(&self, source: &str, error: SpannedLoxRuntimeError) -> Error {
    let error = self.render_error(error, source);
    Error::RuntimeError(error)
  }

  fn render_error(&self, error: impl Diagnostic + Send + Sync + 'static, source: &str) -> String {
    let report = Report::from(error).with_source_code(source.to_string());
    let mut s = String::new();
    self
      .reporter
      .render_report(&mut s, report.as_ref())
      .unwrap();
    s
  }
}
