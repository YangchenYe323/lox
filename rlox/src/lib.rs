#![warn(rust_2018_idioms, missing_debug_implementations)]

//! This crate implements a tree-walking interpreter in rust from [crafting-interpreters](https://craftinginterpreters.com/)

mod lexer;

use miette::{GraphicalReportHandler, Result};

use crate::lexer::Lex;

/// The interpreter that handles interpreting and executing source code.
#[derive(Debug)]
pub struct Interpreter {
  reporter: GraphicalReportHandler,
}

impl Default for Interpreter {
  fn default() -> Self {
    Self::new()
  }
}

impl Interpreter {
  pub fn new() -> Self {
    Self {
      reporter: GraphicalReportHandler::new(),
    }
  }

  /// Interprete and run a lox source string
  pub fn run(&mut self, source: &'_ str) -> Result<()> {
    println!("{:?}", source);

    let lex = lexer::lex(source);

    match lex {
      Lex::Success(tokens) => {
        println!("{:?}", tokens);
      }
      Lex::Failure(reports) => {
        for report in reports {
          let mut out = String::new();
          let report = report.with_source_code(source.to_string());
          self
            .reporter
            .render_report(&mut out, report.as_ref())
            .unwrap();
          println!("{}", out);
        }
      }
    }

    Ok(())
  }
}
