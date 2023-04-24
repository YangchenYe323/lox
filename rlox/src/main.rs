#![allow(clippy::uninlined_format_args)]

use std::path::PathBuf;

use clap::Parser;
use miette::Result;
use rlox::Interpreter;
use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;

#[derive(Debug, Parser)]
#[command(author, version, about, long_about = None)]
pub struct Args {
  pub file: Option<PathBuf>,
}

fn main() -> Result<()> {
  let Args { file } = Args::parse();
  let mut interpreter = Interpreter::default();
  match file {
    Some(file) => run_file(file, &mut interpreter),
    None => run_interactive(&mut interpreter),
  }
}

fn run_file(file: PathBuf, interpreter: &mut Interpreter) -> Result<()> {
  let source = std::fs::read_to_string(file.as_path()).unwrap();
  interpreter.run(&source)?;
  Ok(())
}

fn run_interactive(interpreter: &mut Interpreter) -> Result<()> {
  let mut rl = DefaultEditor::new().unwrap();

  loop {
    let readline = rl.readline("> ");
    match readline {
      Ok(line) => {
        if let Err(report) = interpreter.run(&line) {
          println!("{}", report);
        }
        rl.add_history_entry(line).unwrap();
      }
      Err(ReadlineError::Interrupted) => {
        println!("CTRL-C");
      }
      Err(ReadlineError::Eof) => {
        break;
      }
      Err(err) => {
        println!("Error: {:?}", err);
        break;
      }
    }
  }
  Ok(())
}
