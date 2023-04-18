use std::{
  io::{stdin, stdout, Write},
  path::PathBuf,
};

use clap::Parser;
use miette::Result;
use rlox::Interpreter;

#[derive(Debug, Parser)]
#[command(author, version, about, long_about = None)]
pub struct Args {
  pub file: Option<PathBuf>,
}

fn main() -> Result<()> {
  let Args { file } = Args::parse();
  let mut interpreter = Interpreter::new();
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
  for source_line in prompt_stream() {
    let source = source_line.strip_suffix("\n").unwrap();
    if let Err(report) = interpreter.run(source) {
      println!("{}", report);
    }
  }
  Ok(())
}

fn prompt_stream() -> impl Iterator<Item = String> {
  const PROMPT: &str = "> ";
  /// Handles outputting a prompt and getting back a user-typed source string
  struct PromptStream;
  impl Iterator for PromptStream {
    type Item = String;
    fn next(&mut self) -> Option<Self::Item> {
      let mut out = stdout();
      write!(&mut out, "{}", PROMPT).unwrap();
      out.flush().unwrap();
      let mut s = String::new();
      stdin().read_line(&mut s).ok()?;
      Some(s)
    }
  }
  PromptStream
}
