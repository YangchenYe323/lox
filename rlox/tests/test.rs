use assert_cmd::cargo::CommandCargoExt;
use std::{
  path::{Path, PathBuf},
  process::Command,
};

const TEST_ROOT: &str = "lox_files";

fn interpret_lox_file(path: impl AsRef<Path>) -> String {
  let path = path.as_ref();
  let mut interpreter = Command::cargo_bin("rlox").unwrap();
  let output = interpreter.arg(path).output().unwrap();
  String::from_utf8_lossy(&output.stdout).into_owned()
}

fn test_source(name: &'static str) {
  let path = PathBuf::from(TEST_ROOT).join(format!("{name}.lox"));
  let output = interpret_lox_file(&path);
  insta::assert_snapshot!(name, output);
}

// Calls test_soruce on each respective file under `TEST_ROOT` directory
rlox_macros::declare_interpreter_tests! {
  class::member,
  class::method,
  conditionals::if1,
  expression::assign,
  loops::nested_while,
  loops::simple_while,
  function::print,
  function::recursion,
  function::fib,
  function::default,
  function::ret,
  function::closure,
  function::scope_rule,
  function::hof,
  comp_errors::print,
  comp_errors::invalid_return,
  comp_errors::invalid_break,
  comp_errors::params,
  comp_errors::args,
}
