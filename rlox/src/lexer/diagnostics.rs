use miette::Diagnostic;
use thiserror::Error;

#[derive(Debug, Error, Diagnostic)]
#[error("Unexpected character {0} on line {1}")]
pub struct UnexpectedCharacter(pub char, pub u32);
