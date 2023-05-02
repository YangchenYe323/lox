use crate::{lineno::LineBuffer, values::Value};

use super::values::ValueArray;
use bytes::{Buf, BufMut};

pub type AddressType = u8;

/// [OpCode] of a bytecode instruction
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum OpCode {
  Return = 0,
  Constant,
}

impl std::fmt::Display for OpCode {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      OpCode::Return => write!(f, "OP_RETURN"),
      OpCode::Constant => write!(f, "OP_CONSTANT"),
    }
  }
}

impl From<u8> for OpCode {
  fn from(value: u8) -> Self {
    // SAFETY: OpCode is only accessed through [Chunk]s, which only allow
    // valid instructions to be added, so there will never be invalid opcode when
    // disassembling
    unsafe { std::mem::transmute(value) }
  }
}

/// A bytecode instruction
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Instruction {
  /// Simple Instructions with no address codes
  Simple(OpCode),
  OneOperand(OpCode, AddressType),
}

impl Instruction {
  pub fn encode(self, mut w: impl BufMut) {
    match self {
      Instruction::Simple(op) => w.put_u8(op as u8),
      Instruction::OneOperand(op, operand) => {
        w.put_u8(op as u8);
        w.put_u8(operand);
      }
    }
  }
}

/// Chunk is an abstraction of a sequence of [Instructions] in a packed format
#[derive(Debug, Default)]
pub struct Chunk {
  constants: ValueArray,
  instructions: Vec<u8>,
  line_numbers: LineBuffer,
}

impl Chunk {
  pub fn add_instruction(&mut self, instr: Instruction, line: u32) {
    instr.encode(&mut self.instructions);
    self.line_numbers.push(line);
  }

  pub fn add_constant(&mut self, constant: Value) {
    self.constants.push(constant);
  }

  pub fn iter(&self) -> ChunkIter<'_> {
    ChunkIter {
      original: self.instructions.as_ptr(),
      instructions: &self.instructions,
    }
  }

  pub fn disassemble(&self, name: &str, w: &mut impl std::fmt::Write) -> std::fmt::Result {
    writeln!(w, "== {} ==", name)?;
    let mut iter = self.iter();
    let mut idx = 0;

    while let Some((offset, instruction)) = iter.next_with_offset() {
      // disassemble offset in binary
      write!(w, "{:#08x} ", offset)?;

      // disassemble line number
      let current_line = self.line_numbers.get_lineno(idx);
      if idx > 0 && current_line == self.line_numbers.get_lineno(idx - 1) {
        write!(w, "   | ")?;
      } else {
        write!(w, "{:4} ", current_line)?;
      }
      idx += 1;

      // disassemble instruction
      match instruction {
        Instruction::Simple(op) => writeln!(w, "{}", op)?,
        Instruction::OneOperand(op, offset) => {
          let value = self.constants.value_at(offset);
          writeln!(w, "{}    {} '{}'", op, offset, value)?;
        }
      }
    }
    Ok(())
  }
}

impl std::fmt::Display for Chunk {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    self.disassemble("Chunk", f)
  }
}

pub struct ChunkIter<'a> {
  original: *const u8,
  instructions: &'a [u8],
}

impl<'a> ChunkIter<'a> {
  pub fn current_offset(&self) -> usize {
    // SAFETY: original and instructions are derived from the same object.
    unsafe { self.instructions.as_ptr().offset_from(self.original) as usize }
  }

  pub fn next_with_offset(&mut self) -> Option<(usize, Instruction)> {
    let offset = self.current_offset();
    self.next().map(|instruction| (offset, instruction))
  }
}

impl<'a> Iterator for ChunkIter<'a> {
  type Item = Instruction;

  fn next(&mut self) -> Option<Self::Item> {
    if self.instructions.remaining() == 0 {
      return None;
    }
    let op = OpCode::from(self.instructions.get_u8());
    match op {
      OpCode::Return => Some(Instruction::Simple(OpCode::Return)),
      OpCode::Constant => {
        let operand = self.instructions.get_u8();
        Some(Instruction::OneOperand(OpCode::Constant, operand))
      }
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn ret() {
    let mut chunk = Chunk::default();
    chunk.add_instruction(Instruction::Simple(OpCode::Return), 0);

    let mut iter = chunk.iter();
    assert_eq!(Some(Instruction::Simple(OpCode::Return)), iter.next());
    assert_eq!(None, iter.next());
  }

  #[test]
  fn constant() {
    let mut chunk = Chunk::default();
    chunk.add_constant(Value::Number(1.0));
    chunk.add_instruction(Instruction::OneOperand(OpCode::Constant, 0), 0);
    chunk.add_instruction(Instruction::Simple(OpCode::Return), 0);
    let mut iter = chunk.iter();

    assert_eq!(
      Some(Instruction::OneOperand(OpCode::Constant, 0)),
      iter.next()
    );
    assert_eq!(2, iter.current_offset());
    assert_eq!(Some(Instruction::Simple(OpCode::Return)), iter.next());
    assert_eq!(None, iter.next());
  }
}
