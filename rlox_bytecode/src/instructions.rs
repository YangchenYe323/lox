use crate::{lineno::LineBuffer, values::Value};

use super::values::ValueArray;
use bytes::{Buf, BufMut};

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
  OneOperand(OpCode, u8),
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
      instructions: &self.instructions,
    }
  }

  pub fn iter_with_offset(&self) -> ChunkIterWithOffset<'_> {
    let inner = self.iter();
    let original = self.instructions.as_ptr();
    ChunkIterWithOffset { original, inner }
  }

  pub fn disassemble(&self, name: &str, w: &mut impl std::fmt::Write) -> std::fmt::Result {
    writeln!(w, "== {} ==", name)?;
    let iter = self.iter_with_offset();
    for (idx, (offset, instruction)) in iter.enumerate() {
      let idx = idx as u32;
      // disassemble offset in binary
      write!(w, "{:#08x} ", offset)?;

      // disassemble line number
      let current_line = self.line_numbers.get_lineno(idx);
      if idx > 0 && current_line == self.line_numbers.get_lineno(idx - 1) {
        write!(w, "   | ")?;
      } else {
        write!(w, "{:4} ", current_line)?;
      }

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
  instructions: &'a [u8],
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

pub struct ChunkIterWithOffset<'a> {
  original: *const u8,
  inner: ChunkIter<'a>,
}

impl<'a> Iterator for ChunkIterWithOffset<'a> {
  type Item = (usize, Instruction);

  fn next(&mut self) -> Option<Self::Item> {
    // SAFETY: original and instructions are derived from the same object.
    let offset = unsafe { self.inner.instructions.as_ptr().offset_from(self.original) as usize };
    self.inner.next().map(|i| (offset, i))
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
    let mut iter = chunk.iter_with_offset();

    assert_eq!(
      Some((0, Instruction::OneOperand(OpCode::Constant, 0))),
      iter.next()
    );
    assert_eq!(Some((2, Instruction::Simple(OpCode::Return))), iter.next());
    assert_eq!(None, iter.next());
  }
}
