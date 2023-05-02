use bytes::{Buf, BufMut};

/// [OpCode] of a bytecode instruction
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum OpCode {
  Return = 0,
}

impl From<u8> for OpCode {
  fn from(value: u8) -> Self {
    match value {
      0 => Self::Return,
      _ => unreachable!(),
    }
  }
}

/// A bytecode instruction
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Instruction {
  Simple(OpCode),
}

impl Instruction {
  pub fn encode(self, mut w: impl BufMut) {
    match self {
      Instruction::Simple(op) => w.put_u8(op as u8),
    }
  }
}

/// Chunk is an abstraction of a sequence of [Instructions] in a packed format
#[derive(Debug, Default)]
pub struct Chunk {
  instructions: Vec<u8>,
}

impl Chunk {
  pub fn add_instruction(&mut self, instr: Instruction) {
    instr.encode(&mut self.instructions);
  }

  pub fn iter(&self) -> ChunkIter<'_> {
    ChunkIter {
      instructions: &self.instructions,
    }
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
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_disassemble() {
    let mut chunk = Chunk::default();
    chunk.add_instruction(Instruction::Simple(OpCode::Return));

    let mut iter = chunk.iter();
    assert_eq!(Some(Instruction::Simple(OpCode::Return)), iter.next());
    assert_eq!(None, iter.next());
  }
}
