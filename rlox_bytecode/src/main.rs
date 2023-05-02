use rlox_bytecode::{Chunk, Instruction, OpCode, Value};

fn main() {
  let mut c = Chunk::default();
  c.add_instruction(Instruction::Simple(OpCode::Return), 0);
  c.add_instruction(Instruction::Simple(OpCode::Return), 0);
  c.add_instruction(Instruction::Simple(OpCode::Return), 0);
  c.add_constant(Value::Number(1.0));
  c.add_instruction(Instruction::OneOperand(OpCode::Constant, 0), 0);
  c.add_instruction(Instruction::Simple(OpCode::Return), 0);
  println!("{}", c);
}
