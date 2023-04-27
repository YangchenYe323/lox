use bitflags::bitflags;

use super::Parser;

bitflags! {
  #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
  pub struct ParserContextFlags: u32 {
    const IN_LOOP = 1 << 0;
    const IN_FUNCTION_DECL = 1 << 1;
  }
}

impl Parser {
  pub fn in_function(&self) -> bool {
    self.context.contains(ParserContextFlags::IN_FUNCTION_DECL)
  }

  pub fn in_loop(&self) -> bool {
    self.context.contains(ParserContextFlags::IN_LOOP)
  }

  pub fn with_context<T>(
    &mut self,
    context: ParserContextFlags,
    f: impl FnOnce(&mut Parser) -> T,
  ) -> T {
    let old_context = self.context;
    let new_context = old_context | context;
    self.context = new_context;
    let result = f(self);
    self.context = old_context;
    result
  }
}
