//! Code copied from [rust](https://github.com/rust-lang/rust/blob/master/compiler/rustc_span/src/symbol.rs)

use rustc_hash::FxHashMap;

use crate::arena::DroplessArena;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SymbolId(u32);

/// [Interner] is responsible for assigning all the symbols (literals, identifiers) in the program a unique
/// id and make sure the same symbol is stored once, and can be compare by address.
#[derive(Default)]
pub struct Interner {
  // The `&'static str`s in this type actually point into the arena.
  //
  // This type is private to prevent accidentally constructing more than one
  // `Interner` on the same thread, which makes it easy to mix up `Symbol`s
  // between `Interner`s.
  arena: DroplessArena,
  names: FxHashMap<&'static str, SymbolId>,
  strings: Vec<&'static str>,
}

impl std::fmt::Debug for Interner {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.debug_struct("Interner")
      .field("names", &self.names)
      .finish()
  }
}

impl Interner {
  pub fn intern(&mut self, string: &str) -> SymbolId {
    if let Some(&name) = self.names.get(string) {
      return name;
    }

    let name = SymbolId(self.strings.len() as u32);

    // SAFETY: we convert from `&str` to `&[u8]`, clone it into the arena,
    // and immediately convert the clone back to `&[u8]`, all because there
    // is no `inner.arena.alloc_str()` method. This is clearly safe.
    let string: &str =
      unsafe { std::str::from_utf8_unchecked(self.arena.alloc_slice(string.as_bytes())) };

    // SAFETY: we can extend the arena allocation to `'static` because we
    // only access these while the arena is still alive.
    let string: &'static str = unsafe { &*(string as *const str) };
    self.strings.push(string);

    // This second hash table lookup can be avoided by using `RawEntryMut`,
    // but this code path isn't hot enough for it to be worth it. See
    // #91445 for details.
    self.names.insert(string, name);
    name
  }
}
