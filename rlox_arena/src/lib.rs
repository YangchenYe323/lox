#![feature(dropck_eyepatch, new_uninit, strict_provenance)]
#![allow(clippy::mut_from_ref)]

//! Code copied from [rust](https://github.com/rust-lang/rust/blob/master/compiler/rustc_arena/src/lib.rs)

use std::{
  alloc::Layout,
  cell::{Cell, RefCell},
  mem::MaybeUninit,
  ptr::NonNull,
};

/// [ArenaChunk] represents a contiguous memory chunk in the arena
struct ArenaChunk<T = u8> {
  /// The raw storage for the arena chunk.
  storage: NonNull<[MaybeUninit<T>]>,
}

unsafe impl<#[may_dangle] T> Drop for ArenaChunk<T> {
  fn drop(&mut self) {
    unsafe { Box::from_raw(self.storage.as_mut()) };
  }
}

impl<T> ArenaChunk<T> {
  #[inline]
  unsafe fn new(capacity: usize) -> ArenaChunk<T> {
    ArenaChunk {
      storage: NonNull::new(Box::into_raw(Box::new_uninit_slice(capacity))).unwrap(),
    }
  }

  // Returns a pointer to the first allocated object.
  #[inline]
  fn start(&mut self) -> *mut T {
    self.storage.as_ptr() as *mut T
  }

  // Returns a pointer to the end of the allocated space.
  #[inline]
  fn end(&mut self) -> *mut T {
    unsafe {
      if std::mem::size_of::<T>() == 0 {
        // A pointer as large as possible for zero-sized elements.
        std::ptr::invalid_mut(!0)
      } else {
        self.start().add((*self.storage.as_ptr()).len())
      }
    }
  }
}

// The arenas start with PAGE-sized chunks, and then each new chunk is twice as
// big as its predecessor, up until we reach HUGE_PAGE-sized chunks, whereupon
// we stop growing. This scales well, from arenas that are barely used up to
// arenas that are used for 100s of MiBs. Note also that the chosen sizes match
// the usual sizes of pages and huge pages on Linux.
const PAGE: usize = 4096;
const HUGE_PAGE: usize = 2 * 1024 * 1024;

/// An arena that can hold objects of multiple different types that impl `Copy`
/// and/or satisfy `!mem::needs_drop`.
pub struct DroplessArena {
  /// A pointer to the start of the free space.
  start: Cell<*mut u8>,

  /// A pointer to the end of free space.
  ///
  /// The allocation proceeds downwards from the end of the chunk towards the
  /// start. (This is slightly simpler and faster than allocating upwards,
  /// see <https://fitzgeraldnick.com/2019/11/01/always-bump-downwards.html>.)
  /// When this pointer crosses the start pointer, a new chunk is allocated.
  end: Cell<*mut u8>,

  /// A vector of arena chunks.
  chunks: RefCell<Vec<ArenaChunk>>,
}

unsafe impl Send for DroplessArena {}

impl Default for DroplessArena {
  #[inline]
  fn default() -> DroplessArena {
    DroplessArena {
      start: Cell::new(std::ptr::null_mut()),
      end: Cell::new(std::ptr::null_mut()),
      chunks: Default::default(),
    }
  }
}

impl DroplessArena {
  #[inline(never)]
  #[cold]
  fn grow(&self, additional: usize) {
    unsafe {
      let mut chunks = self.chunks.borrow_mut();
      let mut new_cap;
      if let Some(last_chunk) = chunks.last_mut() {
        // There is no need to update `last_chunk.entries` because that
        // field isn't used by `DroplessArena`.

        // If the previous chunk's len is less than HUGE_PAGE
        // bytes, then this chunk will be least double the previous
        // chunk's size.
        new_cap = (*last_chunk.storage.as_ptr()).len().min(HUGE_PAGE / 2);
        new_cap *= 2;
      } else {
        new_cap = PAGE;
      }
      // Also ensure that this chunk can fit `additional`.
      new_cap = std::cmp::max(additional, new_cap);

      let mut chunk = ArenaChunk::new(new_cap);
      self.start.set(chunk.start());
      self.end.set(chunk.end());
      chunks.push(chunk);
    }
  }

  /// Allocates a byte slice with specified layout from the current memory
  /// chunk. Returns `None` if there is no free space left to satisfy the
  /// request.
  #[inline]
  fn alloc_raw_without_grow(&self, layout: Layout) -> Option<*mut u8> {
    let start = self.start.get().addr();
    let old_end = self.end.get();
    let end = old_end.addr();

    let align = layout.align();
    let bytes = layout.size();

    let new_end = end.checked_sub(bytes)? & !(align - 1);
    if start <= new_end {
      let new_end = old_end.with_addr(new_end);
      self.end.set(new_end);
      Some(new_end)
    } else {
      None
    }
  }

  #[inline]
  pub fn alloc_raw(&self, layout: Layout) -> *mut u8 {
    assert!(layout.size() != 0);
    loop {
      if let Some(a) = self.alloc_raw_without_grow(layout) {
        break a;
      }
      // No free space left. Allocate a new chunk to satisfy the request.
      // On failure the grow will panic or abort.
      self.grow(layout.size());
    }
  }

  #[inline]
  #[allow(dead_code)]
  pub fn alloc<T>(&self, object: T) -> &mut T {
    assert!(!std::mem::needs_drop::<T>());

    let mem = self.alloc_raw(Layout::for_value::<T>(&object)) as *mut T;

    unsafe {
      // Write into uninitialized memory.
      std::ptr::write(mem, object);
      &mut *mem
    }
  }

  /// Allocates a slice of objects that are copied into the `DroplessArena`, returning a mutable
  /// reference to it. Will panic if passed a zero-sized type.
  ///
  /// Panics:
  ///
  ///  - Zero-sized types
  ///  - Zero-length slices
  #[inline]
  pub fn alloc_slice<T>(&self, slice: &[T]) -> &mut [T]
  where
    T: Copy,
  {
    assert!(!std::mem::needs_drop::<T>());
    assert!(std::mem::size_of::<T>() != 0);
    assert!(!slice.is_empty());

    let mem = self.alloc_raw(Layout::for_value::<[T]>(slice)) as *mut T;

    unsafe {
      mem.copy_from_nonoverlapping(slice.as_ptr(), slice.len());
      std::slice::from_raw_parts_mut(mem, slice.len())
    }
  }
}
