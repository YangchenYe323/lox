mod heap;
mod object_id;
mod print;
mod time;

pub use heap::{builtin_heap, Heap};
pub use object_id::{builtin_get_object_id, GetObjectId};
pub use print::{builtin_print, Print};
pub use time::{builtin_time, Time};
