//! This crate contains utility procedural macros, which are meant to be used in other rlox
//! crates and hence couples tightly with the specific items available where the macros are invoked.
//! They are NOT general purpose.

use syn::parse_macro_input;

mod testing;

extern crate proc_macro;

#[proc_macro]
pub fn declare_output_tests(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
  let all_test_files = parse_macro_input!(input as testing::AllTestFiles);
  proc_macro::TokenStream::from(all_test_files.into_test_impl())
}
