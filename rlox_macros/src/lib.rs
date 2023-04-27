use syn::parse_macro_input;

mod testing;

extern crate proc_macro;

#[proc_macro]
pub fn declare_output_tests(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
  let all_test_files = parse_macro_input!(input as testing::AllTestFiles);
  proc_macro::TokenStream::from(all_test_files.to_test_impl())
}
