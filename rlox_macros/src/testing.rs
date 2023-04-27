use proc_macro2::{Ident, Literal, Span};
use quote::quote;
use syn::parse::Parse;

pub struct TestFile {
  path: syn::Path,
}

impl Parse for TestFile {
  fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
    let path = syn::Path::parse(input)?;
    Ok(Self { path })
  }
}

impl TestFile {
  pub fn into_test_fn_impl(self) -> proc_macro2::TokenStream {
    let components: Vec<_> = self
      .path
      .segments
      .into_iter()
      .map(|seg| seg.ident.to_string())
      .collect();
    let test_fn_name = components.join("_");
    let test_file_path = components.join("/");
    let test_fn_ident = Ident::new(&test_fn_name, Span::call_site());
    let test_file_lit = Literal::string(&test_file_path);

    quote! {
      #[test]
      fn #test_fn_ident() {
        test_source(#test_file_lit);
      }
    }
  }
}

pub struct AllTestFiles {
  files: Vec<TestFile>,
}

impl Parse for AllTestFiles {
  fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
    let files = input
      .parse_terminated(TestFile::parse, syn::Token![,])?
      .into_iter()
      .collect();
    Ok(Self { files })
  }
}

impl AllTestFiles {
  pub fn into_test_impl(self) -> proc_macro2::TokenStream {
    let test_impls = self.files.into_iter().map(TestFile::into_test_fn_impl);
    quote! {
      #(#test_impls)*
    }
  }
}
