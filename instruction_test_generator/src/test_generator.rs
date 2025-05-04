use std::ffi::OsString;

use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::LitStr;

fn is_json_file(path: &OsString) -> bool {
    path.to_str().unwrap().contains(".json")
}

pub fn test_generator_impl(input: TokenStream) -> TokenStream {
    let input: LitStr = syn::parse2(input).unwrap();
    let path = input.value();
    let mut tokens = Vec::new();
    for entry in std::fs::read_dir(path)
        .unwrap()
        .map(|e| e.unwrap())
        .filter(|e| e.path().is_file())
        .filter(|e| is_json_file(&e.file_name()))
    {
        let path = entry.path();
        let file_name = path.file_stem().unwrap().to_string_lossy().replace(".", "_");
        let test_name = format_ident!("test_{}", file_name);
        let path_str = path.to_string_lossy();
        tokens.push(quote! {
            #[test]
            fn #test_name() {
                run_instruction_test_case(#path_str)
            }
        });
    }
    quote! {
        #(#tokens)*
    }
}
