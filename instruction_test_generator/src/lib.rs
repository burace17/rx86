use proc_macro::TokenStream;
use syn::parse_macro_input;

mod test_generator;

#[proc_macro]
pub fn generate_instruction_tests(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input);
    test_generator::test_generator_impl(input).into()
}
