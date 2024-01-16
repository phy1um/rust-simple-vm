
use proc_macro::TokenStream;
use quote::quote;
use syn;


#[proc_macro_derive(VmInstruction, attributes(opcode))]
pub fn generate_vm_instruction_impl(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();
    impl_opcode_struct(&ast)     
}

fn impl_opcode_struct(ast: &syn::ItemEnum) -> TokenStream {
    let field_names: Vec<_> = ast.variants.iter().map(|x| &x.ident).collect();
    let field_values = ast.variants.iter().map(|x| {
        for attr in x.attrs.iter() {
            if attr.path().is_ident("opcode") {
                let value: syn::LitInt = attr.parse_args().unwrap();
                return value;
            }
        }
        syn::parse(quote!{0}.into()).unwrap()
    });
    quote!{
        #[repr(u8)]
        #[derive(Debug)]
        pub enum OpCode {
            #(#field_names = #field_values,)*
        }

        impl FromStr for OpCode {
            type Err = String;

            fn from_str(s: &str) -> Result<Self, Self::Err> {
                match s {
                    #(stringify!(#field_names) => Ok(Self::#field_names),)*
                    _ => Err(format!("unknown opcode {}", s)),
                }
            }
        }

        impl TryFrom<u8> for OpCode {
            type Error = String;
            fn try_from(b: u8) -> Result<Self, Self::Error> {
                match b {
                    #(x if x == Self::#field_names as u8 => Ok(Self::#field_names),)*
                    _ => Err(format!("unknown opcode {:X}", b)),
                }
            }
        }

    }.into()
}


