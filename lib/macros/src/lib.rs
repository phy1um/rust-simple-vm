
use proc_macro::TokenStream;
use quote::quote;
use syn;


#[proc_macro_derive(VmInstruction, attributes(opcode))]
pub fn generate_vm_instruction_impl(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();
    impl_opcode_struct(&ast)     
}

fn get_type_name(ty: &syn::Type) -> String {
    if let syn::Type::Path(x) = ty {
        x.path.segments.iter().map(|x| x.ident.to_string()).collect()
    } else {
        panic!("don't know how to handle this type")
    }
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

    let field_u16_encodings: Vec<_> = ast.variants.iter().map(|x| {
        let name = &x.ident;
        if let syn::Fields::Unit = &x.fields {
            return quote!{
                Self::#name => OpCode::#name as u16
            }
        }
        if let syn::Fields::Unnamed(fields) = &x.fields {
            let types: Vec<_> = fields.unnamed.iter().map(|x| get_type_name(&x.ty)).collect();
            let types_str: Vec<_> = types.iter().map(AsRef::as_ref).collect();
            match &types_str[..] {
                ["u8"] => quote!{
                            Self::#name(u) => OpCode::#name as u16 | ((*u as u16) << 8)
                },
                ["Register"] => quote!{
                            Self::#name(r) => OpCode::#name as u16 | ((*r as u16)&0xf << 8)
                },
                ["Register", "Register"] => quote!{
                            Self::#name(r1, r2) => OpCode::#name as u16 | ((*r1 as u16)&0xf << 8) 
                                | ((*r2 as u16)&0xf << 12)
                },
                _ => panic!("invalid types: {:?}", types)
            }
        } else {
            panic!("fields must be unnamed in variant: {}", name)
        }
    }).collect();

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

        impl Instruction {
            pub fn encode_u16(&self) -> u16 {
                match self {
                    #(#field_u16_encodings,)*
                }
            }
        }
    }.into()
}


