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
        x.path
            .segments
            .iter()
            .map(|x| x.ident.to_string())
            .collect()
    } else {
        panic!("don't know how to handle this type")
    }
}

fn variant_opcode_value(v: &syn::Variant) -> u8 {
    for attr in v.attrs.iter() {
        if attr.path().is_ident("opcode") {
            let value: syn::LitInt = attr.parse_args().unwrap();
            return value.base10_parse().unwrap();
        }
    }
    panic!("instruction ??? has no opcode");
}

fn impl_opcode_struct(ast: &syn::ItemEnum) -> TokenStream {
    let mut field_u16_encodings: Vec<_> = Vec::new();
    let mut field_u16_decodings: Vec<_> = Vec::new();
    let mut field_to_string: Vec<_> = Vec::new();
    let mut field_from_str: Vec<_> = Vec::new();
    for x in ast.variants.iter() {
        let name = &x.ident;
        let opcode_value = variant_opcode_value(&x);
        if let syn::Fields::Unit = &x.fields {
            field_u16_encodings.push(quote! {
                Self::#name => #opcode_value as u16
            });
            field_u16_decodings.push(quote! {
                #opcode_value => Ok(Self::#name)
            });
            field_to_string.push(quote! {
                Self::#name => write!(f, stringify!(#name))
            });
            field_from_str.push(quote! {
                stringify!(#name) => {
                    assert_length(&parts, 1)?;
                    Ok(Self::#name)
                }
            });
            continue;
        }
        if let syn::Fields::Unnamed(fields) = &x.fields {
            let types: Vec<_> = fields
                .unnamed
                .iter()
                .map(|x| get_type_name(&x.ty))
                .collect();
            let types_str: Vec<_> = types.iter().map(AsRef::as_ref).collect();
            match &types_str[..] {
                ["u8"] => {
                    field_u16_encodings.push(quote! {
                        Self::#name(u) => #opcode_value as u16 | ((*u as u16) << 8)
                    });
                    field_u16_decodings.push(quote! {
                        #opcode_value => Ok(Self::#name(((ins&0xff00)>>8) as u8))
                    });
                    field_to_string.push(quote! {
                        Self::#name(b) => write!(f, "{} {}", stringify!(#name), b)
                    });
                    field_from_str.push(quote! {
                        stringify!(#name) => {
                            assert_length(&parts, 2)?;
                            Ok(Self::#name(Self::parse_numeric(parts[1])?))
                        }
                    });
                }
                ["i8"] => {
                    field_u16_encodings.push(quote! {
                        Self::#name(u) => {
                            let raw_value = u.to_le_bytes();
                            #opcode_value as u16 | ((raw_value[0] as u16) << 8)
                        }
                    });
                    field_u16_decodings.push(quote! {
                        #opcode_value => {
                            let raw_value = i8::from_le_bytes([((ins&0xff00)>>8) as u8]);
                            Ok(Self::#name(raw_value))
                        }
                    });
                    field_to_string.push(quote! {
                        Self::#name(b) => write!(f, "{} {}", stringify!(#name), b)
                    });
                    field_from_str.push(quote! {
                        stringify!(#name) => {
                            assert_length(&parts, 2)?;
                            Ok(Self::#name(Self::parse_numeric_signed(parts[1])?))
                        }
                    });
                }
                ["Register"] => {
                    field_u16_encodings.push(quote! {
                            Self::#name(r) => #opcode_value as u16 | (((*r as u16)&0xf) << 8)
                    });
                    field_u16_decodings.push(quote! {
                        #opcode_value => {
                            let reg = (ins&0xf00)>>8;
                            Register::from_u8(reg as u8)
                                .ok_or(format!("unknown register 0x{:X}", reg))
                                .map(|r| Self::#name(r))
                        }
                    });
                    field_to_string.push(quote! {
                        Self::#name(r) => write!(f, "{} {}", stringify!(#name), r)
                    });
                    field_from_str.push(quote! {
                        stringify!(#name) => {
                            assert_length(&parts, 2)?;
                            Ok(Self::#name(Register::from_str(parts[1])?))
                        }
                    });
                }
                ["Register", "Register"] => {
                    field_u16_encodings.push(quote! {
                            Self::#name(r1, r2) => #opcode_value as u16 | (((*r1 as u16)&0xf) << 8)
                                | (((*r2 as u16)&0xf) << 12)
                    });
                    field_u16_decodings.push(quote! {
                            #opcode_value => {
                                let reg1_value = (ins&0xf00)>>8;
                                let reg2_value = (ins&0xf000)>>12;
                                let reg1 = Register::from_u8(reg1_value as u8)
                                    .ok_or(format!("unknown register 0x{:X}", reg1_value))
                                    .unwrap();
                                let reg2 = Register::from_u8(reg2_value as u8)
                                    .ok_or(format!("unknown register 0x{:X}", reg2_value))
                                    .unwrap();
                                Ok(Self::#name(reg1, reg2))
                            }
                    });
                    field_to_string.push(quote! {
                        Self::#name(r1, r2) => write!(f, "{} {} {}", stringify!(#name), r1, r2)
                    });
                    field_from_str.push(quote! {
                        stringify!(#name) => {
                            assert_length(&parts, 3)?;
                            Ok(Self::#name(
                                    Register::from_str(parts[1])?,
                                    Register::from_str(parts[2])?))
                        }
                    });
                }
                _ => panic!("invalid types: {:?}", types),
            }
        } else {
            panic!("fields must be unnamed in variant: {}", name)
        }
    }

    quote! {
        impl Instruction {
            pub fn encode_u16(&self) -> u16 {
                match self {
                    #(#field_u16_encodings,)*
                }
            }

            fn parse_numeric(s: &str) -> Result<u8, String> {
                if s.len() == 0 {
                    return Err("string has no length".to_string());
                }
                let fst = s.chars().nth(0).unwrap();
                let (num, radix) = match fst {
                    '$' => (&s[1..], 16),
                    '%' => (&s[1..], 2),
                    _ => (s, 10)
                };
                u8::from_str_radix(num, radix).map_err(|x| format!("{}", x))
            }

            fn parse_numeric_signed(s: &str) -> Result<i8, String> {
                if s.len() == 0 {
                    return Err("string has no length".to_string());
                }
                let fst = s.chars().nth(0).unwrap();
                let (num, radix) = match fst {
                    '$' => (&s[1..], 16),
                    '%' => (&s[1..], 2),
                    _ => (s, 10)
                };
                i8::from_str_radix(num, radix).map_err(|x| format!("{}", x))
            }
        }

        impl TryFrom<u16> for Instruction {
            type Error = String;
            fn try_from(ins: u16) -> Result<Self, Self::Error> {
                let op = (ins & 0xff) as u8;
                match op {
                    #(#field_u16_decodings,)*
                    _ => Err(format!("unknown opcode {:X}", op))
                }
            }
        }

        impl fmt::Display for Instruction {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                match self {
                    #(#field_to_string,)*
                }
            }
        }

        fn assert_length(parts: &Vec<&str>, n: usize) -> Result<(), String> {
            if parts.len() == n {
                Ok(())
            } else {
                Err(format!("expected {} got {}", parts.len(), n))
            }
        }

        impl FromStr for Instruction {
            type Err = InstructionParseError;
            fn from_str(s: &str) -> Result<Self, Self::Err> {
                let parts: Vec<_> = s.split(" ").filter(|x| x.len() > 0).collect();
                if parts.len() == 0 {
                    return Err(Self::Err::NoContent);
                }
                match parts[0] {
                    #(#field_from_str,)*
                    _ => Err(Self::Err::Fail(format!("unknown op {}", parts[0]))),
                }
            }
        }
    }
    .into()
}
