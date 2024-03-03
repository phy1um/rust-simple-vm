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
                    assert_length(&parts, 1).map_err(|x| Self::Err::Fail(x))?;
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
                ["Register", "u16"] => {
                    field_u16_encodings.push(quote!{
                        Self::#name(r, n) => {
                            r.as_mask_first() | (n&0xfff)
                        }
                    });
                    field_to_string.push(quote!{
                        Self::#name(r, l) => write!(f, "{} {} {}", stringify!(#name), r, l)
                    });
                    field_from_str.push(quote! {
                        stringify!(#name) => {
                            assert_length(&parts, 3).map_err(|x| Self::Err::Fail(x))?;
                            Ok(Self::#name(
                                    Register::from_str(parts[1])
                                           .map_err(|x| Self::Err::Fail(x))?,
                                    Instruction::parse_numeric(parts[2]).unwrap()))
                        }
                    });
                },
                ["Register", "Literal7Bit"] => {
                    field_u16_encodings.push(quote! {
                            Self::#name(r, l) => {
                                (#opcode_value as u16) << 4 
                                    | r.as_mask_first()
                                    | l.as_mask()
                                    | 0x8000
                            }
                    });
                    field_u16_decodings.push(quote! {
                        #opcode_value => {
                            let r = Register::from_instruction_first(ins)
                                .ok_or("unknown register")?;
                            let lit = Literal7Bit::from_instruction(ins);
                            Ok(Self::#name(r, lit))
                        }
                    });
                    field_to_string.push(quote! {
                        Self::#name(r, l) => write!(f, "{} {} {}", stringify!(#name), r, l)
                    });
                    field_from_str.push(quote! {
                        stringify!(#name) => {
                            assert_length(&parts, 3).map_err(|x| Self::Err::Fail(x))?;
                            Ok(Self::#name(
                                    Register::from_str(parts[1])
                                           .map_err(|x| Self::Err::Fail(x))?,
                                    Literal7Bit::from_str(parts[2])))
                        }
                    });
                }
                ["Register", "Register", "Register"] => {
                    field_u16_encodings.push(quote! {
                            Self::#name(r1, r2, r3) => {
                                (#opcode_value as u16) << 4 
                                    | r1.as_mask_first()
                                    | r2.as_mask_second()
                                    | r3.as_mask_third()
                                    | 0x8000
                            }                    
                    });
                    field_u16_decodings.push(quote! {
                            #opcode_value => {
                                let reg1 = Register::from_instruction_first(ins)
                                    .ok_or("unknown register")
                                    .unwrap();
                                let reg2 = Register::from_instruction_second(ins)
                                    .ok_or("unknown register")
                                    .unwrap();
                                let reg3 = Register::from_instruction_third(ins)
                                    .ok_or("unknown register")
                                    .unwrap();
                                Ok(Self::#name(reg1, reg2, reg3))
                            }
                    });
                    field_to_string.push(quote! {
                        Self::#name(r1, r2, r3) => write!(f, "{} {} {} {}", stringify!(#name), r1, r2, r3)
                    });
                    field_from_str.push(quote! {
                        stringify!(#name) => {
                            assert_length(&parts, 4).map_err(|x| Self::Err::Fail(x))?;
                            Ok(Self::#name(
                                    Register::from_str(parts[1]).map_err(|x| Self::Err::Fail(x))?,
                                    Register::from_str(parts[2]).map_err(|x| Self::Err::Fail(x))?,
                                    Register::from_str(parts[3]).map_err(|x| Self::Err::Fail(x))?))
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

            fn parse_numeric(s: &str) -> Result<u16, String> {
                if s.len() == 0 {
                    return Err("string has no length".to_string());
                }
                let fst = s.chars().nth(0).unwrap();
                let (num, radix) = match fst {
                    '$' => (&s[1..], 16),
                    '%' => (&s[1..], 2),
                    _ => (s, 10)
                };
                u16::from_str_radix(num, radix).map_err(|x| format!("{}", x))
            }

            fn parse_numeric_signed(s: &str) -> Result<i16, String> {
                if s.len() == 0 {
                    return Err("string has no length".to_string());
                }
                let fst = s.chars().nth(0).unwrap();
                let (num, radix) = match fst {
                    '$' => (&s[1..], 16),
                    '%' => (&s[1..], 2),
                    _ => (s, 10)
                };
                i16::from_str_radix(num, radix).map_err(|x| format!("{}", x))
            }
        }

        impl TryFrom<u16> for Instruction {
            type Error = String;
            fn try_from(ins: u16) -> Result<Self, Self::Error> {
                if ins & 0x8000 {
                    // match TYPE B 
                    let op = ((ins & 0x1f0) >> 4) as u8;
                    let ins = match op {
                        #(#field_u16_decodings,)*
                        _ => Err(format!("unknown opcode {:X}", op))
                    }
                } else {
                    // match TYPE A
                    let register_bits = ((ins & 0x7000) >> 12) as u8;
                    let register = Register::from_u8(register_bits).ok_or("invalid register")?;
                    Ok(Instruction::Imm(register, ins&0xfff))
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
