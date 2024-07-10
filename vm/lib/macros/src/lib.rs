use proc_macro::TokenStream;
use quote::{format_ident, quote};

#[proc_macro_derive(StringyEnum)]
pub fn generate_stringy_enum_impls(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();
    match impl_stringy_enum(&ast) {
        Ok(ts) => ts.into(),
        Err(e) => panic!("{}", e),
    }
}

fn impl_stringy_enum(ast: &syn::ItemEnum) -> Result<proc_macro2::TokenStream, String> {
    let mut to_string = quote! {};
    let mut from_string = quote! {};
    let type_name = &ast.ident;
    for x in ast.variants.iter() {
        let name = &x.ident;
        if let syn::Fields::Unit = &x.fields {
            to_string.extend(quote!(Self::#name => write!(f, "{}", stringify!(#name)),));
            from_string.extend(quote!(stringify!(#name) => Ok(Self::#name),));
        } else {
            return Err(format!(
                "all enum variants must be unit. got {}",
                stringify!(#name)
            ));
        }
    }
    Ok(quote! {
        impl FromStr for #type_name {
            type Err = String;
            fn from_str(s: &str) -> Result<Self, Self::Err> {
                match s {
                    #from_string
                    _ => Err(format!("unknown {}: {}", stringify!(#type_name), s)),
                }
            }
        }
        impl fmt::Display for #type_name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                match self {
                    #to_string
                    _ => write!(f, "invalid {} value: {:?}", stringify!(#type_name), self),
                }
            }
        }
    })
}

#[proc_macro_derive(VmInstruction, attributes(opcode))]
pub fn generate_vm_instruction_impl(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();
    match impl_opcode_struct(&ast) {
        Ok(ts) => ts.into(),
        Err(e) => panic!("{}", e),
    }
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

fn get_arg_name(i: usize) -> Result<proc_macro2::Ident, String> {
    Ok(format_ident!(
        "{}",
        match i {
            0 => Ok("a0"),
            1 => Ok("a1"),
            2 => Ok("a2"),
            _ => Err(format!("invalid argument index: {}", i)),
        }?
    ))
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

fn impl_opcode_struct(ast: &syn::ItemEnum) -> Result<proc_macro2::TokenStream, String> {
    let mut field_u16_encodings: proc_macro2::TokenStream = quote!();
    let mut field_u16_decodings: proc_macro2::TokenStream = quote!();
    let mut field_to_string: proc_macro2::TokenStream = quote!();
    let mut field_from_str: proc_macro2::TokenStream = quote!();
    for x in ast.variants.iter() {
        let name = &x.ident;
        let opcode_value = variant_opcode_value(x);
        if let syn::Fields::Unit = &x.fields {
            field_u16_encodings.extend(quote! {
                Self::#name => #opcode_value as u16,
            });
            field_u16_decodings.extend(quote! {
                #opcode_value => Ok(Self::#name),
            });
            field_to_string.extend(quote! {
                Self::#name => write!(f, stringify!(#name)),
            });
            field_from_str.extend(quote! {
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
            let mut part_encoders: proc_macro2::TokenStream = quote!();
            let mut part_decoders: proc_macro2::TokenStream = quote!();
            let mut part_stringers: proc_macro2::TokenStream = quote!();
            for (i, type_name) in types.iter().enumerate() {
                match (type_name.as_str(), i) {
                    ("Register", 0) => {
                        part_encoders.extend(quote!(op_parts[0] = a0.as_mask_first();));
                        part_decoders.extend(
                            quote!(let a0 = Register::from_instruction_first(ins).ok_or("unknown register")?;)
                        );

                        part_stringers.extend(
                            quote!(let a0 = Register::from_str(parts[1]).map_err(|x| Self::Err::Fail(x))?;)
                        );
                    }
                    ("Register", 1) => {
                        part_encoders.extend(quote!(op_parts[1] = a1.as_mask_second();));
                        part_decoders.extend(
                            quote!(let a1 = Register::from_instruction_second(ins).ok_or("unknown register")?;)
                        );
                        part_stringers.extend(
                            quote!(let a1 = Register::from_str(parts[2]).map_err(|x| Self::Err::Fail(x))?;)
                        );
                    }
                    ("Register", 2) => {
                        part_encoders.extend(quote!(op_parts[2] = a2.as_mask_third();));
                        part_decoders.extend(
                            quote!(let a2 = Register::from_instruction_third(ins).ok_or("unknown register")?;)
                        );
                        part_stringers.extend(
                            quote!(let a2 = Register::from_str(parts[3]).map_err(|x| Self::Err::Fail(x))?;)
                        );
                    }
                    ("Literal7Bit", i) => {
                        let argname = get_arg_name(i)?;
                        let part_index = i + 1;
                        part_encoders.extend(quote!{
                            op_parts[#i] = ((#argname.value&0xf)as u16) | (((#argname.value as u16)&0x70) << 5);
                        });
                        part_decoders.extend(quote!{
                            let #argname = Literal7Bit::new_checked(((ins&0xf) as u8) | (((ins&0xe00)>>5) as u8))?;
                        });
                        part_stringers.extend(quote!{
                            let (part, radix) = Instruction::pre_handle_number(&parts[#part_index]).map_err(|x| Self::Err::Fail(x))?;
                            let #argname = Literal7Bit::from_str_radix(part, radix).map_err(|x| Self::Err::Fail(x))?;
                        });
                    }
                    ("Literal10Bit", i) => {
                        let argname = get_arg_name(i)?;
                        let part_index = i + 1;
                        part_encoders.extend(quote!{
                            op_parts[#i] = ((#argname.value&0xf) as u16) | (((#argname.value&0x70) as u16) << 5)
                                | ((#argname.value&0x0380 as u16) << 5);
                        });
                        part_decoders.extend(quote!{
                            let #argname = Literal10Bit::new_checked(((ins&0xf) as u16) | (((ins&0xe00)>>5) as u16) | (((ins&0x7000)>>5) as u16))?;
                        });
                        part_stringers.extend(quote!{
                            let (part, radix) = Instruction::pre_handle_number(&parts[#part_index]).map_err(|x| Self::Err::Fail(x))?;
                            let #argname = Literal10Bit::from_str_radix(part, radix).map_err(|x| Self::Err::Fail(x))?;
                        });
                    }
                    ("Literal12Bit", i) => {
                        let argname = get_arg_name(i)?;
                        let part_index = i + 1;
                        part_encoders.extend(quote! {
                            op_parts[#i] = #argname.value&0xfff;
                        });
                        part_decoders.extend(quote! {
                            let #argname = Literal12Bit::new_checked(ins&0xfff)?;
                        });
                        part_stringers.extend(quote!{
                            let (part, radix) = Instruction::pre_handle_number(&parts[#part_index]).map_err(|x| Self::Err::Fail(x))?;
                            let #argname = Literal12Bit::from_str_radix(part, radix).map_err(|x| Self::Err::Fail(x))?;
                        });
                    }
                    ("Nibble", i) => {
                        let argname = get_arg_name(i)?;
                        let part_index = i + 1;
                        part_encoders.extend(quote! {
                            op_parts[#i] = (#argname.value as u16)&0xf;
                        });
                        part_decoders.extend(quote! {
                            let #argname = Nibble::new_checked((ins&0xf) as u8)?;
                        });
                        part_stringers.extend(quote!{
                            let (part, radix) = Instruction::pre_handle_number(&parts[#part_index]).map_err(|x| Self::Err::Fail(x))?;
                            let #argname = Nibble::from_str_radix(part, radix).map_err(|x| Self::Err::Fail(x))?;
                        });
                    }
                    ("TestOp", i) => {
                        let argname = get_arg_name(i)?;
                        let part_index = i + 1;
                        part_encoders.extend(quote! {
                            op_parts[#i] = (*#argname as u16)&0xf;
                        });
                        part_decoders.extend(quote! {
                            let #argname = TestOp::try_from(ins&0xf)?;
                        });
                        part_stringers.extend(quote! {
                            let #argname = TestOp::from_str(&parts[#part_index]).map_err(|x| {
                                Self::Err::Fail(x)
                            })?;
                        });
                    }
                    ("StackOp", i) => {
                        let argname = get_arg_name(i)?;
                        let part_index = i + 1;
                        part_encoders.extend(quote! {
                            op_parts[#i] = (*#argname as u16)&0xf;
                        });
                        part_decoders.extend(quote! {
                            let #argname = StackOp::try_from(ins&0xf)?;
                        });
                        part_stringers.extend(quote! {
                            let #argname = StackOp::from_str(&parts[#part_index]).map_err(|x| {
                                Self::Err::Fail(x)
                            })?;
                        });
                    }
                    (_, _) => {
                        panic!("invalid type {} at place {}", type_name, i)
                    }
                }
            }
            let name_matcher = match types.len() {
                0 => Ok(quote!(Self::#name)),
                1 => Ok(quote!(Self::#name(a0))),
                2 => Ok(quote!(Self::#name(a0, a1))),
                3 => Ok(quote!(Self::#name(a0, a1, a2))),
                x => Err(format!("opcodes may not have {} fields", x)),
            }?;

            // immediate instructions get an extra step
            if name == "Imm" {
                field_u16_encodings.extend(quote! {
                    #name_matcher => {
                        let mut op_parts: [u16;3] = [0,0,0];
                        #part_encoders
                        0x8000 | op_parts[0] | op_parts[1] | op_parts[2]
                    }
                });
            } else {
                let opcode_mask = ((opcode_value as u16) & 0x1f) << 4;
                field_u16_encodings.extend(quote! {
                    #name_matcher => {
                        let mut op_parts: [u16;3] = [0,0,0];
                        #part_encoders
                        op_parts[0] | op_parts[1] | op_parts[2] | #opcode_mask
                    }
                });
                field_u16_decodings.extend(quote! {
                    #opcode_value => {
                        #part_decoders
                        Ok(#name_matcher)
                    }
                });
            }

            let types_len = types.len();
            field_from_str.extend(quote! {
                stringify!(#name) => {
                    assert_length(&parts, #types_len + 1).map_err(|x| Self::Err::Fail(x))?;
                    #part_stringers
                    Ok(#name_matcher)
                }
            });

            match types.len() {
                0 => field_to_string.extend(quote! {
                    Self::#name => {
                        write!(f, "{}", stringify!(#name))
                    }
                }),
                1 => field_to_string.extend(quote! {
                    Self::#name(a0) => {
                        write!(f, "{} {}", stringify!(#name), a0)
                    }
                }),
                2 => field_to_string.extend(quote! {
                    Self::#name(a0, a1) => {
                        write!(f, "{} {} {}", stringify!(#name), a0, a1)
                    }
                }),
                3 => field_to_string.extend(quote! {
                    Self::#name(a0, a1, a2) => {
                        write!(f, "{} {} {} {}", stringify!(#name), a0, a1, a2)
                    }
                }),
                x => panic!("{}", format!("types must be 0-3, got {}", x)),
            };
        } else {
            panic!("fields must be unnamed in variant: {}", name)
        }
    }

    Ok(quote! {
        impl Instruction {
            pub fn encode_u16(&self) -> u16 {
                match self {
                    #field_u16_encodings
                    _ => 0,
                }
            }

            pub fn pre_handle_number(s: &str) -> Result<(&str, u32), String> {
                if s.len() == 0 {
                    return Err("string has no length".to_string());
                }
                let fst = s.chars().nth(0).unwrap();
                Ok(match fst {
                    '$' => (&s[1..], 16),
                    '%' => (&s[1..], 2),
                    _ => (s, 10)
                })
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

            fn result_join<A,B,E>(left: Result<A, E>, right: Result<B, E>) -> Result<Result<A,B>, E> {
                match left {
                    Ok(a) => Ok(Ok(a)),
                    Err(_) => match right {
                        Ok(b) => Ok(Err(b)),
                        Err(e2) => Err(e2),
                    }
                }
            }
        }

        impl TryFrom<u16> for Instruction {
            type Error = String;
            fn try_from(ins: u16) -> Result<Self, Self::Error> {
                if (ins & 0x8000) == 0 {
                    // match other instructions
                    let op = ((ins & 0x1f0) >> 4) as u8;
                    match op {
                        #field_u16_decodings
                        _ => Err(format!("unknown opcode {:X}", op))
                    }
                } else {
                    // match immediate
                    let register_bits = ((ins & 0x7000) >> 12) as u8;
                    let register = Register::from_u8(register_bits).ok_or("invalid register")?;
                    let lit = Literal12Bit::new_checked(ins&0xfff)?;
                    Ok(Instruction::Imm(register, lit))
                }
            }
        }

        impl fmt::Display for Instruction {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                match self {
                    #field_to_string
                    _ => write!(f, "placeholder")
                }
            }
        }

        fn assert_length(parts: &[&str], n: usize) -> Result<(), String> {
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
                    #field_from_str
                    _ => Err(Self::Err::Fail(format!("unknown op {}", parts[0]))),
                }
            }
        }
    })
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn arg_name() -> Result<(), String> {
        assert!(get_arg_name(0)?.to_string() == "a0".to_string());
        assert!(get_arg_name(1)?.to_string() == "a1".to_string());
        assert!(get_arg_name(2)?.to_string() == "a2".to_string());
        assert!(get_arg_name(3).is_err());
        assert!(get_arg_name(99).is_err());
        Ok(())
    }
}
