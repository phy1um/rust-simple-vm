use std::env;
use std::fs::File;
use std::io;
use std::io::{BufReader, Read, Write};
use std::path::Path;

use simplevm::pp::macros;
use simplevm::pp::{Chunk, PreProcessor};

use simplevm::binfmt::{BinaryFile, Section};

mod args;

fn main() -> Result<(), String> {
    // ./asm file.asm
    let args_vec: Vec<_> = env::args().collect();
    let args = args::process_cli(&args_vec).map_err(|x| x.to_string())?;
    if !args.validate() {
        println!("{}", args.usage());
        return Ok(());
    }

    let file = File::open(Path::new(&args.input_file.unwrap()))
        .map_err(|x| format!("failed to open: {}", x))?;
    let mut output: Vec<u8> = Vec::new();

    let mut processor = PreProcessor::default();
    macros::setup_std_macros(&mut processor);
    let mut reader = BufReader::new(file);
    let mut content = String::new();
    reader
        .read_to_string(&mut content)
        .map_err(|_| "failed to read file".to_string())?;
    processor
        .handle(&content)
        .map_err(|_| "failed to resolve".to_string())?;
    let sections = processor
        .get_unresolved_instructions()
        .map_err(|e| format!("part resolve: {e}"))?;
    processor
        .define_labels(&sections)
        .map_err(|e| format!("define labels: {e}"))?;
    if args.preprocess_only {
        todo!("wip rebuilding");
    } else {
        let mut bin = BinaryFile {
            entrypoint: 0,
            version: 99,
            ..BinaryFile::default()
        };

        for (_name, section) in sections {
            let mut section_data: Vec<u8> = Vec::new();
            for chunk in section.chunks {
                match chunk {
                    Chunk::Raw(v) => section_data.extend(v),
                    Chunk::Lines(ls) => {
                        for line in ls {
                            let ins_res = line
                                .resolve(&processor.labels)
                                .map_err(|e| format!("resolve: {e:?}"))?;
                            if let Some(ins) = ins_res {
                                section_data.extend_from_slice(&ins.encode_u16().to_le_bytes());
                            }
                        }
                    }
                }
            }
            bin.sections.push(Section {
                size: section_data.len() as u16,
                mode: section.mode,
                address: section.offset,
                file_offset: bin.data.len() as u32,
            });
            bin.data.extend(section_data);
        }
        bin.to_bytes(&mut output);
    }
    let mut stdout = io::stdout().lock();
    stdout.write_all(&output).map_err(|x| format!("{}", x))?;
    Ok(())
}
