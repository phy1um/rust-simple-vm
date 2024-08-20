use crate::{LinearMemory, Machine, MemoryMappedBuffer};
use std::fmt;
use std::str::FromStr;

#[derive(Debug, Default)]
pub struct Header {
    pub version: u16,
    pub entrypoint: u16,
    pub hwconf: [u8; 4],
    pub section_count: u16,
    reserved: [u8; 12],
}

fn slice_to_u16(data: &[u8], start: usize) -> Result<u16, String> {
    let le: [u8; 2] = data
        .get(start..start + 2)
        .ok_or("not enough data".to_string())
        .and_then(|x| {
            x.try_into()
                .map_err(|e: std::array::TryFromSliceError| e.to_string())
        })?;
    Ok(u16::from_le_bytes(le))
}

fn slice_to_u32(data: &[u8], start: usize) -> Result<u32, String> {
    let le: [u8; 4] = data
        .get(start..start + 4)
        .ok_or("not enough data".to_string())
        .and_then(|x| {
            x.try_into()
                .map_err(|e: std::array::TryFromSliceError| e.to_string())
        })?;
    Ok(u32::from_le_bytes(le))
}

impl Header {
    fn from_bytes(data: &[u8]) -> Result<Self, String> {
        Ok(Self {
            version: slice_to_u16(data, 0)?,
            entrypoint: slice_to_u16(data, 2)?,
            hwconf: data
                .get(4..8)
                .ok_or("not enough data".to_string())
                .and_then(|x| {
                    x.try_into()
                        .map_err(|e: std::array::TryFromSliceError| e.to_string())
                })?,
            section_count: slice_to_u16(data, 8)?,
            reserved: data
                .get(10..22)
                .ok_or("not enough data".to_string())
                .and_then(|x| {
                    x.try_into()
                        .map_err(|e: std::array::TryFromSliceError| e.to_string())
                })?,
        })
    }

    fn to_bytes(&self, out: &mut Vec<u8>) {
        out.extend_from_slice(&self.version.to_le_bytes());
        out.extend_from_slice(&self.entrypoint.to_le_bytes());
        out.extend_from_slice(&self.hwconf);
        out.extend_from_slice(&self.section_count.to_le_bytes());
        out.extend_from_slice(&self.reserved);
    }

    fn byte_size() -> usize {
        22
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
#[repr(u16)]
pub enum SectionMode {
    RO = 0,
    #[default]
    RW = 1,
    Heap = 2,
}

impl TryFrom<u16> for SectionMode {
    type Error = String;
    fn try_from(value: u16) -> Result<Self, Self::Error> {
        match value {
            x if x == SectionMode::RO as u16 => Ok(SectionMode::RO),
            x if x == SectionMode::RW as u16 => Ok(SectionMode::RW),
            x if x == SectionMode::Heap as u16 => Ok(SectionMode::Heap),
            _ => Err(format!("invalid value: {value}")),
        }
    }
}

impl FromStr for SectionMode {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "RO" => Ok(Self::RO),
            "RW" => Ok(Self::RW),
            "Heap" => Ok(Self::Heap),
            _ => Err(format!("unknown mode: {s}")),
        }
    }
}

#[derive(Debug, Default)]
pub struct Section {
    pub size: u16,
    pub mode: SectionMode,
    pub address: u32,
    pub file_offset: u32,
}

impl Section {
    fn byte_size() -> usize {
        12
    }

    fn vec_from_bytes(data: &[u8], n: u16) -> Result<Vec<Self>, String> {
        let mut out = Vec::new();
        for i in 0..n {
            let section_start = i as usize * Self::byte_size();
            let section_end = section_start + Self::byte_size();
            let sub_data = data
                .get(section_start..section_end)
                .ok_or(format!("no data for section {i}"))?;
            let s = Self::from_bytes(sub_data)?;
            out.push(s);
        }
        Ok(out)
    }

    fn from_bytes(data: &[u8]) -> Result<Self, String> {
        Ok(Self {
            size: slice_to_u16(data, 0)?,
            mode: SectionMode::try_from(slice_to_u16(data, 2)?)?,
            address: slice_to_u32(data, 4)?,
            file_offset: slice_to_u32(data, 8)?,
        })
    }

    fn to_bytes(&self, out: &mut Vec<u8>) {
        out.extend_from_slice(&self.size.to_le_bytes());
        out.extend_from_slice(&(self.mode as u16).to_le_bytes());
        out.extend_from_slice(&self.address.to_le_bytes());
        out.extend_from_slice(&self.file_offset.to_le_bytes());
    }
}

#[derive(Debug, Default)]
pub struct BinaryFile {
    pub version: u16,
    pub entrypoint: u16,
    pub sections: Vec<Section>,
    pub data: Vec<u8>,
}

impl fmt::Display for BinaryFile {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(
            f,
            "version: {}\nentrypoint: {}\n# sections: {}\nheader size: {}",
            self.version,
            self.entrypoint,
            self.sections.len(),
            self.get_header_size(),
        )?;
        for s in &self.sections {
            writeln!(
                f,
                "section: size: {} mode: {:?} offset: {} addr: {}",
                s.size, s.mode, s.file_offset, s.address
            )?;
        }
        Ok(())
    }
}

impl BinaryFile {
    pub fn get_header(&self) -> Header {
        Header {
            version: self.version,
            entrypoint: self.entrypoint,
            section_count: self.sections.len() as u16,
            hwconf: [0; 4],
            reserved: [0; 12],
        }
    }

    pub fn get_header_size(&self) -> usize {
        Header::byte_size() + (self.sections.len() * Section::byte_size())
    }

    pub fn from_bytes(data: &[u8]) -> Result<Self, String> {
        let header = Header::from_bytes(data)?;
        let sections_offset = Header::byte_size();
        let data_offset = sections_offset + (Section::byte_size() * header.section_count as usize);
        let sections = Section::vec_from_bytes(&data[sections_offset..], header.section_count)?;
        let tail_data = data
            .get(data_offset..data.len())
            .ok_or("no tail data".to_string())?;
        Ok(BinaryFile {
            version: header.version,
            entrypoint: header.entrypoint,
            sections,
            data: tail_data.to_vec(),
        })
    }

    pub fn to_bytes(&self, out: &mut Vec<u8>) {
        let header = self.get_header();
        header.to_bytes(out);
        for s in &self.sections {
            s.to_bytes(out);
        }
        out.extend(self.data.clone());
    }

    pub fn load_to_vm(&self, vm: &mut Machine) -> Result<(), String> {
        let header_size = self.get_header_size() as u32;
        for (i, section) in self.sections.iter().enumerate() {
            if section.size == 0 {
                continue;
            } else if section.mode == SectionMode::Heap {
                vm.map(
                    section.address as usize,
                    section.size as usize,
                    Box::new(LinearMemory::new(section.size.into())),
                )?;
            } else if section.file_offset < header_size {
                return Err(format!(
                    "section file offset points to header: {} (header size = {header_size})",
                    section.file_offset
                ));
            } else {
                let file_offset_start = (section.file_offset - header_size) as usize;
                let file_offset_end = file_offset_start + section.size as usize;
                if let Some(sub_data) = self.data.get(file_offset_start..file_offset_end) {
                    let addressable = match section.mode {
                        SectionMode::RO => {
                            Box::new(MemoryMappedBuffer::new_read_only(sub_data.to_vec()))
                        }
                        SectionMode::RW => Box::new(MemoryMappedBuffer::new(sub_data.to_vec())),
                        _ => return Err("invalid mode".to_string()),
                    };
                    vm.map(section.address as usize, section.size as usize, addressable)?;
                } else {
                    return Err(format!(
                        "section {i} [{}, {}] => {}: file read out of bounds ({})",
                        section.file_offset,
                        section.file_offset + section.size as u32,
                        section.address,
                        self.data.len(),
                    ));
                }
            }
        }
        Ok(())
    }
}
