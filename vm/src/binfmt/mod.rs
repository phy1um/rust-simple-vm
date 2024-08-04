use crate::{Machine, MemoryMappedBuffer};

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
        let mut out = Self::default();
        out.version = slice_to_u16(data, 0)?;
        out.entrypoint = slice_to_u16(data, 2)?;
        out.hwconf = data
            .get(4..8)
            .ok_or("not enough data".to_string())
            .and_then(|x| {
                x.try_into()
                    .map_err(|e: std::array::TryFromSliceError| e.to_string())
            })?;
        out.section_count = slice_to_u16(data, 8)?;
        out.reserved = data
            .get(10..22)
            .ok_or("not enough data".to_string())
            .and_then(|x| {
                x.try_into()
                    .map_err(|e: std::array::TryFromSliceError| e.to_string())
            })?;
        Ok(out)
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

#[derive(Debug, Default)]
pub struct Section {
    pub size: u16,
    pub mode: u16,
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
        let mut out = Self::default();
        out.size = slice_to_u16(data, 0)?;
        out.mode = slice_to_u16(data, 2)?;
        out.address = slice_to_u32(data, 4)?;
        out.file_offset = slice_to_u32(data, 8)?;
        Ok(out)
    }

    fn to_bytes(&self, out: &mut Vec<u8>) {
        out.extend_from_slice(&self.size.to_le_bytes());
        out.extend_from_slice(&self.mode.to_le_bytes());
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
        Header::byte_size() + (self.sections.len()*Section::byte_size())
    }

    pub fn from_bytes(data: &[u8]) -> Result<Self, String> {
        let header = Header::from_bytes(data)?;
        let sections_offset = Header::byte_size();
        let data_offset = sections_offset + (Section::byte_size() * header.section_count as usize);
        let sections = Section::vec_from_bytes(&data[sections_offset..], header.section_count)?;
        let tail_data = data
            .get(data_offset..data.len())
            .ok_or(format!("no tail data"))?;
        Ok(BinaryFile {
            version: header.version,
            entrypoint: header.entrypoint,
            sections: sections,
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
        let header_size = self.get_header_size();
        for (i, section) in self.sections.iter().enumerate() {
            let file_offset_start = section.file_offset as usize - header_size;
            let file_offset_end = file_offset_start + section.size as usize;
            if let Some(sub_data) = self.data.get(file_offset_start..file_offset_end) {
                let addressable = if section.mode == 1 {
                    MemoryMappedBuffer::new_read_only(sub_data.to_vec())
                } else {
                    MemoryMappedBuffer::new(sub_data.to_vec())
                };
                vm.map(
                    section.address as usize,
                    section.size as usize,
                    Box::new(addressable),
                )?;
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
        Ok(())
    }
}
