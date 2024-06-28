use crate::memory::{Addressable, MemoryError};

pub struct MemoryMappedBuffer(Vec<u8>);

impl MemoryMappedBuffer {
    pub fn new(buf: Vec<u8>) -> Self {
        Self(buf)
    }
}

impl Addressable for MemoryMappedBuffer {
    fn read(&mut self, addr: u32) -> Result<u8, MemoryError> {
        self.0
            .get(addr as usize)
            .ok_or(MemoryError::OutOfBounds(addr))
            .copied()
    }

    fn write(&mut self, addr: u32, value: u8) -> Result<(), MemoryError> {
        if addr as usize >= self.0.len() {
            Err(MemoryError::OutOfBounds(addr))
        } else {
            self.0[addr as usize] = value;
            Ok(())
        }
    }

    fn zero_all(&mut self) -> Result<(), MemoryError> {
        self.zero(0, self.0.len() as u32)
    }
}
