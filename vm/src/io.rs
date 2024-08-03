use crate::memory::{Addressable, MemoryError};

pub struct MemoryMappedBuffer {
    buffer: Vec<u8>,
    read_only: bool,
}

impl MemoryMappedBuffer {
    pub fn new(buffer: Vec<u8>) -> Self {
        Self {
            buffer,
            read_only: false,
        }
    }

    pub fn new_read_only(buffer: Vec<u8>) -> Self {
        Self {
            buffer,
            read_only: true,
        }
    }
}

impl Addressable for MemoryMappedBuffer {
    fn read(&mut self, addr: u32) -> Result<u8, MemoryError> {
        self.buffer
            .get(addr as usize)
            .ok_or(MemoryError::OutOfBounds(addr))
            .copied()
    }

    fn write(&mut self, addr: u32, value: u8) -> Result<(), MemoryError> {
        if self.read_only {
            Err(MemoryError::ReadOnly)
        } else if addr as usize >= self.buffer.len() {
            Err(MemoryError::OutOfBounds(addr))
        } else {
            self.buffer[addr as usize] = value;
            Ok(())
        }
    }

    fn zero_all(&mut self) -> Result<(), MemoryError> {
        self.zero(0, self.buffer.len() as u32)
    }
}
