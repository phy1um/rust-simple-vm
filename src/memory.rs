use std::cell::RefCell;

pub trait Addressable {
    fn read(&mut self, addr: u32) -> Option<u8>;
    fn write(&mut self, addr: u32, value: u8) -> bool;
    fn zero_all(&mut self) -> bool;

    fn read2(&mut self, addr: u32) -> Option<u16> {
        if let Some(x0) = self.read(addr) {
            if let Some(x1) = self.read(addr + 1) {
                return Some((x0 as u16) | ((x1 as u16) << 8));
            }
        };
        None
    }

    fn write2(&mut self, addr: u32, value: u16) -> bool {
        let lower = value & 0xff;
        let upper = (value & 0xff00) >> 8;
        self.write(addr, lower as u8) && self.write(addr + 1, upper as u8)
    }

    fn copy(&mut self, from: u32, to: u32, n: usize) -> bool {
        for i in 0..n {
            if let Some(x) = self.read(from + (i as u32)) {
                if !self.write(to + (i as u32), x) {
                    return false;
                }
            } else {
                return false;
            }
        }
        true
    }

    fn load_from_vec(&mut self, from: &[u8], addr: u32) -> bool {
        for (i, b) in from.iter().enumerate() {
            if !self.write(addr + (i as u32), *b) {
                return false;
            }
        }
        true
    }

    fn zero(&mut self, from: u32, to: u32) -> bool {
        for i in from..to {
            if !self.write(i, 0) {
                return false;
            }
        }
        true
    }
}

pub struct MemoryMapper {
    mapped: Vec<(usize, usize, RefCell<Box<dyn Addressable>>)>,
}

impl MemoryMapper {
    pub fn new() -> Self {
        Self{ mapped: Vec::new() }
    }

    pub fn map(&mut self, start: usize, size: usize, a: Box<dyn Addressable>) -> Result<(), String> {
        self.mapped.push((start, size, RefCell::new(a)));
        Ok(())
    }

    pub fn get_mapped(&mut self, addr: u32) -> Option<(usize, usize)> {
        let mut candidate: Option<(usize, usize)> = None;
        for (i, (start, _, _)) in self.mapped.iter().enumerate() {
            if *start <= (addr as usize) { 
                if let Some((c,_)) = candidate {
                    if *start > c {
                        candidate = Some((*start, i));
                    }
                } else {
                    candidate = Some((*start, i));
                }
            }
        };
        candidate
    }
}

impl Addressable for MemoryMapper {
    fn read(&mut self, addr: u32) -> Option<u8> {
        let (i,_) = self.get_mapped(addr)?;
        match self.mapped.get(i) {
            Some((start, size, a)) => {
                let addr_local = addr - (*start as u32);
                if addr_local >= *size as u32 {
                    None
                } else {
                    a.try_borrow_mut().unwrap().read(addr_local)
                }
            },
            None => None,
        }
    }

    fn write(&mut self, addr: u32, value: u8) -> bool {        
        if let Some((i,_)) = self.get_mapped(addr) {
            match self.mapped.get(i) {
                Some((start, size, a)) => {
                    let addr_local = addr - (*start as u32);
                    if addr_local >= *size as u32 {
                        false 
                    } else {
                        a.try_borrow_mut().unwrap().write(addr_local, value)
                    }
                },
                None => false,
            }
        } else {
            return false;
        }
    }

    fn zero_all(&mut self) -> bool {
        true
    }
}

pub struct LinearMemory {
    bytes: Vec<u8>,
    size: usize,
}

impl LinearMemory {
    pub fn new(n: usize) -> Self {
        Self {
            bytes: vec![0; n],
            size: n,
        }
    }
}

impl Addressable for LinearMemory {
    fn read(&mut self, addr: u32) -> Option<u8> {
        if (addr as usize) < self.size {
            Some(self.bytes[addr as usize])
        } else {
            None
        }
    }

    fn write(&mut self, addr: u32, value: u8) -> bool {
        if (addr as usize) < self.size {
            self.bytes[addr as usize] = value;
            true
        } else {
            false
        }
    }

    fn zero_all(&mut self) -> bool {
        self.zero(0, self.size as u32)
    }
}
