pub fn read_word(mem: &[u8], loc: usize) -> u16 {
    u16::from_le_bytes([mem[loc], mem[loc + 1]])
}

pub fn write_word(mem: &mut [u8], loc: usize, val: u16) {
    let bytes = val.to_le_bytes();
    mem[loc] = bytes[0];
    mem[loc + 1] = bytes[1];
}

#[cfg(test)]
mod tests {
    use super::*;
    const TEST_MEM_SIZE: usize = 65535;

    #[test]
    fn mem_read_write_test() {
        let mut mem = vec![0; TEST_MEM_SIZE];
        mem[0x44] = 0xFF;
        mem[0x45] = 0x13;
        mem[0x46] = 0x33;
        mem[0x47] = 0x12;

        assert_eq!(read_word(&mem, 0x45), 0x3313);
        write_word(&mut mem, 0x45, 0x1014);
        assert_eq!(mem[0x44], 0xFF);
        assert_eq!(mem[0x45], 0x14);
        assert_eq!(mem[0x46], 0x10);
        assert_eq!(mem[0x47], 0x12);
        assert_eq!(read_word(&mem, 0x45), 0x1014);
    }
}
