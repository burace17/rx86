pub fn read_word(mem: &[u8], loc: &Vec<usize>) -> u16 {
    u16::from_le_bytes([mem[loc[0]], mem[loc[1]]])
}

pub fn write_word(mem: &mut [u8], loc: &Vec<usize>, val: u16) {
    let bytes = val.to_le_bytes();
    mem[loc[0]] = bytes[0];
    mem[loc[1]] = bytes[1];
}
