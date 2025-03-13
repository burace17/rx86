use crate::{
    bits::Bits,
    cpu::{CARRY_FLAG, SIGN_FLAG, ZERO_FLAG},
    memory::{read_word, write_word},
};

#[derive(Clone, Copy)]
pub struct ModRmByte {
    pub id_mod: u8,
    pub id_reg: u8,
    pub id_rm: u8,
}

impl ModRmByte {
    pub fn unpack(&self) -> (u8, u8, u8) {
        (self.id_mod, self.id_reg, self.id_rm)
    }
}

pub fn parse_mod_rm_byte(modrm: u8) -> ModRmByte {
    // Mod R/M byte format
    // 00 | 000 | 000
    // Mod  Reg   R/M
    let id_mod = (modrm & 0xC0) >> 6;
    let id_reg = (modrm & 0x38) >> 3;
    let id_rm = modrm & 0x07;
    ModRmByte {
        id_mod,
        id_reg,
        id_rm,
    }
}

pub enum RegisterOrMemory {
    Register(u8),
    Memory(u16),
}

pub fn is_addressing_mode(id_mod: u8) -> bool {
    id_mod != 0b11
}

pub fn inc_reg(reg: &mut u16, flags: &mut u16) -> u16 {
    *reg = (*reg).wrapping_add(1);
    flags.set_bit(ZERO_FLAG, *reg == 0);
    flags.set_bit(SIGN_FLAG, calc_word_sign_bit(*reg));
    1
}

pub fn dec_reg(reg: &mut u16, flags: &mut u16) -> u16 {
    *reg = (*reg).wrapping_sub(1);
    flags.set_bit(ZERO_FLAG, *reg == 0);
    flags.set_bit(SIGN_FLAG, calc_word_sign_bit(*reg));
    1
}

pub fn inc_byte(reg: &mut u8, flags: &mut u16) -> u16 {
    *reg = (*reg).wrapping_add(1);
    flags.set_bit(ZERO_FLAG, *reg == 0);
    flags.set_bit(SIGN_FLAG, calc_byte_sign_bit(*reg));
    1
}

pub fn dec_byte(reg: &mut u8, flags: &mut u16) -> u16 {
    *reg = (*reg).wrapping_sub(1);
    flags.set_bit(ZERO_FLAG, *reg == 0);
    flags.set_bit(SIGN_FLAG, calc_byte_sign_bit(*reg));
    1
}

pub fn push_reg(mem: &mut [u8], sp: &mut u16, reg: u16) -> u16 {
    *sp -= 2;
    write_word(mem, *sp as usize, reg);
    1
}

pub fn pop_reg(mem: &[u8], sp: &mut u16, reg: &mut u16) -> u16 {
    *reg = read_word(mem, *sp as usize);
    *sp += 2;
    1
}

pub fn swap_reg(ax: &mut u16, xx: &mut u16) -> u16 {
    std::mem::swap(ax, xx);
    1
}

pub fn mov_reg_imm_word(reg: &mut u16, imm: u16) -> u16 {
    *reg = imm;
    3
}

pub fn mov_reg_imm_byte(reg: &mut u16, imm: u8, high: bool) -> u16 {
    if high {
        (*reg).set_high(imm);
    } else {
        (*reg).set_low(imm);
    }
    2
}

pub fn calc_word_sign_bit(val: u16) -> bool {
    (val as i16) < 0
}

pub fn calc_byte_sign_bit(val: u8) -> bool {
    (val as i8) < 0
}

fn calc_word_add_carry_bit(a: u16, b: u16) -> bool {
    let sum = a as u32 + b as u32;
    sum > 0xFFFF
}

fn calc_byte_add_carry_bit(a: u8, b: u8) -> bool {
    let sum = a as u16 + b as u16;
    sum > 0xFF
}

pub fn calc_add_word_flags(flags: &mut u16, left: u16, right: u16, result: u16) {
    flags.set_bit(CARRY_FLAG, calc_word_add_carry_bit(left, right));
    flags.set_bit(SIGN_FLAG, calc_word_sign_bit(result));
    flags.set_bit(ZERO_FLAG, result == 0);
}

pub fn calc_add_byte_flags(flags: &mut u16, left: u8, right: u8, result: u8) {
    flags.set_bit(CARRY_FLAG, calc_byte_add_carry_bit(left, right));
    flags.set_bit(SIGN_FLAG, calc_byte_sign_bit(result));
    flags.set_bit(ZERO_FLAG, result == 0);
}

pub fn calc_sub_word_flags(flags: &mut u16, left: u16, right: u16, result: u16) {
    flags.set_bit(CARRY_FLAG, left < right);
    flags.set_bit(SIGN_FLAG, calc_word_sign_bit(result));
    flags.set_bit(ZERO_FLAG, result == 0);
}

pub fn calc_sub_byte_flags(flags: &mut u16, left: u8, right: u8, result: u8) {
    flags.set_bit(CARRY_FLAG, left < right);
    flags.set_bit(SIGN_FLAG, calc_byte_sign_bit(result));
    flags.set_bit(ZERO_FLAG, result == 0);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn calc_word_sign_bit_test() {
        assert!(calc_word_sign_bit(0xFFFF));
        assert!(!calc_word_sign_bit(0x0000));
        assert!(calc_word_sign_bit(0x8000));
        assert!(!calc_word_sign_bit(0x1000));
        assert!(!calc_word_sign_bit(0x0001));
        assert!(!calc_word_sign_bit(0x0008));
        assert!(!calc_word_sign_bit(0x0080));
        assert!(!calc_word_sign_bit(0x0800));
    }

    #[test]
    fn calc_byte_sign_bit_test() {
        assert!(!calc_byte_sign_bit(0x01));
        assert!(!calc_byte_sign_bit(0x08));
        assert!(calc_byte_sign_bit(0x80));
        assert!(!calc_byte_sign_bit(0x10));
    }
}
