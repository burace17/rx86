use num_traits::Zero;

use crate::{
    bits::Bits,
    cpu::{CARRY_FLAG, SIGN_FLAG, ZERO_FLAG},
    memory::{read_word, write_word},
    traits::NumericOps,
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
    flags.set_bit(SIGN_FLAG, calc_sign_bit(*reg));
    1
}

pub fn dec_reg(reg: &mut u16, flags: &mut u16) -> u16 {
    *reg = (*reg).wrapping_sub(1);
    flags.set_bit(ZERO_FLAG, *reg == 0);
    flags.set_bit(SIGN_FLAG, calc_sign_bit(*reg));
    1
}

pub fn inc_byte(reg: &mut u8, flags: &mut u16) -> u16 {
    *reg = (*reg).wrapping_add(1);
    flags.set_bit(ZERO_FLAG, *reg == 0);
    flags.set_bit(SIGN_FLAG, calc_sign_bit(*reg));
    1
}

pub fn dec_byte(reg: &mut u8, flags: &mut u16) -> u16 {
    *reg = (*reg).wrapping_sub(1);
    flags.set_bit(ZERO_FLAG, *reg == 0);
    flags.set_bit(SIGN_FLAG, calc_sign_bit(*reg));
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

pub fn calc_sign_bit<T>(val: T) -> bool
where
    T: NumericOps,
{
    val.cast_signed() < T::Signed::zero()
}

pub fn calc_carry_bit<T>(a: T, b: T) -> bool
where
    T: NumericOps,
{
    let sum = a.upcast() + b.upcast();
    sum > T::max_value().upcast()
}

pub fn calc_add_flags<T>(flags: &mut u16, left: T, right: T, result: T)
where
    T: NumericOps,
{
    flags.set_bit(CARRY_FLAG, calc_carry_bit(left, right));
    flags.set_bit(SIGN_FLAG, calc_sign_bit(result));
    flags.set_bit(ZERO_FLAG, result == T::zero());
}

pub fn calc_sub_flags<T>(flags: &mut u16, left: T, right: T, result: T)
where
    T: NumericOps,
{
    flags.set_bit(CARRY_FLAG, left < right);
    flags.set_bit(SIGN_FLAG, calc_sign_bit(result));
    flags.set_bit(ZERO_FLAG, result == T::zero());
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn calc_word_sign_bit_test() {
        assert!(calc_sign_bit(0xFFFF_u16));
        assert!(!calc_sign_bit(0x0000_u16));
        assert!(calc_sign_bit(0x8000_u16));
        assert!(!calc_sign_bit(0x1000_u16));
        assert!(!calc_sign_bit(0x0001_u16));
        assert!(!calc_sign_bit(0x0008_u16));
        assert!(!calc_sign_bit(0x0080_u16));
        assert!(!calc_sign_bit(0x0800_u16));
    }

    #[test]
    fn calc_byte_sign_bit_test() {
        assert!(!calc_sign_bit(0x01_u8));
        assert!(!calc_sign_bit(0x08_u8));
        assert!(calc_sign_bit(0x80_u8));
        assert!(!calc_sign_bit(0x10_u8));
    }
}
