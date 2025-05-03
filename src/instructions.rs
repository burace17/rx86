use num_traits::Zero;

use crate::{
    cpu::CpuFlags,
    traits::{CalcFlags, NumericOps, Upcast},
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
    Memory(usize),
}

pub fn is_addressing_mode(id_mod: u8) -> bool {
    id_mod != 0b11
}

// can relocate these to operations.rs sometime
pub fn inc_reg(reg: &mut u16, flags: &mut CpuFlags) -> u16 {
    let old = *reg;
    *reg = (*reg).wrapping_add(1);
    flags.set(CpuFlags::ZERO, *reg == 0);
    flags.set(CpuFlags::SIGN, calc_sign_bit(*reg));
    flags.set(CpuFlags::PARITY, u16::calc_parity(*reg));
    flags.set(CpuFlags::OVERFLOW, u16::calc_overflow(old, 1, *reg));
    flags.set(CpuFlags::AUX_CARRY, u16::calc_af(old, 1, *reg));
    1
}

pub fn dec_reg(reg: &mut u16, flags: &mut CpuFlags) -> u16 {
    let old = *reg;
    *reg = (*reg).wrapping_sub(1);
    flags.set(CpuFlags::ZERO, *reg == 0);
    flags.set(CpuFlags::SIGN, calc_sign_bit(*reg));
    flags.set(CpuFlags::PARITY, u16::calc_parity(*reg));
    let upcasted = old.upcast().wrapping_sub(1);
    flags.set(
        CpuFlags::OVERFLOW,
        u16::calc_overflow_sbb(old, 1, upcasted),
    );
    flags.set(CpuFlags::AUX_CARRY, u16::calc_af(old, 1, *reg));
    1
}

pub fn inc_byte(reg: &mut u8, flags: &mut CpuFlags) -> u16 {
    let old = *reg;
    *reg = (*reg).wrapping_add(1);
    flags.set(CpuFlags::ZERO, *reg == 0);
    flags.set(CpuFlags::SIGN, calc_sign_bit(*reg));
    flags.set(CpuFlags::PARITY, u8::calc_parity(*reg));
    flags.set(CpuFlags::OVERFLOW, u8::calc_overflow(old, 1, *reg));
    flags.set(CpuFlags::AUX_CARRY, u8::calc_af(old, 1, *reg));
    1
}

pub fn dec_byte(reg: &mut u8, flags: &mut CpuFlags) -> u16 {
    let old = *reg;
    *reg = (*reg).wrapping_sub(1);
    flags.set(CpuFlags::ZERO, *reg == 0);
    flags.set(CpuFlags::SIGN, calc_sign_bit(*reg));
    flags.set(CpuFlags::PARITY, u8::calc_parity(*reg));
    let upcasted = old.upcast().wrapping_sub(1);
    flags.set(
        CpuFlags::OVERFLOW,
        u8::calc_overflow_sbb(old, 1, upcasted),
    );
    flags.set(CpuFlags::AUX_CARRY, u8::calc_af(old, 1, *reg));
    1
}

pub fn swap_reg(ax: &mut u16, xx: &mut u16) -> u16 {
    std::mem::swap(ax, xx);
    1
}

fn jmp(mem: &[u8], ip: &mut u16) {
    let mut sip = *ip as i16;
    sip += (mem[(*ip + 1) as usize] as i8) as i16;
    *ip = sip as u16;
}

pub fn jmp_if_any_set(mem: &[u8], ip: &mut u16, flags: CpuFlags, mask: CpuFlags) -> u16 {
    if flags.intersects(mask) {
        jmp(mem, ip);
    }
    2
}

pub fn jmp_if_none_set(mem: &[u8], ip: &mut u16, flags: CpuFlags, mask: CpuFlags) -> u16 {
    if (flags & mask).is_empty() {
        jmp(mem, ip);
    }
    2
}

pub fn jmp_if<F>(mem: &[u8], ip: &mut u16, f: F) -> u16
where
    F: Fn() -> bool,
{
    if f() {
        jmp(mem, ip);
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

pub fn calc_add_flags<T>(flags: &mut CpuFlags, left: T, right: T, result: T)
where
    T: NumericOps,
{
    flags.set(CpuFlags::CARRY, calc_carry_bit(left, right));
    flags.set(CpuFlags::PARITY, T::calc_parity(result));
    flags.set(CpuFlags::OVERFLOW, T::calc_overflow(left, right, result));
    flags.set(CpuFlags::AUX_CARRY, T::calc_af(left, right, result));
    flags.set(CpuFlags::SIGN, calc_sign_bit(result));
    flags.set(CpuFlags::ZERO, result == T::zero());
}

pub fn calc_sub_flags<T>(flags: &mut CpuFlags, left: T, right: T, result: T)
where
    T: NumericOps,
{
    flags.set(CpuFlags::CARRY, left < right);
    flags.set(CpuFlags::PARITY, T::calc_parity(result));
    flags.set(CpuFlags::OVERFLOW, T::calc_overflow(left, right, result));
    flags.set(CpuFlags::AUX_CARRY, T::calc_af(left, right, result));
    flags.set(CpuFlags::SIGN, calc_sign_bit(result));
    flags.set(CpuFlags::ZERO, result == T::zero());
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
