use std::{
    fmt::UpperHex,
    ops::{Add, BitAndAssign, BitOrAssign, BitXorAssign},
};

use num_conv::CastSigned;
use num_traits::{Bounded, WrappingAdd, WrappingSub, Zero};

use crate::bits::Bits;

pub trait Upcast {
    type UpcastedType: Add<Output: PartialOrd<Self::UpcastedType>> + PartialOrd + WrappingAdd;

    fn upcast(&self) -> Self::UpcastedType;
}

impl Upcast for u8 {
    type UpcastedType = u16;

    fn upcast(&self) -> Self::UpcastedType {
        *self as _
    }
}

impl Upcast for u16 {
    type UpcastedType = u32;

    fn upcast(&self) -> Self::UpcastedType {
        *self as u32
    }
}

pub trait NumericOps:
    WrappingAdd
    + WrappingSub
    + Copy
    + CastSigned<Signed: Zero + PartialOrd>
    + Zero
    + PartialEq
    + PartialOrd
    + Upcast
    + Bounded
    + Add
    + BitOrAssign
    + BitAndAssign
    + BitXorAssign
    + UpperHex
    + From<bool>
    + CalcFlags
{
}

impl NumericOps for u8 {}
impl NumericOps for u16 {}

// should this really be a trait....?
pub trait CalcFlags {
    fn calc_overflow(a: Self, b: Self, result: Self) -> bool;
    fn calc_af(a: Self, b: Self, result: Self) -> bool;
    fn calc_parity(result: Self) -> bool;
}

impl CalcFlags for u8 {
    fn calc_overflow(a: Self, b: Self, result: Self) -> bool {
        (a ^ result) & (b ^ result) & 0x80 != 0
    }
    fn calc_af(a: Self, b: Self, result: Self) -> bool {
        (a ^ b ^ result) & 0x10 != 0
    }
    fn calc_parity(result: Self) -> bool {
        result.count_ones() % 2 == 0
    }
}

impl CalcFlags for u16 {
    fn calc_overflow(a: Self, b: Self, result: Self) -> bool {
        (a ^ result) & (b ^ result) & 0x8000 != 0
    }
    fn calc_af(a: Self, b: Self, result: Self) -> bool {
        (a.get_low() ^ b.get_low() ^ result.get_low()) & 0x10 != 0
    }
    fn calc_parity(result: Self) -> bool {
        result.get_low().count_ones() % 2 == 0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn calc_overflow_test() {
        assert!(!u8::calc_overflow(0x64, 0x14, 0x78));
        assert!(u8::calc_overflow(0x64, 0x32, 0x96));
        assert!(!u8::calc_overflow(0xCE, 0xE2, 0xB0));
        assert!(u8::calc_overflow(0x9C, 0xB0, 0x4C));
        assert!(u8::calc_overflow(0x7F, 0x01, 0x80));
        assert!(u8::calc_overflow(0x80, 0xFF, 0x7F));
        assert!(!u8::calc_overflow(0x7F, 0xF6, 0x75));
    }

    #[test]
    fn calc_overflow_u16_test() {
        assert!(u16::calc_overflow(0x4E20, 0x4E20, 0x9C40));
        assert!(u16::calc_overflow(0x8AD0, 0x8AD0, 0x15A0));
        assert!(!u16::calc_overflow(0x2710, 0x4E20, 0x7530));
        assert!(!u16::calc_overflow(0xB1E0, 0xD8F0, 0x8AD0));
        assert!(u16::calc_overflow(0x7FFF, 0x0001, 0x8000));
        assert!(u16::calc_overflow(0x8000, 0xFFFF, 0x7FFF));
        assert!(!u16::calc_overflow(0x7FFF, 0xFC18, 0x7C17));
    }

    #[test]
    fn calc_aux_carry_test() {
        assert!(u8::calc_af(0x29, 0x4C, 0x75));
        assert!(u8::calc_af(0x38, 0x3D, 0x75));
        assert!(!u8::calc_af(0x0A, 0x05, 0x0F));
        assert!(!u8::calc_af(0x01, 0x01, 0x00));
        assert!(u8::calc_af(0x10, 0x01, 0x00));
        assert!(!u8::calc_af(0x00, 0x05, 0x05));
    }

    #[test]
    fn calc_aux_carry_u16_test() {
        assert!(u16::calc_af(0x000A, 0x0006, 0x0010));
        assert!(!u16::calc_af(0x0A00, 0x0105, 0x0B05));
    }

    #[test]
    fn calc_parity_test() {
        assert!(u8::calc_parity(0x00));
        assert!(!u8::calc_parity(0x01));
        assert!(!u8::calc_parity(0x02));
        assert!(u8::calc_parity(0x03));
        assert!(!u8::calc_parity(0x04));
        assert!(u8::calc_parity(0x05));
        assert!(u8::calc_parity(0x06));
        assert!(!u8::calc_parity(0x07));
        assert!(!u8::calc_parity(0x08));
        assert!(u8::calc_parity(0x09));
        assert!(u8::calc_parity(0x0A));
        assert!(!u8::calc_parity(0x0B));
        assert!(u8::calc_parity(0x0C));
        assert!(!u8::calc_parity(0x0D));
        assert!(!u8::calc_parity(0x0E));
        assert!(u8::calc_parity(0x0F));
    }

    #[test]
    fn calc_parity_u16_test() {
        assert!(u16::calc_parity(0xFF00));
        assert!(!u16::calc_parity(0xEE01));
        assert!(!u16::calc_parity(0xAA02));
        assert!(u16::calc_parity(0x0003));
        assert!(!u16::calc_parity(0xAA04));
        assert!(u16::calc_parity(0xBB05));
        assert!(u16::calc_parity(0x0006));
        assert!(!u16::calc_parity(0x0007));
        assert!(!u16::calc_parity(0x0C08));
        assert!(u16::calc_parity(0x0C09));
        assert!(u16::calc_parity(0x000A));
        assert!(!u16::calc_parity(0x000B));
        assert!(u16::calc_parity(0x000C));
        assert!(!u16::calc_parity(0x000D));
        assert!(!u16::calc_parity(0x000E));
        assert!(u16::calc_parity(0x000F));
    }
}
