pub trait Bits {
    fn get_low(&self) -> u8;
    fn get_high(&self) -> u8;
    fn set_low(&mut self, value: u8);
    fn set_high(&mut self, value: u8);
}

impl Bits for u16 {
    fn get_low(&self) -> u8 {
        (self & 0x00FF) as u8
    }
    fn get_high(&self) -> u8 {
        ((self & 0xFF00) >> 8) as u8
    }
    fn set_low(&mut self, value: u8) {
        *self &= !0 << 8;
        *self |= value as u16;
    }
    fn set_high(&mut self, value: u8) {
        *self &= !0 >> 8;
        *self |= (value as u16) << 8;
    }
}

#[cfg(test)]
mod tests {
    use crate::bits::Bits;
    #[test]
    fn bits_u16_test() {
        let mut test: u16 = 0x41F4;
        assert_eq!(test.get_low(), 0xF4);
        assert_eq!(test.get_high(), 0x41);
        test.set_low(0xEF);
        assert_eq!(test, 0x41EF);
        test.set_high(0xBE);
        assert_eq!(test, 0xBEEF);
        assert_eq!(test.get_low(), 0xEF);
        assert_eq!(test.get_high(), 0xBE);
    }
}
