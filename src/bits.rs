pub trait Bits {
    fn get_low(&self) -> u8;
    fn get_high(&self) -> u8;
    fn set_low(&mut self, value: u8);
    fn set_high(&mut self, value: u8);
    fn get_bit(&self, n: u16) -> bool;
    fn set_bit(&mut self, n: u16, val: bool);
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
    fn get_bit(&self, n: u16) -> bool {
        ((1 << n) & self) != 0
    }
    fn set_bit(&mut self, n: u16, val: bool) {
        if val {
            *self |= 1 << n;
        } else {
            *self &= !(1 << n);
        }
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

        test = 0b0101;
        assert!(test.get_bit(0));
        assert!(!test.get_bit(1));
        assert!(test.get_bit(2));

        test.set_bit(0, false);
        assert_eq!(test, 4);
        assert!(!test.get_bit(0));
        test.set_bit(2, false);
        assert_eq!(test, 0);
        assert!(!test.get_bit(2));
    }
}
