use num_traits::WrappingAdd;

use crate::{
    bits::Bits,
    cpu::{CARRY_FLAG, SIGN_FLAG, ZERO_FLAG},
    instructions::{calc_add_flags, calc_sign_bit, calc_sub_flags},
    traits::NumericOps,
};

pub fn swap_args<F, T, T2>(op: F) -> impl Fn(&mut T, &mut T, &mut T2)
where
    F: Fn(&mut T, &mut T, &mut T2),
{
    move |a, b, c| op(b, a, c)
}

pub fn only_flags<F, T, T2>(op: F) -> impl Fn(&mut T, &mut T, &mut T2)
where
    F: Fn(&mut T, &mut T, &mut T2),
    T: NumericOps,
{
    move |a, b, c| {
        let mut ax = *a;
        let mut bx = *b;
        op(&mut ax, &mut bx, c)
    }
}

pub fn add<T>(rm: &mut T, reg: &mut T, flags: &mut u16)
where
    T: NumericOps,
{
    let old = *rm;
    *rm = old.wrapping_add(reg);
    calc_add_flags(flags, old, *reg, *rm);
}

pub fn add_with_carry<T>(rm: &mut T, reg: &mut T, flags: &mut u16)
where
    T: NumericOps,
{
    let old = *rm;
    let val = old.wrapping_add(reg);
    let c: T = flags.get_bit(CARRY_FLAG).into();
    *rm = val.wrapping_add(&c);
    calc_add_flags(flags, old, *reg, *rm); // check?
}

pub fn sub<T>(rm: &mut T, reg: &mut T, flags: &mut u16)
where
    T: NumericOps,
{
    let old = *rm;
    *rm = old.wrapping_sub(reg);
    calc_sub_flags(flags, old, *reg, *rm);
}

pub fn sub_with_borrow<T>(rm: &mut T, reg: &mut T, flags: &mut u16)
where
    T: NumericOps,
{
    let old = *rm;
    let c: T = flags.get_bit(CARRY_FLAG).into();
    let val = (*reg).wrapping_add(&c);
    *rm = old.wrapping_sub(&val);
    let carry = old.upcast() < ((*reg).upcast().wrapping_add(&c.upcast()));
    flags.set_bit(CARRY_FLAG, carry);
    flags.set_bit(ZERO_FLAG, *rm == T::zero());
    flags.set_bit(SIGN_FLAG, calc_sign_bit(*rm));
}

pub fn bitwise_or<T>(rm: &mut T, reg: &mut T, flags: &mut u16)
where
    T: NumericOps,
{
    *rm |= *reg;
    flags.set_bit(CARRY_FLAG, false);
    flags.set_bit(ZERO_FLAG, *rm == T::zero());
    flags.set_bit(SIGN_FLAG, calc_sign_bit(*rm));
    // todo clear overflow flag
}

pub fn bitwise_and<T>(rm: &mut T, reg: &mut T, flags: &mut u16)
where
    T: NumericOps,
{
    *rm &= *reg;
    flags.set_bit(CARRY_FLAG, false);
    flags.set_bit(ZERO_FLAG, *rm == T::zero());
    flags.set_bit(SIGN_FLAG, calc_sign_bit(*rm));
    // todo clear overflow flag
}

pub fn bitwise_xor<T>(rm: &mut T, reg: &mut T, flags: &mut u16)
where
    T: NumericOps,
{
    *rm ^= *reg;
    flags.set_bit(CARRY_FLAG, false);
    flags.set_bit(ZERO_FLAG, *rm == T::zero());
    flags.set_bit(SIGN_FLAG, calc_sign_bit(*rm));
    // todo clear overflow flag
}
