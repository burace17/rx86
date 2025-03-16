use num_traits::WrappingAdd;

use crate::{
    cpu::CpuFlags,
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

pub fn add<T>(rm: &mut T, reg: &mut T, flags: &mut CpuFlags)
where
    T: NumericOps,
{
    let old = *rm;
    *rm = old.wrapping_add(reg);
    calc_add_flags(flags, old, *reg, *rm);
}

pub fn add_with_carry<T>(rm: &mut T, reg: &mut T, flags: &mut CpuFlags)
where
    T: NumericOps,
{
    let old = *rm;
    let val = old.wrapping_add(reg);
    let c: T = flags.contains(CpuFlags::CARRY).into();
    *rm = val.wrapping_add(&c);
    calc_add_flags(flags, old, *reg, *rm); // check?
}

pub fn sub<T>(rm: &mut T, reg: &mut T, flags: &mut CpuFlags)
where
    T: NumericOps,
{
    let old = *rm;
    *rm = old.wrapping_sub(reg);
    calc_sub_flags(flags, old, *reg, *rm);
}

pub fn sub_with_borrow<T>(rm: &mut T, reg: &mut T, flags: &mut CpuFlags)
where
    T: NumericOps,
{
    let old = *rm;
    let c: T = flags.contains(CpuFlags::CARRY).into();
    let val = (*reg).wrapping_add(&c);
    *rm = old.wrapping_sub(&val);
    let carry = old.upcast() < ((*reg).upcast().wrapping_add(&c.upcast()));
    flags.set(CpuFlags::CARRY, carry);
    flags.set(CpuFlags::ZERO, *rm == T::zero());
    flags.set(CpuFlags::SIGN, calc_sign_bit(*rm));
}

pub fn cmp<T>(rm: &mut T, reg: &mut T, flags: &mut CpuFlags)
where
    T: NumericOps,
{
    only_flags(sub)(rm, reg, flags);
}

pub fn xchg<T>(rm: &mut T, reg: &mut T, _flags: &mut CpuFlags)
where
    T: NumericOps,
{
    std::mem::swap(rm, reg);
}

pub fn mov<T>(rm: &mut T, reg: &mut T, _flags: &mut CpuFlags)
where
    T: NumericOps,
{
    *rm = *reg;
}

pub fn bitwise_or<T>(rm: &mut T, reg: &mut T, flags: &mut CpuFlags)
where
    T: NumericOps,
{
    *rm |= *reg;
    flags.remove(CpuFlags::CARRY);
    flags.set(CpuFlags::ZERO, *rm == T::zero());
    flags.set(CpuFlags::SIGN, calc_sign_bit(*rm));
    // todo clear overflow flag
}

pub fn bitwise_and<T>(rm: &mut T, reg: &mut T, flags: &mut CpuFlags)
where
    T: NumericOps,
{
    *rm &= *reg;
    flags.remove(CpuFlags::CARRY);
    flags.set(CpuFlags::ZERO, *rm == T::zero());
    flags.set(CpuFlags::SIGN, calc_sign_bit(*rm));
    // todo clear overflow flag
}

pub fn bitwise_xor<T>(rm: &mut T, reg: &mut T, flags: &mut CpuFlags)
where
    T: NumericOps,
{
    *rm ^= *reg;
    flags.remove(CpuFlags::CARRY);
    flags.set(CpuFlags::ZERO, *rm == T::zero());
    flags.set(CpuFlags::SIGN, calc_sign_bit(*rm));
    // todo clear overflow flag
}
