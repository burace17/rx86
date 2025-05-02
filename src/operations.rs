use num_traits::{WrappingAdd, WrappingSub};

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
    let c: T = flags.contains(CpuFlags::CARRY).into();
    let val = reg.wrapping_add(&c);
    *rm = old.wrapping_add(&val);
    calc_add_flags(flags, old, val, *rm);

    let upcasted = old.upcast() + (*reg).upcast() + c.upcast();
    flags.set(CpuFlags::CARRY, upcasted > T::max_value().upcast());
    flags.set(CpuFlags::AUX_CARRY, T::calc_af(old, *reg, *rm));
    flags.set(
        CpuFlags::OVERFLOW,
        T::calc_overflow_adc(old, *reg, upcasted),
    );
}

pub fn sub<T>(rm: &mut T, reg: &mut T, flags: &mut CpuFlags)
where
    T: NumericOps,
{
    let old = *rm;
    *rm = old.wrapping_sub(reg);
    calc_sub_flags(flags, old, *reg, *rm);
    let upcasted = old.upcast().wrapping_sub(&((*reg).upcast()));
    flags.set(
        CpuFlags::OVERFLOW,
        T::calc_overflow_sbb(old, *reg, upcasted),
    );
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
    let upcasted = old.upcast().wrapping_sub(&((*reg).upcast() + c.upcast()));
    flags.set(CpuFlags::CARRY, carry);
    flags.set(CpuFlags::PARITY, T::calc_parity(*rm));
    flags.set(
        CpuFlags::OVERFLOW,
        T::calc_overflow_sbb(old, *reg, upcasted),
    );
    flags.set(CpuFlags::AUX_CARRY, T::calc_af_sbb(old, *reg, upcasted));
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
    flags.set(CpuFlags::ZERO, *rm == T::zero());
    flags.set(CpuFlags::SIGN, calc_sign_bit(*rm));
    flags.set(CpuFlags::PARITY, T::calc_parity(*rm));
    flags.remove(CpuFlags::CARRY);
    flags.remove(CpuFlags::OVERFLOW);
    flags.remove(CpuFlags::AUX_CARRY);
}

pub fn bitwise_and<T>(rm: &mut T, reg: &mut T, flags: &mut CpuFlags)
where
    T: NumericOps,
{
    *rm &= *reg;
    flags.set(CpuFlags::ZERO, *rm == T::zero());
    flags.set(CpuFlags::SIGN, calc_sign_bit(*rm));
    flags.set(CpuFlags::PARITY, T::calc_parity(*rm));
    flags.remove(CpuFlags::CARRY);
    flags.remove(CpuFlags::OVERFLOW);
    flags.remove(CpuFlags::AUX_CARRY);
}

pub fn bitwise_xor<T>(rm: &mut T, reg: &mut T, flags: &mut CpuFlags)
where
    T: NumericOps,
{
    *rm ^= *reg;
    flags.set(CpuFlags::ZERO, *rm == T::zero());
    flags.set(CpuFlags::SIGN, calc_sign_bit(*rm));
    flags.set(CpuFlags::PARITY, T::calc_parity(*rm));
    flags.remove(CpuFlags::CARRY);
    flags.remove(CpuFlags::OVERFLOW);
}
