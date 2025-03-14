use std::ops::{Add, BitAndAssign, BitOrAssign, BitXorAssign};

use num_conv::CastSigned;
use num_traits::{Bounded, WrappingAdd, WrappingSub, Zero};

pub trait Upcast {
    type UpcastedType: Add<Output: PartialOrd<Self::UpcastedType>> + PartialOrd;

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
    + From<bool>
{
}

impl NumericOps for u8 {}
impl NumericOps for u16 {}
