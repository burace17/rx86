pub trait Upcast {
    type UpcastedType;

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
