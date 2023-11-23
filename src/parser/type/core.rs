use std::fmt::{Debug, Display, Formatter, Write};
use crate::parser::ast::ast_type::AstType;
use crate::parser::r#type::{TypeSignature, TypeSize};



#[derive(Debug)]
pub struct CoreType<T: Debug> {
    pub name: &'static str,
    pub val: T,
}


pub const F16_FRAC: usize = 1 << 10;
pub const F32_FRAC: usize = 1 << 23;
pub const F64_FRAC: usize = 1 << 52;

#[derive(Debug, PartialEq, Eq)]
pub struct Void();

impl Display for Void {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "void")
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct f16(u16);

impl f16 {
    pub fn new(sign: bool, exp: u8, frac: u16) -> f16 {
        f16(
            ((sign as u16) << 15)
                | (((exp as u16) & 0b11111) << 10)
                | (frac & 0b11_1111_1111)
        )
    }

    pub fn sign(&self) -> bool {
        (self.0 >> 15) != 0
    }

    pub fn exp(&self) -> u8 {
        ((self.0 >> 10) & 0b11111) as u8
    }

    pub fn frac(&self) -> u16 {
        self.0 & 0b11_1111_1111
    }

    pub fn normal(x: usize, y: usize, scale: usize) -> usize {
        (x * scale) / (1 << 15) + (y * x * scale) / (F16_FRAC * (1 << 15))
    }

    pub fn subnormal(y: usize, scale: usize) -> usize {
        (y * scale) / (F16_FRAC * (1 << 14))
    }

    pub fn unsigned_value(&self, a: usize) -> usize {
        match (self.exp(), self.frac()) {
            (0, frac) => {
                todo!()
            },
            (0b11111, frac) => {
                todo!()
            },
            (x,  frac) => {
                todo!()
            },
        }
    }
}

impl Display for f16 {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match (self.sign(), self.exp(), self.frac()) {
            (sign, 0, frac) => {
                // subnormal values
                if sign {
                    write!(f, "-")?;
                }
                let y = frac as usize;
                let mut old_val = f16::subnormal(y, 1);
                write!(f, "{}.", old_val)?;

                let chars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];
                for i in 1..24 {
                    let val = f16::subnormal(y, i);
                    let diff = val - old_val * 10;
                    old_val = val;
                    f.write_char(chars[diff])?;
                }
            },
            (true, 0b11111, 0) => {
                // Inf
                write!(f, "-inf")?;
            },
            (false, 0b11111, 0) => {
                // Inf
                write!(f, "inf")?;
            },
            (_, 0b11111, frac) => {
                // NaN
                write!(f, "NaN")?;
            },
            (sign, exp, frac) => {
                // normal values
                if sign {
                    write!(f, "-")?;
                }
                let x = 1_usize << exp;
                let y = frac as usize;
                let mut old_val = f16::normal(x, y, 1);
                write!(f, "{}.", old_val)?;

                let chars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];
                for i in 1..24 {
                    let val = f16::normal(x, y, i);
                    let diff = val - old_val * 10;
                    old_val = val;
                    f.write_char(chars[diff])?;
                }
            }
        }
        Ok(())
    }
}

impl From<f16> for f32 {
    fn from(value: f16) -> Self {
        let exp = value.exp();
        let frac = value.frac() as u32 * (1 << 13);
        let sign = value.sign();

        f32::from_bits(
            ((sign as u32) << 31)
            | ((exp as u32) << 23)
            | (frac & 0x7fffff)
        )
    }
}

impl From<f32> for f16 {
    fn from(value: f32) -> Self {
        let bits = f32::to_bits(value);
        let exp = ((bits >> 23) & 0b1111_1111) as u8;
        let frac = ((bits & 0x7fffff) / (1 << 13)) as u16;
        let sign = (bits >> 31) != 0;

        dbg!(exp, frac, sign);

        f16::new(sign, exp, frac)
    }
}


impl<T> Display for CoreType<T>
where T: Display + Debug {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.val, self.name)
    }
}

macro_rules! impl_cores(
    ($($c:ident = $name:literal -> $rust:ty, size = $size:expr, alignment = $align:expr);+) => ($(
        pub const $c: &str = $name;

        impl CoreType<$rust> {
            pub fn new(val: $rust) -> Self {
                CoreType {
                    name: $name,
                    val,
                }
            }

            pub fn ast_type() -> AstType {
                AstType::Base($name.into())
            }
        }

        impl TypeSignature for CoreType<$rust> {
            fn size(&self) -> TypeSize {
                $size
            }

            fn alignment(&self) -> usize {
                $align
            }
        }
    )+);
);

impl_cores!(
    CORE_U8 = "u8" -> u8, size = TypeSize::Sized(8), alignment = 8;
    CORE_U16 = "u16" -> u16, size = TypeSize::Sized(16), alignment = 16;
    CORE_U32 = "u32" -> u32, size = TypeSize::Sized(32), alignment = 32;
    CORE_U64 = "u64" -> u64, size = TypeSize::Sized(64), alignment = 64;
    CORE_USIZE = "usize" -> usize, size = TypeSize::Sized(64), alignment = 64;
    CORE_I8 = "i8" -> i8, size = TypeSize::Sized(8), alignment = 8;
    CORE_I16 = "i16" -> i16, size = TypeSize::Sized(16), alignment = 16;
    CORE_I32 = "i32" -> i32, size = TypeSize::Sized(32), alignment = 32;
    CORE_I64 = "i64" -> i64, size = TypeSize::Sized(64), alignment = 64;
    CORE_ISIZE = "isize" -> isize, size = TypeSize::Sized(64), alignment = 64;
    CORE_F32 = "f32" -> f32, size = TypeSize::Sized(32), alignment = 32;
    CORE_F64 = "f64" -> f64, size = TypeSize::Sized(64), alignment = 64;
    CORE_CHAR = "char" -> char, size = TypeSize::Sized(32), alignment = 32;
    CORE_STR = "str" -> String, size = TypeSize::Unsized, alignment = 64;
    CORE_BOOL = "bool" -> bool, size = TypeSize::Sized(1), alignment = 1;
    CORE_VOID = "void" -> Void, size = TypeSize::Sized(0), alignment = 1
);


#[cfg(test)]
mod test {
    use crate::parser::r#type::core::f16;

    #[test]
    fn test_f16() {
        let tmp: f16 = 3.1415_f32.into();
        println!("{tmp}");
    }
}
