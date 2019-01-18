use std::fmt::{Formatter, Display, Error as FormatError};

#[derive(Debug, Copy, Clone)]
pub enum Register {
    Zero,
    At,
    V0,
    V1,
    A0,
    A1,
    A2,
    A3,
    T0,
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    S0,
    S1,
    S2,
    S3,
    S4,
    S5,
    S6,
    S7,
    T8,
    T9,
    K0,
    K1,
    Gp,
    Sp,
    Fp,
    Ra,
}
use self::Register::*;

impl Register {
    pub fn args() -> [Register; 4] {
        [A0, A1, A2, A3]
    }
}

impl Display for Register {
    fn fmt(&self, f: &mut Formatter) -> Result<(), FormatError> {
        let s = match self {
            Zero => "$zero",
            At => "$at",
            V0 => "$v0",
            V1 => "$v1",
            A0 => "$a0",
            A1 => "$a1",
            A2 => "$a2",
            A3 => "$a3",
            T0 => "$t0",
            T1 => "$t1",
            T2 => "$t2",
            T3 => "$t3",
            T4 => "$t4",
            T5 => "$t5",
            T6 => "$t6",
            T7 => "$t7",
            S0 => "$s0",
            S1 => "$s1",
            S2 => "$s2",
            S3 => "$s3",
            S4 => "$s4",
            S5 => "$s5",
            S6 => "$s6",
            S7 => "$s7",
            T8 => "$t8",
            T9 => "$t9",
            K0 => "$k0",
            K1 => "$k1",
            Gp => "$gp",
            Sp => "$sp",
            Fp => "$fp",
            Ra => "$ra",
        };
        write!(f, "{}", s)
    }
}

