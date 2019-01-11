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
impl Register {
    pub fn arg(i: u8) -> Register {
        use self::Register::*;
        match i {
            0 => A0,
            1 => A1,
            2 => A2,
            3 => A3,
            i => panic!("No argument register nr. {}", i),
        }
    }

    pub fn temporaries() -> [Register; 8] {
        use self::Register::*;
        [T0, T1, T2, T3, T4, T5, T6, T7]
    }
}
