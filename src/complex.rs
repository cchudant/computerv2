use std::fmt;

use crate::ops::*;
use crate::rational::Rational;

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub struct Complex {
    pub r: Rational,
    pub i: Rational,
}

impl Complex {
    pub fn new(r: Rational, i: Rational) -> Complex {
        Complex { r, i }
    }

    pub fn real(r: Rational) -> Complex {
        Complex {
            r,
            i: Rational::ZERO,
        }
    }
    pub fn imag(i: Rational) -> Complex {
        Complex {
            r: Rational::ZERO,
            i,
        }
    }

    pub fn is_real(self) -> bool {
        self.i == 0.into()
    }
    pub fn is_imag(self) -> bool {
        self.r == 0.into()
    }

    pub const ZERO: Complex = Complex {
        r: Rational::ZERO,
        i: Rational::ZERO,
    };
}

impl fmt::Display for Complex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.r == Rational::ZERO && self.i == Rational::ZERO {
            write!(f, "0")?;
        } else if self.r == Rational::ZERO {
            if self.i != 1.into() {
                write!(f, "{}", self.i.abs())?;
            }
            write!(f, "i")?;
        } else if self.i == Rational::ZERO {
            write!(f, "{}", self.r)?;
        } else {
            write!(f, "{} ", self.r)?;
            if self.i < Rational::ZERO {
                write!(f, "- ")?;
            } else {
                write!(f, "+ ")?;
            }
            if self.i != 1.into() {
                write!(f, "{}", self.i.abs())?;
            }
            write!(f, "i")?;
        }

        Ok(())
    }
}

impl Add for Complex {
    type Output = Complex;

    fn add(self, other: Complex) -> Complex {
        Complex::new(self.r + other.r, self.i + other.i)
    }
}

impl Sub for Complex {
    type Output = Complex;

    fn sub(self, other: Complex) -> Complex {
        Complex::new(self.r - other.r, self.i - other.i)
    }
}

impl Mul for Complex {
    type Output = Complex;

    // (a+bi)(c+di) = ac + adi + bci - bd
    fn mul(self, other: Complex) -> Complex {
        Complex::new(
            self.r * other.r - self.i * other.i,
            self.r * other.i + self.i * other.r,
        )
    }
}

impl TryDiv for Complex {
    type Output = Complex;

    // (a + bi)/(c + di) = (ac+bd)/(c^2+d^2) + (bc-ad)/(c^2+d^2)i
    fn try_div(self, other: Complex) -> Result<Self::Output, CalcError> {
        if other == Complex::ZERO {
            Err(CalcError {
                kind: CalcErrorKind::ComplexNotAllowed,
                op: "/",
                arg1: self.into(),
                arg2: other.into(),
            })?;
        }

        let overflow_err = CalcError {
            kind: CalcErrorKind::OverflowUnderflow,
            op: "/",
            arg1: self.into(),
            arg2: other.into(),
        };

        let den = other.r.checked_pow(2).ok_or_else(|| overflow_err.clone())?
            + other.i.checked_pow(2).ok_or_else(|| overflow_err.clone())?;

        Ok(Complex::new(
            (self.r * other.r + self.i * other.i) / den,
            (self.i * other.r - self.r * other.i) / den,
        ))
    }
}

impl TryRem for Complex {
    type Output = Complex;

    fn try_rem(self, other: Complex) -> Result<Self::Output, CalcError> {
        if !self.is_real() {
            Err(CalcError {
                kind: CalcErrorKind::ComplexNotAllowed,
                op: "%",
                arg1: self.into(),
                arg2: other.into(),
            })?;
        }

        if other == Complex::ZERO {
            Err(CalcError {
                kind: CalcErrorKind::ComplexNotAllowed,
                op: "/",
                arg1: self.into(),
                arg2: other.into(),
            })?;
        }

        Ok(Complex::real(self.r % other.r))
    }
}

impl Neg for Complex {
    type Output = Complex;

    fn neg(self) -> Complex {
        Complex::new(-self.r, -self.i)
    }
}

impl TryPow for Complex {
    type Output = Complex;

    fn try_pow(self, other: Complex) -> Result<Self::Output, CalcError> {
        if !other.is_real() {
            Err(CalcError {
                kind: CalcErrorKind::ComplexNotAllowed,
                op: "^",
                arg1: self.into(),
                arg2: other.into(),
            })?;
        }

        if other.r.get_den() != 1 || other.r.get_num() < 0 {
            Err(CalcError {
                kind: CalcErrorKind::ExpPositiveInt,
                op: "^",
                arg1: self.into(),
                arg2: other.into(),
            })?;
        }

        let overflow_err = CalcError {
            kind: CalcErrorKind::OverflowUnderflow,
            op: "/",
            arg1: self.into(),
            arg2: other.into(),
        };
        Ok(Complex::real(
            self.r
                .checked_pow(other.r.get_num() as u32)
                .ok_or(overflow_err)?,
        ))
    }
}
