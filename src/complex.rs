use std::fmt;
use std::ops::{Add, Sub, Mul, Div, Neg};

use crate::rational::Rational;

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub struct Complex(Rational, Rational);

impl Complex {
    pub fn new(r: Rational, i: Rational) -> Complex {
        Complex(r, i)
    }

    pub fn real(r: Rational) -> Complex {
        Complex(r, Rational::ZERO)
    }
    pub fn imag(i: Rational) -> Complex {
        Complex(Rational::ZERO, i)
    }

    pub fn is_real(self) -> bool {
        self.1 == 0.into()
    }
    pub fn is_imag(self) -> bool {
        self.0 == 0.into()
    }

    pub fn powi(self, i: u32) -> Complex {
        
    }
}

impl fmt::Display for Complex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.0 == Rational::ZERO && self.1 == Rational::ZERO {
            write!(f, "0")?;
        } else if self.0 == Rational::ZERO {
            write!(f, "{}i", self.1)?;
        } else if self.1 == Rational::ZERO {
            write!(f, "{}", self.0)?;
        } else {
            write!(f, "{} ", self.0)?;
            if self.1 < Rational::ZERO {
                write!(f, "- ")?;
            } else {
                write!(f, "+ ")?;
            }
            write!(f, "{}i", self.1.abs())?;
        }

        Ok(())
    }
}

impl Sub for Complex {
    type Output = Complex;

    fn sub(self, other: Complex) -> Complex {
        Complex(self.0 - other.0, self.1 - other.1)
    }
}

impl Add for Complex {
    type Output = Complex;

    fn add(self, other: Complex) -> Complex {
        Complex(self.0 + other.0, self.1 + other.1)
    }
}

impl Mul for Complex {
    type Output = Complex;

    // (a+bi)(c+di) = ac + adi + bci - bd
    fn mul(self, other: Complex) -> Complex {
        Complex(self.0 * other.0 - self.1 * other.1, self.0 * other.1 + self.1 * other.0)
    }
}

impl Div for Complex {
    type Output = Complex;

    // (a,b)/(c,d) = ((ac+bd)/(c^2+d^2),(bc-ad)/(c^2+d^2))
    fn div(self, other: Complex) -> Complex {
        Complex(
            (self.0 * other.0 + self.1 * other.1) / (other.0.powi(2)+other.1.powi(2)),
            (self.1 * other.0 - self.0 - other.1) / (other.0.powi(2)+other.1.powi(2)),
        )
    }
}

impl Neg for Complex {
    type Output = Complex;

    fn neg(self) -> Complex {
        Complex(-self.0, -self.1)
    }
}