use std::fmt;
use std::str::FromStr;
use std::num::ParseIntError;
use std::ops::{Add, Sub, Mul, Div, Neg};
use std::cmp::Ordering;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Rational(i64, i64);

impl Rational {
    pub fn new(mut num: i64, mut den: i64) -> Rational {
        if num == 0 { return Rational::ZERO; }

        let negative = (num > 0 && den < 0) || (num < 0 && den > 0);
        num = num.abs();
        den = den.abs();

        let mul = if negative { -1 } else { 1 };

        let gcd_val = crate::util::gcd(num, den);
        Rational(mul * num / gcd_val, den / gcd_val)
    }

    pub fn inv(self) -> Rational {
        let negative = (self.1 > 0 && self.0 < 0) || (self.1 < 0 && self.0 > 0);
        let mul = if negative { -1 } else { 1 };
        
        Rational(mul * self.1.abs(), self.0.abs())
    }

    pub const ZERO: Rational = Rational(0, 1);

    pub fn abs(self) -> Rational {
        Rational(self.0.abs(), self.1.abs())
    }

    pub fn powi(self, i: u32) -> Rational {
        Rational(self.0.pow(i), self.1.pow(i))
    }
}

impl Default for Rational {
    fn default() -> Rational {
        Rational(0, 1)
    }
}

impl fmt::Display for Rational {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.1 == 1 {
            write!(f, "{}", self.0)?;
        } else {
            write!(f, "{}/{}", self.0, self.1)?;
        }
        Ok(())
    }
}

impl FromStr for Rational {
    type Err = ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut coords = s.split('.');

        let mut num;
        let mut den = 1i64;
        
        let int_part = coords.next().unwrap().parse::<i64>()?;
        num = int_part;
        if let Some(val) = coords.next() {
            let mut float_part = val;

            while float_part.len() > 0 {
                num = num * 10 + float_part[0..1].parse::<i64>()? % 10;
                float_part = &float_part[1..];
                den *= 10;
            }
        }

        Ok(Rational::new(num, den))
    }
}

impl From<i64> for Rational {
    fn from(val: i64) -> Rational {
        Rational(val, 1)
    }
}

impl Ord for Rational {
    fn cmp(&self, other: &Self) -> Ordering {
        let same_den_self = self.0 * other.1;
        let same_den_other = other.0 * self.1;
        same_den_self.cmp(&same_den_other)
    }
}

impl PartialOrd for Rational {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Sub for Rational {
    type Output = Rational;

    fn sub(self, other: Rational) -> Rational {
        let same_den = self.1 * other.1;
        let same_den_self = self.0 * other.1;
        let same_den_other = other.0 * self.1;
        Rational(same_den_self - same_den_other, same_den)
    }
}

impl Add for Rational {
    type Output = Rational;

    fn add(self, other: Rational) -> Rational {
        let same_den = self.1 * other.1;
        let same_den_self = self.0 * other.1;
        let same_den_other = other.0 * self.1;
        Rational(same_den_self + same_den_other, same_den)
    }
}

impl Mul for Rational {
    type Output = Rational;

    fn mul(self, other: Rational) -> Rational {
        Rational(self.0 * other.0, self.1 * other.1)
    }
}

impl Div for Rational {
    type Output = Rational;

    // (a/b) / (c/d) = (a/b) * (d/c)
    fn div(self, other: Rational) -> Rational {
        self * other.inv()
    }
}

impl Neg for Rational {
    type Output = Rational;

    fn neg(self) -> Rational {
        Rational(-self.0, self.1)
    }
}
