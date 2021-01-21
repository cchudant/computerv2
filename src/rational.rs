use std::cmp::Ordering;
use std::fmt;
use std::num::ParseIntError;
use std::str::FromStr;

use crate::ops::*;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Rational {
    num: i64,
    den: i64,
}

impl Rational {
    pub fn new(mut num: i64, mut den: i64) -> Rational {
        if num == 0 {
            return Rational::ZERO;
        }

        let negative = (num > 0 && den < 0) || (num < 0 && den > 0);
        num = num.abs();
        den = den.abs();

        let mul = if negative { -1 } else { 1 };

        let gcd_val = crate::util::gcd(num, den);
        Rational {
            num: mul * num / gcd_val,
            den: den / gcd_val,
        }
    }

    pub fn inv(self) -> Rational {
        let negative = (self.den > 0 && self.num < 0) || (self.den < 0 && self.num > 0);
        let mul = if negative { -1 } else { 1 };

        Rational {
            num: mul * self.den.abs(),
            den: self.num.abs(),
        }
    }

    pub const ZERO: Rational = Rational { num: 0, den: 1 };
    pub const ONE: Rational = Rational { num: 1, den: 1 };

    pub fn abs(self) -> Rational {
        Rational {
            num: self.num.abs(),
            den: self.den.abs(),
        }
    }

    pub fn checked_pow(self, i: u32) -> Option<Rational> {
        Some(Rational {
            num: self.num.checked_pow(i)?,
            den: self.den.checked_pow(i)?,
        })
    }

    pub fn get_num(&self) -> i64 {
        self.num
    }
    pub fn get_den(&self) -> i64 {
        self.den
    }

    pub fn is_whole(&self) -> bool {
        return self.den == 1
    }
    pub fn is_positive_nil(&self) -> bool {
        return self.num >= 0
    }
}

impl Default for Rational {
    fn default() -> Rational {
        Rational { num: 0, den: 1 }
    }
}

impl fmt::Display for Rational {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.den == 1 {
            write!(f, "{}", self.num)?;
        } else {
            write!(f, "{}", self.num as f64 / self.den as f64)?;
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
        Rational { num: val, den: 1 }
    }
}

impl Into<f64> for Rational {
    fn into(self) -> f64 {
        self.num as f64 / self.den as f64
    }
}

impl Ord for Rational {
    fn cmp(&self, other: &Self) -> Ordering {
        let same_den_self = self.num * other.den;
        let same_den_other = other.num * self.den;
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
        let same_den = self.den * other.den;
        let same_den_self = self.num * other.den;
        let same_den_other = other.num * self.den;
        Rational {
            num: same_den_self - same_den_other,
            den: same_den,
        }
    }
}

impl Add for Rational {
    type Output = Rational;

    fn add(self, other: Rational) -> Rational {
        let same_den = self.den * other.den;
        let same_den_self = self.num * other.den;
        let same_den_other = other.num * self.den;
        Rational {
            num: same_den_self + same_den_other,
            den: same_den,
        }
    }
}

impl Mul for Rational {
    type Output = Rational;

    fn mul(self, other: Rational) -> Rational {
        Rational::new(self.num * other.num, self.den * other.den)
    }
}

impl Div for Rational {
    type Output = Rational;

    // (a/b) / (c/d) = (a/b) * (d/c)
    fn div(self, other: Rational) -> Rational {
        self * other.inv()
    }
}

impl Rem for Rational {
    type Output = Rational;

    // (a/b) % (c/b) = (a % c)/b
    fn rem(self, other: Rational) -> Rational {
        let same_den = self.den * other.den;
        let same_den_self = self.num * other.den;
        let same_den_other = other.num * self.den;
        Rational {
            num: same_den_self % same_den_other,
            den: same_den,
        }
    }
}

impl Neg for Rational {
    type Output = Rational;

    fn neg(self) -> Rational {
        Rational {
            num: -self.num,
            den: self.den,
        }
    }
}
