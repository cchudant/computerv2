use std::fmt;
pub use std::ops::{Add, Div, Mul, Neg, Rem, Sub};

use crate::value::*;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum CalcErrorKind {
    TypeError,
    ComplexNotAllowed,
    DivByZero,
    ExpPositiveInt,
    DimensionMismatch,
    OverflowUnderflow,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CalcError {
    pub kind: CalcErrorKind,
    pub op: &'static str,
    pub arg1: EvalValue,
    pub arg2: EvalValue,
}

impl fmt::Display for CalcError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.kind {
            CalcErrorKind::TypeError => write!(
                f,
                "operator `{}` cannot be called on types `{}` and `{}`",
                self.op, self.arg1.type_str(), self.arg2.type_str()
            ),
            CalcErrorKind::ComplexNotAllowed => write!(
                f,
                "operator `{}` cannot be called on complex values: `{}` {0} `{}`",
                self.op, self.arg1, self.arg2
            ),
            CalcErrorKind::DivByZero => write!(
                f,
                "operator `{}` division by zero: `{}` {0} `{}`",
                self.op, self.arg1, self.arg2
            ),
            CalcErrorKind::ExpPositiveInt => write!(
                f,
                "operator `{}` can only be called with positive ints: `{}` {0} `{}`",
                self.op, self.arg1, self.arg2
            ),
            CalcErrorKind::DimensionMismatch => write!(
                f,
                "matrix dimension mismatch: `{1}` {0} `{2}`",
                self.op, self.arg1, self.arg2
            ),
            CalcErrorKind::OverflowUnderflow => write!(
                f,
                "overflow or underflow: `{1}` {0} `{2}`",
                self.op, self.arg1, self.arg2
            ),
        }
    }
}

// More operators

pub trait MatMul<Rhs = Self> {
    type Output;

    fn mat_mul(self, other: Rhs) -> Self::Output;
}

pub trait Pow<Rhs = Self> {
    type Output;

    fn pow(self, other: Rhs) -> Self::Output;
}

// Failible traits

pub trait TryAdd<Rhs = Self> {
    type Output;

    fn try_add(self, other: Rhs) -> Result<Self::Output, CalcError>;
}

pub trait TrySub<Rhs = Self> {
    type Output;

    fn try_sub(self, other: Rhs) -> Result<Self::Output, CalcError>;
}

pub trait TryMul<Rhs = Self> {
    type Output;

    fn try_mul(self, other: Rhs) -> Result<Self::Output, CalcError>;
}

pub trait TryDiv<Rhs = Self> {
    type Output;

    fn try_div(self, other: Rhs) -> Result<Self::Output, CalcError>;
}

pub trait TryRem<Rhs = Self> {
    type Output;

    fn try_rem(self, other: Rhs) -> Result<Self::Output, CalcError>;
}

pub trait TryNeg {
    type Output;

    fn try_neg(self) -> Result<Self::Output, CalcError>;
}

pub trait TryPow<Rhs = Self> {
    type Output;

    fn try_pow(self, other: Rhs) -> Result<Self::Output, CalcError>;
}

pub trait TryMatMul<Rhs = Self> {
    type Output;

    fn try_mat_mul(self, other: Rhs) -> Result<Self::Output, CalcError>;
}

// Blanket faillible impls

impl<T, Rhs> TryAdd<Rhs> for T
where
    T: Add<Rhs>,
{
    type Output = T::Output;

    fn try_add(self, other: Rhs) -> Result<Self::Output, CalcError> {
        Ok(self.add(other))
    }
}

impl<T, Rhs> TrySub<Rhs> for T
where
    T: Sub<Rhs>,
{
    type Output = T::Output;

    fn try_sub(self, other: Rhs) -> Result<Self::Output, CalcError> {
        Ok(self.sub(other))
    }
}

impl<T, Rhs> TryMul<Rhs> for T
where
    T: Mul<Rhs>,
{
    type Output = T::Output;

    fn try_mul(self, other: Rhs) -> Result<Self::Output, CalcError> {
        Ok(self.mul(other))
    }
}

impl<T, Rhs> TryDiv<Rhs> for T
where
    T: Div<Rhs>,
{
    type Output = T::Output;

    fn try_div(self, other: Rhs) -> Result<Self::Output, CalcError> {
        Ok(self.div(other))
    }
}

impl<T, Rhs> TryRem<Rhs> for T
where
    T: Rem<Rhs>,
{
    type Output = T::Output;

    fn try_rem(self, other: Rhs) -> Result<Self::Output, CalcError> {
        Ok(self.rem(other))
    }
}

impl<T> TryNeg for T
where
    T: Neg,
{
    type Output = T::Output;

    fn try_neg(self) -> Result<Self::Output, CalcError> {
        Ok(self.neg())
    }
}

impl<T, Rhs> TryPow<Rhs> for T
where
    T: Pow<Rhs>,
{
    type Output = T::Output;

    fn try_pow(self, other: Rhs) -> Result<Self::Output, CalcError> {
        Ok(self.pow(other))
    }
}

impl<T, Rhs> TryMatMul<Rhs> for T
where
    T: MatMul<Rhs>,
{
    type Output = T::Output;

    fn try_mat_mul(self, other: Rhs) -> Result<Self::Output, CalcError> {
        Ok(self.mat_mul(other))
    }
}
