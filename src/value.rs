use std::fmt;

use crate::complex::*;
use crate::eval::*;
use crate::matrix::*;
use crate::ops::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvalValue {
    Complex(Complex),
    Matrix(Matrix),
    Function {
        name: String,
        arg_name: String,
        body: Box<ASTNode>,
    },
    UnknownSolve(Box<ASTNode>),
}

impl EvalValue {
    pub fn type_str(&self) -> &'static str {
        match self {
            EvalValue::Complex(..) => "number",
            EvalValue::Matrix(..) => "matrix",
            EvalValue::Function { .. } => "function",
            EvalValue::UnknownSolve(..) => "unknown",
        }
    }

    pub fn display_with_name(&self, name: &str) {
        if let EvalValue::Function { arg_name, body, .. } = self {
            println!("{}({}) = {}", name, arg_name, body);
        } else {
            println!("{} = {}", name, self);
        }
    }
}

impl From<Complex> for EvalValue {
    fn from(value: Complex) -> EvalValue {
        EvalValue::Complex(value)
    }
}

impl From<Matrix> for EvalValue {
    fn from(value: Matrix) -> EvalValue {
        EvalValue::Matrix(value)
    }
}

impl fmt::Display for EvalValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            EvalValue::Complex(c) => write!(f, "{}", c),
            EvalValue::Matrix(m) => write!(f, "{}", m),
            EvalValue::Function {
                name,
                arg_name,
                body,
            } => write!(f, "{}({}) = {}", name, arg_name, body),
            EvalValue::UnknownSolve(v) => write!(f, "{}", v),
        }
    }
}

impl TryAdd for EvalValue {
    type Output = Self;
    fn try_add(self, other: Self) -> Result<Self::Output, CalcError> {
        Ok(match (self, other) {
            (EvalValue::Complex(v1), EvalValue::Complex(v2)) => v1.try_add(v2)?.into(),
            (EvalValue::Complex(v1), EvalValue::Matrix(ref v2)) => v1.try_add(v2)?.into(),
            (EvalValue::Matrix(ref v1), EvalValue::Complex(v2)) => v1.try_add(v2)?.into(),
            (EvalValue::Matrix(ref v1), EvalValue::Matrix(ref v2)) => v1.try_add(v2)?.into(),
            (arg1, arg2) => Err(CalcError {
                kind: CalcErrorKind::TypeError,
                op: "+",
                arg1,
                arg2,
            })?,
        })
    }
}

impl TrySub for EvalValue {
    type Output = Self;
    fn try_sub(self, other: Self) -> Result<Self::Output, CalcError> {
        Ok(match (self, other) {
            (EvalValue::Complex(v1), EvalValue::Complex(v2)) => v1.try_sub(v2)?.into(),
            (EvalValue::Complex(v1), EvalValue::Matrix(ref v2)) => v1.try_sub(v2)?.into(),
            (EvalValue::Matrix(ref v1), EvalValue::Complex(v2)) => v1.try_sub(v2)?.into(),
            (EvalValue::Matrix(ref v1), EvalValue::Matrix(ref v2)) => v1.try_sub(v2)?.into(),
            (arg1, arg2) => Err(CalcError {
                kind: CalcErrorKind::TypeError,
                op: "-",
                arg1,
                arg2,
            })?,
        })
    }
}

impl TryMul for EvalValue {
    type Output = Self;
    fn try_mul(self, other: Self) -> Result<Self::Output, CalcError> {
        Ok(match (self, other) {
            (EvalValue::Complex(v1), EvalValue::Complex(v2)) => v1.try_mul(v2)?.into(),
            (EvalValue::Complex(v1), EvalValue::Matrix(ref v2)) => v1.try_mul(v2)?.into(),
            (EvalValue::Matrix(ref v1), EvalValue::Complex(v2)) => v1.try_mul(v2)?.into(),
            (EvalValue::Matrix(ref v1), EvalValue::Matrix(ref v2)) => v1.try_mul(v2)?.into(),
            (arg1, arg2) => Err(CalcError {
                kind: CalcErrorKind::TypeError,
                op: "*",
                arg1,
                arg2,
            })?,
        })
    }
}

impl TryDiv for EvalValue {
    type Output = Self;
    fn try_div(self, other: Self) -> Result<Self::Output, CalcError> {
        Ok(match (self, other) {
            (EvalValue::Complex(v1), EvalValue::Complex(v2)) => v1.try_div(v2)?.into(),
            (EvalValue::Complex(v1), EvalValue::Matrix(ref v2)) => v1.try_div(v2)?.into(),
            (EvalValue::Matrix(ref v1), EvalValue::Complex(v2)) => v1.try_div(v2)?.into(),
            (EvalValue::Matrix(ref v1), EvalValue::Matrix(ref v2)) => v1.try_div(v2)?.into(),
            (arg1, arg2) => Err(CalcError {
                kind: CalcErrorKind::TypeError,
                op: "/",
                arg1,
                arg2,
            })?,
        })
    }
}

impl TryRem for EvalValue {
    type Output = Self;
    fn try_rem(self, other: Self) -> Result<Self::Output, CalcError> {
        Ok(match (self, other) {
            (EvalValue::Complex(v1), EvalValue::Complex(v2)) => v1.try_rem(v2)?.into(),
            (EvalValue::Complex(v1), EvalValue::Matrix(ref v2)) => v1.try_rem(v2)?.into(),
            (EvalValue::Matrix(ref v1), EvalValue::Complex(v2)) => v1.try_rem(v2)?.into(),
            (EvalValue::Matrix(ref v1), EvalValue::Matrix(ref v2)) => v1.try_rem(v2)?.into(),
            (arg1, arg2) => Err(CalcError {
                kind: CalcErrorKind::TypeError,
                op: "%",
                arg1,
                arg2,
            })?,
        })
    }
}

impl TryNeg for EvalValue {
    type Output = Self;
    fn try_neg(self) -> Result<Self::Output, CalcError> {
        Ok(match self {
            EvalValue::Complex(v) => v.try_neg()?.into(),
            EvalValue::Matrix(ref v) => v.try_neg()?.into(),
            arg1 => Err(CalcError {
                kind: CalcErrorKind::TypeError,
                op: "unary -",
                arg2: arg1.clone(),
                arg1,
            })?,
        })
    }
}

impl TryPow for EvalValue {
    type Output = Self;
    fn try_pow(self, other: Self) -> Result<Self::Output, CalcError> {
        Ok(match (self, other) {
            (EvalValue::Complex(v1), EvalValue::Complex(v2)) => v1.try_pow(v2)?.into(),
            (arg1, arg2) => Err(CalcError {
                kind: CalcErrorKind::TypeError,
                op: "^",
                arg1,
                arg2,
            })?,
        })
    }
}

impl TryMatMul for EvalValue {
    type Output = Self;
    fn try_mat_mul(self, other: Self) -> Result<Self::Output, CalcError> {
        Ok(match (self, other) {
            (EvalValue::Matrix(ref v1), EvalValue::Matrix(ref v2)) => v1.try_mat_mul(v2)?.into(),
            (arg1, arg2) => Err(CalcError {
                kind: CalcErrorKind::TypeError,
                op: "**",
                arg1,
                arg2,
            })?,
        })
    }
}
