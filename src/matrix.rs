use std::fmt;
use std::ops::{Index, IndexMut};

use crate::complex::*;
use crate::ops::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Matrix {
    rows: usize,
    cols: usize,
    values: Vec<Complex>,
}

impl Matrix {
    pub fn new(rows: usize, cols: usize, values: Vec<Complex>) -> Matrix {
        assert_eq!(values.len(), rows * cols);
        Matrix { rows, cols, values }
    }

    pub fn dim(&self) -> (usize, usize) {
        (self.rows, self.cols)
    }

    pub fn empty() -> Matrix {
        Matrix {
            rows: 0,
            cols: 0,
            values: vec![],
        }
    }

    pub fn zeros(rows: usize, cols: usize) -> Matrix {
        Matrix {
            rows,
            cols,
            values: vec![Complex::ZERO; rows * cols],
        }
    }
}

impl fmt::Display for Matrix {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.rows != 1 || self.cols == 1 {
            write!(f, "[")?;
        }
        for r in 0..self.rows {
            if self.cols != 1 {
                write!(f, "[")?;
            }
            for c in 0..self.cols {
                write!(f, "{}", &self[(r, c)])?;
                if c != self.cols - 1 {
                    write!(f, ", ")?;
                }
            }
            if self.cols != 1 {
                write!(f, "]")?;
            }
            if r != self.rows - 1 {
                write!(f, "; ")?;
            }
        }
        if self.rows != 1 || self.cols == 1 {
            write!(f, "]")?;
        }
        Ok(())
    }
}

impl Index<(usize, usize)> for Matrix {
    type Output = Complex;

    fn index(&self, (r, c): (usize, usize)) -> &Self::Output {
        assert!(r < self.rows);
        assert!(c < self.cols);
        &self.values[r * self.cols + c]
    }
}

impl IndexMut<(usize, usize)> for Matrix {
    fn index_mut(&mut self, (r, c): (usize, usize)) -> &mut Self::Output {
        assert!(r < self.rows);
        assert!(c < self.cols);
        &mut self.values[r * self.cols + c]
    }
}

impl TrySub for &Matrix {
    type Output = Matrix;

    fn try_sub(self, other: &Matrix) -> Result<Self::Output, CalcError> {
        if self.dim() != other.dim() {
            Err(CalcError {
                kind: CalcErrorKind::DimensionMismatch,
                op: "-",
                arg1: self.clone().into(),
                arg2: other.clone().into(),
            })?;
        }
        Ok(Matrix {
            rows: self.rows,
            cols: self.cols,
            values: self
                .values
                .iter()
                .zip(&other.values)
                .map(|(el, oth)| *el - *oth)
                .collect(),
        })
    }
}

impl Sub<Complex> for &Matrix {
    type Output = Matrix;

    fn sub(self, other: Complex) -> Matrix {
        Matrix {
            rows: self.rows,
            cols: self.cols,
            values: self.values.iter().map(|el| *el - other).collect(),
        }
    }
}

impl Sub<&Matrix> for Complex {
    type Output = Matrix;

    fn sub(self, other: &Matrix) -> Matrix {
        Matrix {
            rows: other.rows,
            cols: other.cols,
            values: other.values.iter().map(|el| self - *el).collect(),
        }
    }
}

impl TryAdd for &Matrix {
    type Output = Matrix;

    fn try_add(self, other: &Matrix) -> Result<Self::Output, CalcError> {
        if self.dim() != other.dim() {
            Err(CalcError {
                kind: CalcErrorKind::DimensionMismatch,
                op: "+",
                arg1: self.clone().into(),
                arg2: other.clone().into(),
            })?;
        }
        Ok(Matrix {
            rows: self.rows,
            cols: self.cols,
            values: self
                .values
                .iter()
                .zip(&other.values)
                .map(|(el, oth)| *el + *oth)
                .collect(),
        })
    }
}

impl Add<Complex> for &Matrix {
    type Output = Matrix;

    fn add(self, other: Complex) -> Matrix {
        Matrix {
            rows: self.rows,
            cols: self.cols,
            values: self.values.iter().map(|el| *el + other).collect(),
        }
    }
}

impl Add<&Matrix> for Complex {
    type Output = Matrix;

    fn add(self, other: &Matrix) -> Matrix {
        Matrix {
            rows: other.rows,
            cols: other.cols,
            values: other.values.iter().map(|el| self + *el).collect(),
        }
    }
}

impl TryMul for &Matrix {
    type Output = Matrix;

    fn try_mul(self, other: &Matrix) -> Result<Self::Output, CalcError> {
        if self.dim() != other.dim() {
            Err(CalcError {
                kind: CalcErrorKind::DimensionMismatch,
                op: "*",
                arg1: self.clone().into(),
                arg2: other.clone().into(),
            })?;
        }
        Ok(Matrix {
            rows: self.rows,
            cols: self.cols,
            values: self
                .values
                .iter()
                .zip(&other.values)
                .map(|(el, oth)| *el * *oth)
                .collect(),
        })
    }
}

impl Mul<Complex> for &Matrix {
    type Output = Matrix;

    fn mul(self, other: Complex) -> Matrix {
        Matrix {
            rows: self.rows,
            cols: self.cols,
            values: self.values.iter().map(|el| *el * other).collect(),
        }
    }
}

impl Mul<&Matrix> for Complex {
    type Output = Matrix;

    fn mul(self, other: &Matrix) -> Matrix {
        Matrix {
            rows: other.rows,
            cols: other.cols,
            values: other.values.iter().map(|el| self * *el).collect(),
        }
    }
}

impl TryDiv for &Matrix {
    type Output = Matrix;

    fn try_div(self, other: &Matrix) -> Result<Self::Output, CalcError> {
        if self.dim() != other.dim() {
            Err(CalcError {
                kind: CalcErrorKind::DimensionMismatch,
                op: "/",
                arg1: self.clone().into(),
                arg2: other.clone().into(),
            })?;
        }
        Ok(Matrix {
            rows: self.rows,
            cols: self.cols,
            values: self
                .values
                .iter()
                .zip(&other.values)
                .map(|(el, oth)| el.try_div(*oth))
                .collect::<Result<Vec<Complex>, CalcError>>()?,
        })
    }
}

impl TryDiv<Complex> for &Matrix {
    type Output = Matrix;

    fn try_div(self, other: Complex) -> Result<Self::Output, CalcError> {
        Ok(Matrix {
            rows: self.rows,
            cols: self.cols,
            values: self
                .values
                .iter()
                .map(|el| el.try_div(other))
                .collect::<Result<Vec<Complex>, CalcError>>()?,
        })
    }
}

impl TryDiv<&Matrix> for Complex {
    type Output = Matrix;

    fn try_div(self, other: &Matrix) -> Result<Self::Output, CalcError> {
        Ok(Matrix {
            rows: other.rows,
            cols: other.cols,
            values: other
                .values
                .iter()
                .map(|el| self.try_div(*el))
                .collect::<Result<Vec<Complex>, CalcError>>()?,
        })
    }
}

impl TryRem for &Matrix {
    type Output = Matrix;

    fn try_rem(self, other: &Matrix) -> Result<Self::Output, CalcError> {
        if self.dim() != other.dim() {
            Err(CalcError {
                kind: CalcErrorKind::DimensionMismatch,
                op: "%",
                arg1: self.clone().into(),
                arg2: other.clone().into(),
            })?;
        }
        Ok(Matrix {
            rows: self.rows,
            cols: self.cols,
            values: self
                .values
                .iter()
                .zip(&other.values)
                .map(|(el, oth)| el.try_rem(*oth))
                .collect::<Result<Vec<Complex>, CalcError>>()?,
        })
    }
}

impl TryRem<Complex> for &Matrix {
    type Output = Matrix;

    fn try_rem(self, other: Complex) -> Result<Self::Output, CalcError> {
        Ok(Matrix {
            rows: self.rows,
            cols: self.cols,
            values: self
                .values
                .iter()
                .map(|el| other.try_rem(*el))
                .collect::<Result<Vec<Complex>, CalcError>>()?,
        })
    }
}

impl TryRem<&Matrix> for Complex {
    type Output = Matrix;

    fn try_rem(self, other: &Matrix) -> Result<Self::Output, CalcError> {
        Ok(Matrix {
            rows: other.rows,
            cols: other.cols,
            values: other
                .values
                .iter()
                .map(|el| self.try_rem(*el))
                .collect::<Result<Vec<Complex>, CalcError>>()?,
        })
    }
}

impl Neg for &Matrix {
    type Output = Matrix;

    fn neg(self) -> Matrix {
        Matrix {
            rows: self.rows,
            cols: self.cols,
            values: self.values.iter().map(|el| -*el).collect(),
        }
    }
}

impl TryMatMul<&Matrix> for &Matrix {
    type Output = Matrix;

    fn try_mat_mul(self, other: &Matrix) -> Result<Self::Output, CalcError> {
        println!("hi row={} col={}", self.cols, self.rows);
        println!("ho row={} col={}", other.cols, other.rows);
        if self.cols != other.rows {
            Err(CalcError {
                kind: CalcErrorKind::DimensionMismatch,
                op: "**",
                arg1: self.clone().into(),
                arg2: other.clone().into(),
            })?;
        }

        let mut res = Matrix::zeros(self.rows, other.cols);
        println!("res row={} col={}", res.rows, res.cols);

        for c in 0..other.cols {
            for r in 0..self.rows {
                for i in 0..self.cols {
                    println!("res[({},{})] += lhs[({},{})] * rhs[({},{})]", r, c, r, i, i, c);
                    println!("res[({},{})] += {} * {}", r, c, self[(r, i)], other[(i, c)]);
                    res[(r, c)] = res[(r, c)] + self[(r, i)] * other[(i, c)];
                }
            }
        }

        Ok(res)
    }
}
