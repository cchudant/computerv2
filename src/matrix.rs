use std::fmt;
use std::ops::{Add, Sub, Mul, Div, Neg, Index, IndexMut};
use std::convert::TryFrom;

use crate::rational::Rational;

#[derive(Debug)]
pub struct SizeMismatchError;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Matrix {
    rows: usize,
    cols: usize,
    values: Vec<Rational>,
}

impl Matrix {
    fn dim(&self) -> (usize, usize) {
        (self.rows, self.cols)
    }
}

impl fmt::Display for Matrix {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[")?;
        if self.cols == 1 {
            for (i, el) in self.values.iter().enumerate() {
                write!(f, "{}", el)?;
                if i != self.rows - 1 {
                    write!(f, "; ")?;
                }
            }
        } else if self.rows == 1 {
            write!(f, "[")?;
            for (i, el) in self.values.iter().enumerate() {
                write!(f, "{}", el)?;
                if i != self.rows - 1 {
                    write!(f, ", ")?;
                }
            }
            write!(f, "]")?
        } else {
            for c in 0..self.cols {
                write!(f, "[")?;
                for r in 0..self.rows {
                    write!(f, "{}", self[(r, c)])?;
                    if r != self.rows - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "]")?;
                if c != self.rows - 1 {
                    write!(f, "; ")?;
                }
            }
        }
        write!(f, "]")?;
        Ok(())
    }
}

impl Index<(usize, usize)> for Matrix {
    type Output = Rational;

    fn index(&self, (r, c): (usize, usize)) -> &Self::Output {
        &self.values[c * self.rows + r]
    }
}

impl IndexMut<(usize, usize)> for Matrix {
    fn index_mut(&mut self, (r, c): (usize, usize)) -> &mut Self::Output {
        &mut self.values[c * self.rows + r]
    }
}

impl TryFrom<Vec<Vec<Rational>>> for Matrix {
    type Error = SizeMismatchError;

    fn try_from(value: Vec<Vec<Rational>>) -> Result<Self, Self::Error> {
        if value.len() == 0 {
            return Ok(Matrix { rows: 0, cols: 0, values: vec![] });
        }
        let cols = value.len();
        let rows = value[0].len();
        let mut ctnr = vec![];
        for mut row in value {
            if row.len() != rows {
                return Err(SizeMismatchError);
            }
            ctnr.append(&mut row);
        }
        Ok(Matrix { rows, cols, values: ctnr })
    }
}

impl Sub for &Matrix {
    type Output = Matrix;

    fn sub(self, other: &Matrix) -> Matrix {
        if self.dim() != other.dim() {
            panic!("Cannot substract matrices of different dimensions");
        }
        Matrix {
            rows: self.rows,
            cols: self.cols,
            values: self.values.iter()
                .zip(&other.values)
                .map(|(el, oth)| *el - *oth)
                .collect(),
        }
    }
}

impl Add for &Matrix {
    type Output = Matrix;

    fn add(self, other: &Matrix) -> Matrix {
        if self.dim() != other.dim() {
            panic!("Cannot add matrices of different dimensions");
        }
        Matrix {
            rows: self.rows,
            cols: self.cols,
            values: self.values.iter()
                .zip(&other.values)
                .map(|(el, oth)| *el + *oth)
                .collect(),
        }
    }
}

impl Mul for &Matrix {
    type Output = Matrix;

    fn mul(self, other: &Matrix) -> Matrix {
        if self.dim() != other.dim() {
            panic!("Cannot element-wise multiply matrices of different dimensions");
        }
        Matrix {
            rows: self.rows,
            cols: self.cols,
            values: self.values.iter()
                .zip(&other.values)
                .map(|(el, oth)| *el * *oth)
                .collect(),
        }
    }
}

impl Div for &Matrix {
    type Output = Matrix;

    fn div(self, other: &Matrix) -> Matrix {
        if self.dim() != other.dim() {
            panic!("Cannot divide matrices of different dimensions");
        }
        Matrix {
            rows: self.rows,
            cols: self.cols,
            values: self.values.iter()
                .zip(&other.values)
                .map(|(el, oth)| *el / *oth)
                .collect(),
        }
    }
}

impl Neg for &Matrix {
    type Output = Matrix;

    fn neg(self) -> Matrix {
        Matrix {
            rows: self.rows,
            cols: self.cols,
            values: self.values.iter()
                .map(|el| -*el)
                .collect(),
        }
    }
}
