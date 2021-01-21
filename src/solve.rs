
use std::fmt;
use crate::complex::*;
use crate::rational::*;
use crate::eval::*;
use crate::ops::*;

#[derive(Default, Debug, Clone, Copy, PartialEq)]
struct ComplexF64(f64, f64);

impl ComplexF64 {
    fn real(r: f64) -> ComplexF64 {
        ComplexF64(r, 0.0)
    }
}

impl fmt::Display for ComplexF64 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.0 == 0.0 && self.1 == 0.0 {
            write!(f, "0")?;
        } else if self.0 == 0.0 {
            write!(f, "{}i", self.1)?;
        } else if self.1 == 0.0 {
            write!(f, "{}", self.0)?;
        } else {
            write!(f, "{} ", self.0)?;
            if self.1 < 0.0 {
                write!(f, "- ")?;
            } else {
                write!(f, "+ ")?;
            }
            write!(f, "{}i", self.1.abs())?;
        }

        Ok(())
    }
}

impl From<(f64, f64)> for ComplexF64 {
    fn from((r, i): (f64, f64)) -> ComplexF64 {
        ComplexF64(r, i)
    }
}

impl From<Complex> for ComplexF64 {
    fn from(o: Complex) -> ComplexF64 {
        ComplexF64(o.r.into(), o.i.into())
    }
}

impl Sub for ComplexF64 {
    type Output = ComplexF64;

    fn sub(self, other: ComplexF64) -> ComplexF64 {
        ComplexF64(self.0 - other.0, self.1 - other.1)
    }
}

impl Add for ComplexF64 {
    type Output = ComplexF64;

    fn add(self, other: ComplexF64) -> ComplexF64 {
        ComplexF64(self.0 + other.0, self.1 + other.1)
    }
}

impl Mul for ComplexF64 {
    type Output = ComplexF64;

    // (a+bi)(c+di) = ac + adi + bci - bd
    fn mul(self, other: ComplexF64) -> ComplexF64 {
        ComplexF64(self.0 * other.0 - self.1 * other.1, self.0 * other.1 + self.1 * other.0)
    }
}

impl Div for ComplexF64 {
    type Output = ComplexF64;

    // (a+bi)/(c+di) = (ac+bd)/(c^2+d^2) + (bc-ad)/(c^2+d^2)i
    fn div(self, other: ComplexF64) -> ComplexF64 {
        ComplexF64(
            (self.0 * other.0 + self.1 * other.1) / (other.0.powi(2)+other.1.powi(2)),
            (self.1 * other.0 - self.0 * other.1) / (other.0.powi(2)+other.1.powi(2)),
        )
    }
}

impl Neg for ComplexF64 {
    type Output = ComplexF64;

    fn neg(self) -> ComplexF64 {
        ComplexF64(-self.0, -self.1)
    }
}

#[derive(Debug, Clone)]
pub struct Term {
    a: Rational,
    p: i64,
}

#[derive(Debug, Clone)]
pub struct Equation {
    left: Vec<Term>,
    right: Vec<Term>,
    unknown_var: String,
}

impl fmt::Display for Equation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {

        fn fmt_one_side(f: &mut fmt::Formatter<'_>, side: &Vec<Term>, unknown_var: &str) -> fmt::Result {
            for (i, el) in side.iter().enumerate() {
                let mut a = el.a;
                if i > 0 {
                    if a < 0.into() {
                        write!(f, " - ")?;
                        a = a * Rational::from(-1);
                    } else {
                        write!(f, " + ")?;
                    }
                }

                if el.p == 0 {
                    write!(f, "{}", a)?;
                } else if el.p == 1 {
                    write!(f, "{}*{}", a, unknown_var)?;
                } else {
                    write!(f, "{}*{}^{}", a, unknown_var, el.p)?;
                }
            }

            Ok(())
        }

        fmt_one_side(f, &self.left, &self.unknown_var[..])?;
        write!(f, " = ")?;
        fmt_one_side(f, &self.right, &self.unknown_var[..])?;

        Ok(())
    }
}

impl Equation {

    pub fn try_from_ast(lhs: &ASTNode, rhs: &ASTNode) -> Result<Equation, EvalError> {
        fn one_side(s: &ASTNode, terms: &mut Vec<Term>, neg: bool, unknown_var: &mut Option<String>) -> Result<(), EvalError> {
            match s {
                ASTNode::Add(v1, v2) => {
                    one_side(v1, terms, false, unknown_var)?;
                    one_side(v2, terms, false, unknown_var)?;
                }
                ASTNode::Sub(v1, v2) => {
                    one_side(v1, terms, false, unknown_var)?;
                    one_side(v2, terms, true, unknown_var)?;
                }
                ASTNode::Num(n) => {
                    if !n.is_real() { Err(EvalError::SolveValueType)? }

                    terms.push(Term { a: n.r, p: 0 })
                }
                ASTNode::Var(var) => {
                    if let Some(v) = unknown_var { if v != var { Err(EvalError::SolveValueType)? } }
                    else { *unknown_var = Some(var.clone()); }

                    terms.push(Term { a: 1.into(), p: 1 })
                }
                ASTNode::Mul(v1, v2) => match (v1.as_ref(), v2.as_ref()) {
                    (ASTNode::Num(n), ASTNode::Var(_)) => {
                        if !n.is_real() { Err(EvalError::SolveValueType)? }
    
                        let a = if neg { -n.r } else { n.r };
                        terms.push(Term { a, p: 1 })
                    }
                    (ASTNode::Var(var), ASTNode::Num(n)) => {
                        if !n.is_real() { Err(EvalError::SolveValueType)? }

                        if let Some(v) = unknown_var { if v != var { Err(EvalError::SolveValueType)? } }
                        else { *unknown_var = Some(var.clone()); }
    
                        let a = if neg { -n.r } else { n.r };
                        terms.push(Term { a, p: 1 })
                    }
                    (ASTNode::Num(n), ASTNode::Pow(n1, n2)) => match (n1.as_ref(), n2.as_ref()) {
                        (ASTNode::Var(var), ASTNode::Num(p)) => {
                            if !n.is_real() { Err(EvalError::SolveValueType)? }
                            if !p.is_real() || !p.r.is_whole() || !p.r.is_positive_nil() { Err(EvalError::SolveValueType)? }

                            if let Some(v) = unknown_var { if v != var { Err(EvalError::SolveValueType)? } }
                            else { *unknown_var = Some(var.clone()); }
        
                            let a = if neg { -n.r } else { n.r };
                            terms.push(Term { a, p: p.r.get_num() })
                        }
                        _ => Err(EvalError::SolveValueType)?,
                    }
                    (ASTNode::Pow(n1, n2), ASTNode::Num(n)) => match (n1.as_ref(), n2.as_ref()) {
                        (ASTNode::Var(var), ASTNode::Num(p)) => {
                            if !n.is_real() { Err(EvalError::SolveValueType)? }
                            if !p.is_real() || !p.r.is_whole() || !p.r.is_positive_nil() { Err(EvalError::SolveValueType)? }

                            if let Some(v) = unknown_var { if v != var { Err(EvalError::SolveValueType)? } }
                            else { *unknown_var = Some(var.clone()); }
        
                            let a = if neg { -n.r } else { n.r };
                            terms.push(Term { a, p: p.r.get_num() })
                        }
                        _ => Err(EvalError::SolveValueType)?,
                    }
                    _ => Err(EvalError::SolveValueType)?,
                }
                _ => Err(EvalError::SolveValueType)?,
            }

            Ok(())
        }

        let mut unknown_var = None;

        let mut eq = Equation { left: vec![], right: vec![], unknown_var: String::new() };
        one_side(lhs, &mut eq.left, false, &mut unknown_var)?;
        one_side(rhs, &mut eq.right, false, &mut unknown_var)?;

        if let Some(var) = unknown_var {
            eq.unknown_var = var;
        } else {
            return Err(EvalError::SolveValueType);
        }

        Ok(eq)
    }

    pub fn reduced(&self) -> Equation {
        let mut res = Equation { left: vec![], right: vec![], unknown_var: self.unknown_var.clone() };

        let highest_exp = self.left.iter()
            .chain(self.right.iter())
            .map(|e| e.p)
            .max()
            .unwrap();

        for p in (0..=highest_exp).rev() {
            let left_sum = self.left.iter()
                .filter(|e| e.p == p)
                .fold(Rational::ZERO, |acc, v| acc + v.a);
            let right_sum = self.right.iter()
                .filter(|e| e.p == p)
                .fold(Rational::ZERO, |acc, v| acc + v.a);

            let a = left_sum - right_sum;
            if a != Rational::ZERO {
                res.left.push(Term { a, p });
            }
        }

        if res.left.len() == 0 {
            res.left.push(Term { a: Rational::ZERO, p: 0 });
        }

        res.right.push(Term { a: Rational::ZERO, p: 0 });

        res
    }

    pub fn print_solutions(&self) -> Result<(), EvalError> {
        println!("Reduced form: {}", self);

        let highest_exp = self.left.iter()
            .chain(self.right.iter())
            .map(|e| e.p)
            .max()
            .unwrap();

        println!("Polynomial degree: {}", highest_exp);

        if highest_exp > 2 {
            println!("Cannot solve polynomial degree greater than 2");
            return Ok(());
        }

        let a = self.left.iter().find(|e| e.p == 2).map(|e| e.a).unwrap_or(Rational::ZERO);
        let b = self.left.iter().find(|e| e.p == 1).map(|e| e.a).unwrap_or(Rational::ZERO);
        let c = self.left.iter().find(|e| e.p == 0).map(|e| e.a).unwrap_or(Rational::ZERO);

        if a == Rational::ZERO && b == Rational::ZERO {
            // no x variable
            if c == Rational::ZERO {
                println!("{} ∈ Z", self.unknown_var);
            } else {
                println!("{} ∈ ∅", self.unknown_var);
            }
            return Ok(());
        }

        if a == Rational::ZERO {
            let x = -c / b;
            println!("{} ∈ {{ {} }}", self.unknown_var, x);
            return Ok(());
        }

        let f_a: f64 = a.into();
        let f_b: f64 = b.into();
        let f_c: f64 = c.into();

        // conversion to f64: the delta can be irrational
        let delta: f64 = (f_b.powi(2) - 4.0 * f_a * f_c).into();

        if delta < 0.0 {
            println!("Δ < 0, there are therefore 2 complex solutions");

            let delta_sqrt: ComplexF64 = (0.0, (-delta).sqrt()).into();

            let az = ComplexF64::real(f_a);
            let bz = ComplexF64::real(f_b);
            let _cz = ComplexF64::real(f_c);

            let x1 = (-bz - delta_sqrt) / (ComplexF64::real(2.0) * az);
            let x2 = (-bz + delta_sqrt) / (ComplexF64::real(2.0) * az);

            println!("{} ∈ {{ {}, {} }}", self.unknown_var, x1, x2);
        } else if delta == 0.0 {
            println!("Δ = 0, there is therefore 1 real solution");

            let x0 = -f_b / (2.0 * f_a);

            println!("{} ∈ {{ {} }}", self.unknown_var, x0);
        } else {
            println!("Δ > 0, there are therefore 2 real solutions");

            let x1: f64 = (-f_b - delta.sqrt()) / (2.0 * f_a);
            let x2: f64 = (-f_b + delta.sqrt()) / (2.0 * f_a);

            println!("{} ∈ {{ {}, {} }}", self.unknown_var, x1, x2);
        }

        Ok(())
    }

}