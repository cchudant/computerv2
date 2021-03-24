use std::collections::HashMap;
use std::fmt;

use crate::complex::*;
use crate::matrix::*;
use crate::ops::*;
use crate::value::*;
use crate::solve::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct State {
    stack: Vec<HashMap<String, EvalValue>>,
}

impl State {
    pub fn new() -> State {
        State {
            stack: vec![HashMap::new()],
        }
    }

    fn get(&self, s: &str) -> Option<&EvalValue> {
        self.stack
            .iter()
            .find_map(|frame| frame.get(&s.to_lowercase()))
    }

    fn get_mut(&mut self, s: &str) -> Option<&mut EvalValue> {
        self.stack
            .iter_mut()
            .find_map(|frame| frame.get_mut(&s.to_lowercase()))
    }

    fn set(&mut self, s: &str, val: EvalValue) {
        if let Some(v) = self.get_mut(&s.to_lowercase()) {
            *v = val;
        } else {
            let last = self.stack.last_mut().unwrap();
            last.insert(s.to_lowercase(), val);
        }
    }

    fn new_frame(&mut self) -> Result<(), EvalError> {
        if self.stack.len() > 50 {
            return Err(EvalError::StackOverflow);
        }
        self.stack.push(HashMap::new());
        Ok(())
    }

    fn pop_frame(&mut self) {
        self.stack.pop();
        assert!(self.stack.len() >= 1);
    }

    pub fn vars(&self) -> impl Iterator<Item=(&String, &EvalValue)> {
        self.stack.iter().flat_map(|map| map.iter())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvalError {
    VariableNotExist { name: String },
    TypeErrorCall { name: String },
    MissingValue { node: Box<ASTNode> },
    InvalidMatrixShape { node: Box<ASTNode> },
    ValueInvalidInMatrix { node: Box<ASTNode> },
    StackOverflow,
    SetIVar,
    SolveValueType,
    SolveOverflow,
    SolveVariable,
    Value(CalcError),
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            EvalError::VariableNotExist { name } => write!(f, "variable `{}` does not exist", name),
            EvalError::TypeErrorCall { name } => write!(f, "variable `{}` is not a function", name),
            EvalError::MissingValue { node } => {
                write!(f, "`{}` does not evaluate to a value", node)
            }
            EvalError::InvalidMatrixShape { node } => {
                write!(f, "matrix `{}` does not have a valid shape", node)
            }
            EvalError::ValueInvalidInMatrix { node } => {
                write!(f, "element `{}` cannot be in a matrix", node)
            }
            EvalError::StackOverflow => write!(f, "stack overflow"),
            EvalError::SetIVar => write!(f, "cannot set the `i` variable"),
            EvalError::SolveValueType => write!(f, "cannot solve this equation"),
            EvalError::SolveOverflow => write!(f, "overflow during solving"),
            EvalError::SolveVariable => write!(f, "cannot find the variable for which you want to solve this equation"),
            EvalError::Value(e) => write!(f, "{}", e),
        }
    }
}

impl From<CalcError> for EvalError {
    fn from(value: CalcError) -> Self {
        Self::Value(value)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ASTNode {
    Par(Box<ASTNode>),
    Var(String),
    Num(Complex),
    Add(Box<ASTNode>, Box<ASTNode>),
    Sub(Box<ASTNode>, Box<ASTNode>),
    Mul(Box<ASTNode>, Box<ASTNode>),
    Div(Box<ASTNode>, Box<ASTNode>),
    Mod(Box<ASTNode>, Box<ASTNode>),
    Pow(Box<ASTNode>, Box<ASTNode>),
    MatrixMul(Box<ASTNode>, Box<ASTNode>),
    Neg(Box<ASTNode>),
    Pos(Box<ASTNode>),
    Set(String, Box<ASTNode>),
    Find(Box<ASTNode>, Option<Box<ASTNode>>),
    Fun(String, Box<ASTNode>),
    FunSet(String, String, Box<ASTNode>),
    Matrix(Vec<Vec<Box<ASTNode>>>),
}

fn disp_matrix(mat: &Vec<Vec<Box<ASTNode>>>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let rows = mat.len();
    let cols = mat.first().map(|e| e.len()).unwrap_or(0);
    if rows != 1 || cols == 1 {
        write!(f, "[")?;
    }
    for r in 0..rows {
        if cols != 1 {
            write!(f, "[")?;
        }
        for c in 0..cols {
            if let Some(e) = mat[r].get(c) {
                write!(f, "{}", e)?;
            } else {
                write!(f, "!")?;
            }
            if c != cols - 1 {
                write!(f, ", ")?;
            }
        }
        if cols != 1 {
            write!(f, "]")?;
        }
        if r != rows - 1 {
            write!(f, "; ")?;
        }
    }
    if rows != 1 || cols == 1 {
        write!(f, "]")?;
    }
    Ok(())
}

impl fmt::Display for ASTNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ASTNode::Par(v1) => write!(f, "({})", v1),
            ASTNode::Var(v1) => write!(f, "{}", v1),
            ASTNode::Num(v1) => write!(f, "{}", v1),
            ASTNode::Add(v1, v2) => write!(f, "{} + {}", v1, v2),
            ASTNode::Sub(v1, v2) => write!(f, "{} - {}", v1, v2),
            ASTNode::Mul(v1, v2) => write!(f, "{} * {}", v1, v2),
            ASTNode::Div(v1, v2) => write!(f, "{} / {}", v1, v2),
            ASTNode::Mod(v1, v2) => write!(f, "{} % {}", v1, v2),
            ASTNode::Pow(v1, v2) => write!(f, "{}^{}", v1, v2),
            ASTNode::MatrixMul(v1, v2) => write!(f, "{} ** {}", v1, v2),
            ASTNode::Neg(v1) => write!(f, "-{}", v1),
            ASTNode::Pos(v1) => write!(f, "+{}", v1),
            ASTNode::Set(v1, v2) => write!(f, "{} = {}", v1, v2),
            ASTNode::Find(v1, Some(v2)) => write!(f, "{} = {} ?", v1, v2),
            ASTNode::Find(v1, None) => write!(f, "{} = ?", v1),
            ASTNode::Fun(v1, v2) => write!(f, "{}({})", v1, v2),
            ASTNode::FunSet(v1, v2, v3) => write!(f, "{}({}) = {}", v1, v2, v3),
            ASTNode::Matrix(v1) => disp_matrix(v1, f),
        }
    }
}

impl ASTNode {
    pub fn find_unknown_var(&self) -> Option<String> {
        match self {
            ASTNode::Par(v) => v.find_unknown_var(),
            ASTNode::Var(v) => Some(v.clone()),
            ASTNode::Add(v1, v2) => v1.find_unknown_var().or_else(|| v2.find_unknown_var()),
            ASTNode::Sub(v1, v2) => v1.find_unknown_var().or_else(|| v2.find_unknown_var()),
            ASTNode::Mul(v1, v2) => v1.find_unknown_var().or_else(|| v2.find_unknown_var()),
            ASTNode::Div(v1, v2) => v1.find_unknown_var().or_else(|| v2.find_unknown_var()),
            ASTNode::Mod(v1, v2) => v1.find_unknown_var().or_else(|| v2.find_unknown_var()),
            ASTNode::Pow(v1, v2) => v1.find_unknown_var().or_else(|| v2.find_unknown_var()),
            ASTNode::MatrixMul(v1, v2) => v1.find_unknown_var().or_else(|| v2.find_unknown_var()),
            ASTNode::Neg(v) => v.find_unknown_var(),
            ASTNode::Pos(v) => v.find_unknown_var(),
            _ => None,
        }
    }

    pub fn eval_partial(&self, state: &mut State, unknown_var: &str) -> Result<ASTNode, EvalError> {
        Ok(match self {
            ASTNode::Par(v) => match v.eval_partial(state, unknown_var)? {
                e @ ASTNode::Par(..) 
                    | e @ ASTNode::Var(..)
                    | e @ ASTNode::Num(..)
                    | e @ ASTNode::Neg(..)
                    | e @ ASTNode::Pos(..)
                    | e @ ASTNode::Fun(..)
                    => e,
                e => ASTNode::Par(Box::new(e)),
            },
            ASTNode::Var(v) => if v == unknown_var {
                ASTNode::Var(v.clone())
            } else {
                match self.eval(state)?.ok_or_else(|| EvalError::MissingValue { node: Box::new(self.clone()) })? {
                    EvalValue::Complex(v) => ASTNode::Num(v),
                    EvalValue::UnknownSolve(v) => v.as_ref().clone(),
                    _ => Err(EvalError::SolveValueType)?,
                }
            },
            ASTNode::Num(v) => ASTNode::Num(*v),
            ASTNode::Add(v1, v2) => match (v1.eval_partial(state, unknown_var)?, v2.eval_partial(state, unknown_var)?) {
                (ASTNode::Num(v1), ASTNode::Num(v2)) => ASTNode::Num(v1.try_add(v2)?),

                (other, ASTNode::Num(Complex::ZERO)) => other, // X + 0 = X forall X
                (ASTNode::Num(Complex::ZERO), other) => other, // 0 + X = X forall X

                (v1, v2) => ASTNode::Add(Box::new(v1.clone()), Box::new(v2.clone())),
            },
            ASTNode::Sub(v1, v2) => match (v1.eval_partial(state, unknown_var)?, v2.eval_partial(state, unknown_var)?) {
                (ASTNode::Num(v1), ASTNode::Num(v2)) => ASTNode::Num(v1.try_sub(v2)?),

                (other, ASTNode::Num(Complex::ZERO)) => other, // X - 0 = X forall X
                (ASTNode::Num(Complex::ZERO), other) => ASTNode::Neg(Box::new(other)), // 0 - X = -X forall X

                (v1, v2) => ASTNode::Sub(Box::new(v1.clone()), Box::new(v2.clone())),
            },
            ASTNode::Mul(v1, v2) => match (v1.eval_partial(state, unknown_var)?, v2.eval_partial(state, unknown_var)?) {
                (ASTNode::Num(v1), ASTNode::Num(v2)) => ASTNode::Num(v1.try_mul(v2)?),

                (_, ASTNode::Num(Complex::ZERO)) => ASTNode::Num(Complex::ZERO), // X * 0 = 0 forall X
                (ASTNode::Num(Complex::ZERO), _) => ASTNode::Num(Complex::ZERO), // 0 * X = 0 forall X
                (other, ASTNode::Num(Complex::ONE)) => other, // X * 1 = X forall X
                (ASTNode::Num(Complex::ONE), other) => other, // 1 * X = X forall X

                (v1, v2) => ASTNode::Mul(Box::new(v1.clone()), Box::new(v2.clone())),
            },
            ASTNode::Div(v1, v2) => match (v1.eval_partial(state, unknown_var)?, v2.eval_partial(state, unknown_var)?) {
                (ASTNode::Num(v1), ASTNode::Num(v2)) => ASTNode::Num(v1.try_div(v2)?),

                (other, ASTNode::Num(Complex::ONE)) => other, // X / 1 = X forall X

                (v1, v2) => ASTNode::Div(Box::new(v1.clone()), Box::new(v2.clone())),
            },
            ASTNode::Mod(v1, v2) => match (v1.eval_partial(state, unknown_var)?, v2.eval_partial(state, unknown_var)?) {
                (ASTNode::Num(v1), ASTNode::Num(v2)) => ASTNode::Num(v1.try_rem(v2)?),
                (v1, v2) => ASTNode::Mod(Box::new(v1.clone()), Box::new(v2.clone())),
            },
            ASTNode::Pow(v1, v2) => match (v1.eval_partial(state, unknown_var)?, v2.eval_partial(state, unknown_var)?) {
                (ASTNode::Num(v1), ASTNode::Num(v2)) => ASTNode::Num(v1.try_pow(v2)?),

                (_, ASTNode::Num(Complex::ZERO)) => ASTNode::Num(Complex::ONE), // X ^ 0 = 1 forall X
                (other, ASTNode::Num(Complex::ONE)) => other, // X ^ 1 = X forall X

                (v1, v2) => ASTNode::Pow(Box::new(v1.clone()), Box::new(v2.clone())),
            },
            ASTNode::MatrixMul(v1, v2) => ASTNode::MatrixMul(v1.clone(), v2.clone()),
            ASTNode::Neg(v) => match v.eval_partial(state, unknown_var)? {
                ASTNode::Num(v1) => ASTNode::Num(-v1),
                v1 => ASTNode::Neg(Box::new(v1)),
            },
            ASTNode::Pos(v) => v.eval_partial(state, unknown_var)?,
            ASTNode::Set( .. ) => Err(EvalError::SolveValueType)?,
            ASTNode::Find( .. ) => Err(EvalError::SolveValueType)?,
            ASTNode::Fun(name, node) => {
                let param = node.eval_partial(state, unknown_var)?;
                let func = state.get(name).cloned();
                match func {
                    Some(EvalValue::Function { arg_name, body, .. }) => {
                        state.new_frame()?;
                        state.set(&arg_name, EvalValue::UnknownSolve(Box::new(param)));
                        let ret = body.eval_partial(state, unknown_var)?;
                        state.pop_frame();
                        ret
                    }
                    Some(_) => Err(EvalError::TypeErrorCall { name: name.clone() })?,
                    None => Err(EvalError::VariableNotExist { name: name.clone() })?,
                }
            },
            ASTNode::FunSet( .. ) => Err(EvalError::SolveValueType)?,
            ASTNode::Matrix( .. ) => Err(EvalError::SolveValueType)?,
        })
    }

    pub fn eval(&self, state: &mut State) -> Result<Option<EvalValue>, EvalError> {
        match self {
            ASTNode::Par(node) => node.eval(state),
            ASTNode::Var(s) => {
                if s.to_lowercase() == "i" {
                    Ok(Some(Complex::imag(1.into()).into()))
                } else {
                    state
                        .get(s)
                        .cloned()
                        .map(Some)
                        .ok_or(EvalError::VariableNotExist { name: s.clone() })
                }
            }
            ASTNode::Num(c) => Ok(Some(EvalValue::Complex(*c))),
            ASTNode::Add(lhs, rhs) => Ok(Some(match (lhs.eval(state)?, rhs.eval(state)?) {
                (Some(v1), Some(v2)) => v1.try_add(v2)?,
                (None, _) => Err(EvalError::MissingValue { node: lhs.clone() })?,
                (_, None) => Err(EvalError::MissingValue { node: rhs.clone() })?,
            })),
            ASTNode::Sub(lhs, rhs) => Ok(Some(match (lhs.eval(state)?, rhs.eval(state)?) {
                (Some(v1), Some(v2)) => v1.try_sub(v2)?,
                (None, _) => Err(EvalError::MissingValue { node: lhs.clone() })?,
                (_, None) => Err(EvalError::MissingValue { node: rhs.clone() })?,
            })),
            ASTNode::Mul(lhs, rhs) => Ok(Some(match (lhs.eval(state)?, rhs.eval(state)?) {
                (Some(v1), Some(v2)) => v1.try_mul(v2)?,
                (None, _) => Err(EvalError::MissingValue { node: lhs.clone() })?,
                (_, None) => Err(EvalError::MissingValue { node: rhs.clone() })?,
            })),
            ASTNode::Div(lhs, rhs) => Ok(Some(match (lhs.eval(state)?, rhs.eval(state)?) {
                (Some(v1), Some(v2)) => v1.try_div(v2)?,
                (None, _) => Err(EvalError::MissingValue { node: lhs.clone() })?,
                (_, None) => Err(EvalError::MissingValue { node: rhs.clone() })?,
            })),
            ASTNode::Mod(lhs, rhs) => Ok(Some(match (lhs.eval(state)?, rhs.eval(state)?) {
                (Some(v1), Some(v2)) => v1.try_rem(v2)?,
                (None, _) => Err(EvalError::MissingValue { node: lhs.clone() })?,
                (_, None) => Err(EvalError::MissingValue { node: rhs.clone() })?,
            })),
            ASTNode::Pow(lhs, rhs) => Ok(Some(match (lhs.eval(state)?, rhs.eval(state)?) {
                (Some(v1), Some(v2)) => v1.try_pow(v2)?,
                (None, _) => Err(EvalError::MissingValue { node: lhs.clone() })?,
                (_, None) => Err(EvalError::MissingValue { node: rhs.clone() })?,
            })),
            ASTNode::MatrixMul(lhs, rhs) => Ok(Some(match (lhs.eval(state)?, rhs.eval(state)?) {
                (Some(v1), Some(v2)) => v1.try_mat_mul(v2)?,
                (None, _) => Err(EvalError::MissingValue { node: lhs.clone() })?,
                (_, None) => Err(EvalError::MissingValue { node: rhs.clone() })?,
            })),
            ASTNode::Neg(ope) => Ok(Some(match ope.eval(state)? {
                Some(v) => v.try_neg()?,
                None => Err(EvalError::MissingValue { node: ope.clone() })?,
            })),
            ASTNode::Pos(el) => el.eval(state),
            ASTNode::Set(s, node) => {
                if s.to_lowercase() == "i" { Err(EvalError::SetIVar)? }
                if let Some(val) = node.eval(state)? {
                    state.set(s, val.clone());
                    Ok(Some(val))
                } else {
                    Err(EvalError::MissingValue { node: node.clone() })
                }
            }
            ASTNode::Find(lhs, rhs_opt) => {
                if let Some(rhs) = rhs_opt {
                    let unknown_var = lhs.find_unknown_var().or_else(|| rhs.find_unknown_var());
                    if unknown_var.is_none() { Err(EvalError::SolveVariable)? }
                    let unknown_var = unknown_var.unwrap();

                    let lhs = lhs.eval_partial(state, &unknown_var[..])?;
                    let rhs = rhs.eval_partial(state, &unknown_var[..])?;

                    let eq = Equation::try_from_ast(&lhs, &rhs, unknown_var)?;
                    let reduced = eq.reduced();

                    reduced.print_solutions()?;

                    Ok(None)
                } else {
                    Ok(match lhs.eval(state)? {
                        Some(v) => Some(v),
                        None => Err(EvalError::MissingValue { node: lhs.clone() })?
                    })
                }
            },
            ASTNode::Fun(name, node) => {
                if let Some(param) = node.eval(state)? {
                    let func = state.get(name).cloned();
                    match func {
                        Some(EvalValue::Function { arg_name, body, .. }) => {
                            state.new_frame()?;
                            state.set(&arg_name, param);
                            let ret = body.eval(state)?;
                            state.pop_frame();
                            Ok(ret)
                        }
                        Some(_) => Err(EvalError::TypeErrorCall { name: name.clone() }),
                        None => Err(EvalError::VariableNotExist { name: name.clone() }),
                    }
                } else {
                    Err(EvalError::MissingValue { node: node.clone() })?
                }
            }
            ASTNode::FunSet(name, variable, body) => {
                if name.to_lowercase() == "i" {
                    Err(EvalError::SetIVar)?
                }

                let mut body = body.clone();
                if let Ok(node) = body.eval_partial(state, variable.as_ref()) {
                    body = Box::new(node);
                }

                state.set(
                    &name,
                    EvalValue::Function {
                        name: name.clone(),
                        arg_name: variable.clone(),
                        body,
                    },
                );
                Ok(Some(state.get(&name).unwrap().clone()))
            }
            ASTNode::Matrix(matrix) => Ok(Some({
                if matrix.len() == 0 {
                    Matrix::empty().into()
                } else {
                    let rows = matrix.len();
                    let cols = matrix[0].len();
                    let mut ctnr = vec![];
                    for row in matrix {
                        if row.len() != cols {
                            return Err(EvalError::InvalidMatrixShape {
                                node: Box::new(self.clone()),
                            })?;
                        }
                        ctnr.extend(
                            row.iter()
                                .map(|val| {
                                    val.eval(state).and_then(|o| {
                                        if let Some(EvalValue::Complex(c)) = o {
                                            Ok(c)
                                        } else if let Some(_) = o {
                                            Err(EvalError::ValueInvalidInMatrix {
                                                node: val.clone(),
                                            })
                                        } else {
                                            Err(EvalError::MissingValue { node: val.clone() })
                                        }
                                    })
                                })
                                .collect::<Result<Vec<_>, _>>()?,
                        )
                    }
                    Matrix::new(rows, cols, ctnr).into()
                }
            })),
        }
    }
}
