use std::collections::HashMap;
use std::fmt;

use crate::complex::*;
use crate::matrix::*;
use crate::ops::*;
use crate::value::*;

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
    Find(Box<ASTNode>, Option<String>),
    Fun(String, Box<ASTNode>),
    FunSet(String, String, Box<ASTNode>),
    Matrix(Vec<Vec<Box<ASTNode>>>),
}

fn disp_matrix(mat: &Vec<Vec<Box<ASTNode>>>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let cols = mat.len();
    let rows = mat.first().map(|e| e.len()).unwrap_or(0);
    if cols != 1 || rows == 1 {
        write!(f, "[")?;
    }
    for c in 0..cols {
        if rows != 1 {
            write!(f, "[")?;
        }
        for r in 0..rows {
            write!(f, "{}", mat[c][r])?;
            if r != rows - 1 {
                write!(f, ", ")?;
            }
        }
        if rows != 1 {
            write!(f, "]")?;
        }
        if c != cols - 1 {
            write!(f, "; ")?;
        }
    }
    if cols != 1 || rows == 1 {
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
                if let Some(val) = node.eval(state)? {
                    state.set(s, val.clone());
                    Ok(Some(val))
                } else {
                    Err(EvalError::MissingValue { node: node.clone() })
                }
            }
            ASTNode::Find(_lhs, _var_name) => unimplemented!(),
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

                state.set(
                    &name,
                    EvalValue::Function {
                        name: name.clone(),
                        arg_name: variable.clone(),
                        body: body.clone(),
                    },
                );
                Ok(Some(state.get(&name).unwrap().clone()))
            }
            ASTNode::Matrix(matrix) => Ok(Some({
                if matrix.len() == 0 {
                    Matrix::empty().into()
                } else {
                    let cols = matrix.len();
                    let rows = matrix[0].len();
                    let mut ctnr = vec![];
                    for row in matrix {
                        if row.len() != rows {
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
