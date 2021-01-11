use std::collections::HashMap;
use std::fmt;

use crate::complex::*;
use crate::matrix::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvalValue {
    Complex(Complex),
    Matrix(Matrix),
    Function { name: String, arg_name: String, body: Box<ASTNode> },
}

impl fmt::Display for EvalValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            EvalValue::Complex(c) => write!(f, "{}", c),
            EvalValue::Matrix(m) => write!(f, "{}", m),
            EvalValue::Function { name, arg_name, .. } => write!(f, "{}({}) = (...)", name, arg_name),
        }
    }
}

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
        self.stack.iter()
            .find_map(|frame| frame.get(s))
    }

    fn get_mut(&mut self, s: &str) -> Option<&mut EvalValue> {
        self.stack.iter_mut()
            .find_map(|frame| frame.get_mut(s))
    }

    fn set(&mut self, s: &str, val: EvalValue) {
        if let Some(v) = self.get_mut(&s) {
            *v = val;
        } else {
            let last = self.stack.last_mut().unwrap();
            last.insert(s.to_string(), val);
        }
    }

    fn new_frame(&mut self) {
        self.stack.push(HashMap::new());
    }

    fn pop_frame(&mut self) {
        self.stack.pop();
        assert!(self.stack.len() >= 1);
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvalError {
    VariableNotExist { name: String },
    TypeErrorBinary { op: &'static str, arg1: EvalValue, arg2: EvalValue },
    TypeErrorUnary { op: &'static str, arg: EvalValue },
    TypeErrorCall { name: String },
    MissingValueBinary { right: bool },
    MissingValueUnary,
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
    Matrix(Matrix),
}

impl ASTNode {
    pub fn eval(&self, state: &mut State) -> Result<Option<EvalValue>, EvalError> {
        match self {
            ASTNode::Par(node) => node.eval(state),
            ASTNode::Var(s) => state.get(s)
                .cloned()
                .map(Some)
                .ok_or(EvalError::VariableNotExist { name: s.clone() }),
            ASTNode::Num(c) => Ok(Some(EvalValue::Complex(*c))),
            ASTNode::Add(lhs, rhs) => {
                let lhs_val = lhs.eval(state)?;
                let rhs_val = rhs.eval(state)?;

                Ok(Some(match (lhs_val, rhs_val) {
                    (Some(EvalValue::Complex(c1)), Some(EvalValue::Complex(c2))) => EvalValue::Complex(c1 + c2),
                    (Some(EvalValue::Matrix(m1)), Some(EvalValue::Matrix(m2))) => EvalValue::Matrix(&m1 + &m2),
                    (Some(arg1), Some(arg2)) => Err(EvalError::TypeErrorBinary { op: "+", arg1, arg2 })?,
                    (None, _) => Err(EvalError::MissingValueBinary { right: false })?,
                    (_, None) => Err(EvalError::MissingValueBinary { right: true })?,
                }))
            },
            ASTNode::Sub(lhs, rhs) => {
                let lhs_val = lhs.eval(state)?;
                let rhs_val = rhs.eval(state)?;

                Ok(Some(match (lhs_val, rhs_val) {
                    (Some(EvalValue::Complex(c1)), Some(EvalValue::Complex(c2))) => EvalValue::Complex(c1 - c2),
                    (Some(EvalValue::Matrix(m1)), Some(EvalValue::Matrix(m2))) => EvalValue::Matrix(&m1 - &m2),
                    (Some(arg1), Some(arg2)) => Err(EvalError::TypeErrorBinary { op: "-", arg1, arg2 })?,
                    (None, _) => Err(EvalError::MissingValueBinary { right: false })?,
                    (_, None) => Err(EvalError::MissingValueBinary { right: true })?,
                }))
            },
            ASTNode::Mul(lhs, rhs) => {
                let lhs_val = lhs.eval(state)?;
                let rhs_val = rhs.eval(state)?;

                Ok(Some(match (lhs_val, rhs_val) {
                    (Some(EvalValue::Complex(c1)), Some(EvalValue::Complex(c2))) => EvalValue::Complex(c1 * c2),
                    (Some(EvalValue::Matrix(m1)), Some(EvalValue::Matrix(m2))) => EvalValue::Matrix(&m1 * &m2),
                    (Some(arg1), Some(arg2)) => Err(EvalError::TypeErrorBinary { op: "*", arg1, arg2 })?,
                    (None, _) => Err(EvalError::MissingValueBinary { right: false })?,
                    (_, None) => Err(EvalError::MissingValueBinary { right: true })?,
                }))
            },
            ASTNode::Div(lhs, rhs) => {
                let lhs_val = lhs.eval(state)?;
                let rhs_val = rhs.eval(state)?;

                Ok(Some(match (lhs_val, rhs_val) {
                    (Some(EvalValue::Complex(c1)), Some(EvalValue::Complex(c2))) => EvalValue::Complex(c1 / c2),
                    (Some(EvalValue::Matrix(m1)), Some(EvalValue::Matrix(m2))) => EvalValue::Matrix(&m1 / &m2),
                    (Some(arg1), Some(arg2)) => Err(EvalError::TypeErrorBinary { op: "/", arg1, arg2 })?,
                    (None, _) => Err(EvalError::MissingValueBinary { right: false })?,
                    (_, None) => Err(EvalError::MissingValueBinary { right: true })?,
                }))
            },
            ASTNode::Mod(_lhs, _rhs) => unimplemented!(),
            ASTNode::Pow(_lhs, _rhs) => unimplemented!(),
            ASTNode::MatrixMul(_lhs, _rhs) => unimplemented!(),
            ASTNode::Neg(el) => {
                let el_val = el.eval(state)?;

                Ok(Some(match el_val {
                    Some(EvalValue::Complex(c)) => EvalValue::Complex(-c),
                    Some(EvalValue::Matrix(m)) => EvalValue::Matrix(-&m),
                    Some(arg) => Err(EvalError::TypeErrorUnary { op: "-", arg })?,
                    None => Err(EvalError::MissingValueUnary)?,
                }))
            },
            ASTNode::Pos(el) => el.eval(state),
            ASTNode::Set(s, node) => {
                if let Some(val) = node.eval(state)? {
                    state.set(s, val.clone());
                    Ok(Some(val))
                } else { Err(EvalError::MissingValueUnary) }
            },
            ASTNode::Find(_lhs, _var_name) => unimplemented!(),
            ASTNode::Fun(name, node) => {
                if let Some(param) = node.eval(state)? {

                    let func = state.get(name).cloned();
                    match func {
                        Some(EvalValue::Function { arg_name, body, .. }) => {
                            state.new_frame();
                            state.set(&arg_name, param);
                            let ret = body.eval(state)?;
                            state.pop_frame();
                            Ok(ret)
                        },
                        Some(_) => Err(EvalError::TypeErrorCall { name: name.clone() }),
                        None => Err(EvalError::VariableNotExist { name: name.clone() }),
                    }

                } else { Err(EvalError::MissingValueUnary)? }
            },
            ASTNode::FunSet(name, variable, body) => {
                state.set(&name, EvalValue::Function {
                    name: name.clone(),
                    arg_name: variable.clone(),
                    body: body.clone(),
                });
                Ok(Some(state.get(&name).unwrap().clone()))
            },
            ASTNode::Matrix(matrix) => Ok(Some(EvalValue::Matrix(matrix.clone())))
        }
    }
}
