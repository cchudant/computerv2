pub mod complex;
pub mod eval;
pub mod matrix;
pub mod ops;
pub mod rational;
pub mod solve;
pub mod util;
pub mod value;

use complex::*;
use eval::*;
use rational::*;
use value::*;

use std::io::Write;

peg::parser! {
    grammar computor_parser() for str {
        rule number() -> Rational
            = quiet!{ n:$(['+' | '-']? " "* ['0'..='9']+ ("." ['0'..='9']+)?)
            { n.parse().unwrap() } } / expected!("number")

        rule identifier() -> String
            = quiet!{ ident:$(['a'..='z' | 'A'..='Z' | '_'] ['a'..='z' | 'A'..='Z' | '0'..='9' | '_']*)
            { ident.into() } } / expected!("identifier")

        rule matrix_row() -> Vec<Box<ASTNode>>
            = "[" " "* n:expression() ** (" "* "," " "*) " "*  "]"
            { n } / expected!("matrix row")

        rule matrix() -> Vec<Vec<Box<ASTNode>>>
            = "[" " "* rows:matrix_row() ** (" "* ";" " "*) " "*  "]"
            { rows }
            / "[" " "* n:expression() ** (" "* ";" " "*) " "*  "]"
            { n.iter().map(|n| vec![n.clone()]).collect() }
            / rows:matrix_row()
            { vec![rows] } / expected!("matrix")

        pub rule expression() -> Box<ASTNode>
            = quiet!{ precedence!{
                ident:identifier() " "* "=" " "* expr2:expression() " "* !"?"
                { Box::new(ASTNode::Set(ident, expr2)) }
                ident1:identifier() " "* "(" " "* ident2:identifier() " "* ")" " "* "=" " "*  expr2:expression() " "* !"?"
                { Box::new(ASTNode::FunSet(ident1, ident2, expr2)) }
                --
                expr:(@) " "* "=" " "* expr2:expression()? " "* "?"
                { Box::new(ASTNode::Find(expr, expr2)) }
                --
                expr1:(@) " "* "+" " "*  expr2:@
                { Box::new(ASTNode::Add(expr1, expr2)) }
                expr1:(@) " "* "-" " "* expr2:@
                { Box::new(ASTNode::Sub(expr1, expr2)) }
                --
                expr1:(@) " "* "**" " "*  expr2:@
                { Box::new(ASTNode::MatrixMul(expr1, expr2)) }
                expr1:(@) " "* ident:identifier()
                { Box::new(ASTNode::Mul(expr1, Box::new(ASTNode::Var(ident)))) }
                expr1:(@) " "* "*" " "*  expr2:@
                { Box::new(ASTNode::Mul(expr1, expr2)) }
                expr1:(@) " "* "/" " "* expr2:@
                { Box::new(ASTNode::Div(expr1, expr2)) }
                expr1:(@) " "* "%" " "* expr2:@
                { Box::new(ASTNode::Mod(expr1, expr2)) }
                --
                expr1:@ " "* "^" " "* expr2:(@)
                { Box::new(ASTNode::Pow(expr1, expr2)) }
                --
                "-" " "* expr:(@)
                { Box::new(ASTNode::Neg(expr)) }
                "+" " "* expr:(@)
                { Box::new(ASTNode::Pos(expr)) }
                --
                "(" " "* expr:expression() " "*  ")"
                { Box::new(ASTNode::Par(expr)) }
                --
                ident:identifier() " "* "(" " "* expr:expression() " "*  ")"
                { Box::new(ASTNode::Fun(ident, expr)) }
                --
                ident:identifier()
                { Box::new(ASTNode::Var(ident)) }
                --
                mat:matrix()
                { Box::new(ASTNode::Matrix(mat)) }
                "i" " "* expr:number()
                { Box::new(ASTNode::Num(Complex::imag(expr))) }
                expr:number() " "* "i"
                { Box::new(ASTNode::Num(Complex::imag(expr))) }
                "i"
                { Box::new(ASTNode::Num(Complex::imag(1.into()))) }
                expr:number()
                { Box::new(ASTNode::Num(Complex::real(expr))) }
            } }
            / expected!("expression")
    }
}

struct HistoryEntry {
    input: Box<ASTNode>,
    value: EvalValue,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut state = State::new();
    let mut history: Vec<HistoryEntry> = Vec::new();

    let mut s = String::new();
    loop {
        s.clear();

        std::io::stdout().write("> ".as_bytes())?;
        std::io::stdout().flush()?;
        std::io::stdin().read_line(&mut s)?;

        let trimmed = s.trim();

        if s.len() == 0 || trimmed == "exit" {
            break;
        } else if trimmed == "" {
            continue;
        } else if trimmed == "show vars" {
            for el in state.vars() {
                el.1.display_with_name(&el.0[..])
            }
            continue;
        } else if trimmed == "show history" {
            for el in &history {
                println!("Input: {}, Result: {}", el.input, el.value);
            }
            continue;
        }

        let parsed_res = computor_parser::expression(trimmed);

        if let Err(e) = parsed_res {
            println!("Parsing error at {}: expected {}", e.location, e.expected);
            continue;
        }

        let parsed = parsed_res.unwrap();

        match parsed.eval(&mut state) {
            Ok(Some(val)) => {
                println!("{}", val);

                history.push(HistoryEntry {
                    input: parsed,
                    value: val,
                })
            },
            Ok(None) => {}
            Err(e) => {
                println!("Evaluation error: {}", e);
                continue;
            }
        }
    }

    Ok(())
}
