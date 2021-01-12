pub mod complex;
pub mod eval;
pub mod matrix;
pub mod ops;
pub mod rational;
pub mod util;
pub mod value;

use complex::*;
use eval::*;
use rational::*;

use std::io::Write;

peg::parser! {
    grammar computor_parser() for str {
        rule number() -> Rational
            = quiet!{ n:$(['+' | '-']? " "* ['0'..='9']+ ("." ['0'..='9']+)?)
            { n.parse().unwrap() } } / expected!("number")

        rule identifier() -> String
            = quiet!{ ident:$(['a'..='z' | 'A'..='Z'] ['a'..='z' | 'A'..='Z' | '0'..='9']*)
            { ident.into() } } / expected!("identifier")

        rule matrix_row() -> Vec<Box<ASTNode>>
            = "[" " "* n:expression() ** (" "* "," " "*) " "*  "]"
            { n }

        rule matrix() -> Vec<Vec<Box<ASTNode>>>
            = "[" " "* rows:matrix_row() ** (" "* ";" " "*) " "*  "]"
            { rows }
            / "[" " "* n:expression() ** (" "* ";" " "*) " "*  "]"
            { n.iter().map(|n| vec![n.clone()]).collect() }
            / rows:matrix_row()
            { vec![rows] }

        pub rule expression() -> Box<ASTNode>
            = quiet!{ precedence!{
                expr:(@) " "* "=" " "* ident:identifier()? " "* "?"
                { Box::new(ASTNode::Find(expr, ident)) }
                ident:identifier() " "* "=" " "* expr2:expression()
                { Box::new(ASTNode::Set(ident, expr2)) }
                ident1:identifier() " "* "(" " "* ident2:identifier() " "* ")" " "* "=" " "*  expr2:(@)
                { Box::new(ASTNode::FunSet(ident1, ident2, expr2)) }
                --
                expr1:(@) " "* "+" " "*  expr2:@
                { Box::new(ASTNode::Add(expr1, expr2)) }
                expr1:(@) " "* "-" " "* expr2:@
                { Box::new(ASTNode::Sub(expr1, expr2)) }
                --
                expr1:(@) " "* "**" " "*  expr2:@
                { Box::new(ASTNode::MatrixMul(expr1, expr2)) }
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
            // / expected!("expression")
    }
}

// type ParseError = peg::error::ParseError<peg::str::LineCol>;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut state = State::new();

    let mut s = String::new();
    loop {
        s.clear();

        std::io::stdout().write("> ".as_bytes())?;
        std::io::stdout().flush()?;
        std::io::stdin().read_line(&mut s)?;

        if s.len() == 0 {
            break;
        }

        if s.trim() == "" {
            continue;
        }

        let parsed_res = computor_parser::expression(s.trim());

        if let Err(_) = parsed_res {
            println!("Parsing error");
            continue;
        }

        let parsed = parsed_res.unwrap();

        match parsed.eval(&mut state) {
            Ok(Some(val)) => println!("{}", val),
            Ok(None) => {},
            Err(e) => {
                println!("Evaluation error: {}", e);
                continue;
            }
        }
    }

    Ok(())
}
