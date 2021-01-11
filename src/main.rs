pub mod rational;
pub mod complex;
pub mod matrix;
pub mod util;
pub mod eval;

use rational::*;
use complex::*;
use eval::*;

use std::convert::TryInto;

peg::parser!{
    grammar computor_parser() for str {
        rule number() -> Rational
            = quiet!{ n:$(['+' | '-']? " "* ['0'..='9']+ ("." ['0'..='9']+)?)
            { n.parse().unwrap() } } / expected!("number")

        rule identifier() -> String
            = quiet!{ ident:$(['a'..='z' | 'A'..='Z'] ['a'..='z' | 'A'..='Z' | '0'..='9']*)
            { ident.into() } } / expected!("identifier")

        rule matrix_row() -> Vec<Rational>
            = "[" " "* n:number() ** (" "* "," " "*) " "*  "]"
            { n }

        rule matrix() -> Vec<Vec<Rational>>
            = "[" " "* rows:matrix_row() ** (" "* ";" " "*) " "*  "]"
            { rows }
            / "[" " "* n:number() ** (" "* ";" " "*) " "*  "]"
            { n.iter().map(|n| vec![*n]).collect() }
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
                { Box::new(ASTNode::Matrix(mat.try_into().unwrap())) }
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

type ParseError = peg::error::ParseError<peg::str::LineCol>;

fn main() -> Result<(), ParseError> {
    let parsed = computor_parser::expression(std::env::args().nth(1).unwrap().trim())?;
    println!("Parsed: {:?}", parsed);

    let mut state = State::new();

    let ev = parsed.eval(&mut state);
    println!("{:?}", ev);

    match ev {
        Err(e) => println!("Evaluation error: {:?}", e),
        Ok(Some(v)) => println!("{}", v),
        Ok(None) => println!("None"),
    }

    Ok(())
}
