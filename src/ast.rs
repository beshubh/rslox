use crate::token::Literal;
use crate::token::Token;

pub enum Expr {
    Binary(Box<Expr>, Token, Box<Expr>),
    Literal(Literal),
    Unary(Token, Box<Expr>),
    Grouping(Box<Expr>),
    Ternary(Box<Expr>, Box<Expr>, Box<Expr>),
}

impl Expr
where
    Self: ExprVisitor,
{
    pub fn accept(&self) -> String {
        return self.visit();
    }
}

pub trait ExprVisitor {
    fn visit(&self) -> String;
}

impl ExprVisitor for Expr {
    fn visit(&self) -> String {
        match self {
            Expr::Binary(left, op, right) => return parenthesize(&op.lexeme, &vec![left, right]),
            Expr::Literal(literal) => match literal {
                Literal::Nil => String::from("nil"),
                _ => literal.to_string(),
            },
            Expr::Unary(op, right) => return parenthesize(&op.lexeme, &vec![right]),
            Expr::Grouping(expr) => return parenthesize("group", &vec![expr]),
            Expr::Ternary(cond, then_expr, else_expr) => {
                return parenthesize("?:", &vec![cond, then_expr, else_expr]);
            }
        }
    }
}

fn parenthesize(name: &str, exprs: &Vec<&Box<Expr>>) -> String {
    let mut res = String::new();
    res.push('(');
    res.push_str(name);
    for expr in exprs {
        res.push_str(&expr.visit());
        res.push(' ');
    }
    res.push(')');
    res
}
