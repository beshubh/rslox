use crate::ast::{Expr, ExprVisitor};
use crate::token::Literal;

pub fn print_ast(expr: Expr) -> String {
    match &expr {
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

pub fn parenthesize(name: &str, exprs: &Vec<&Box<Expr>>) -> String {
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
