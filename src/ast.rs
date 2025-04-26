use crate::token::Literal;
use crate::token::Token;
use crate::token::TokenType;

pub enum Expr {
    Assign(Token, Box<Expr>),
    Binary(Box<Expr>, Token, Box<Expr>),
    Literal(Literal),
    Logical(Box<Expr>, Token, Box<Expr>),
    Unary(Token, Box<Expr>),
    Var(Token),
    Grouping(Box<Expr>),
    Ternary(Box<Expr>, Box<Expr>, Box<Expr>),
}

impl Expr {
    pub fn accept<R>(&self, vistor: &impl ExprVisitor<R>) -> R {
        match self {
            Expr::Assign(name, expr) => return vistor.visit_assign(name, expr),
            Expr::Binary(left, op, right) => {
                return vistor.visit_binary(left, op, right);
            }
            Expr::Literal(literal) => return vistor.visit_literal(literal),
            Expr::Logical(left, logical_op, right) => {
                return vistor.visit_logical(left, logical_op, right)
            }
            Expr::Unary(token, expr) => return vistor.visit_unary(token, expr),
            Expr::Grouping(expr) => return vistor.visit_grouping(expr),
            Expr::Ternary(cond, then_expr, else_expr) => {
                return vistor.visit_ternary(cond, then_expr, else_expr)
            }
            Expr::Var(name) => return vistor.visit_var(name),
        }
    }
}

pub trait ExprVisitor<R> {
    fn visit_assign(&self, name: &Token, expr: &Box<Expr>) -> R;
    fn visit_binary(&self, left: &Box<Expr>, op: &Token, right: &Box<Expr>) -> R;
    fn visit_literal(&self, literal: &Literal) -> R;
    fn visit_logical(&self, left: &Box<Expr>, logical_op: &Token, right: &Box<Expr>) -> R;
    fn visit_unary(&self, token: &Token, expr: &Box<Expr>) -> R;
    fn visit_grouping(&self, expr: &Box<Expr>) -> R;
    fn visit_ternary(&self, cond: &Box<Expr>, then_expr: &Box<Expr>, else_expr: &Box<Expr>) -> R;
    fn visit_var(&self, name: &Token) -> R;
}
