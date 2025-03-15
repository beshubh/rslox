use crate::ast::{Expr, ExprVisitor};
use crate::token::{Literal, Token};

pub struct AstPrinter;

impl AstPrinter {
    fn parenthesize(&self, name: &str, exprs: &[&Expr]) -> String {
        let mut result = String::new();

        result.push('(');
        result.push_str(name);

        for expr in exprs {
            result.push(' ');
            result.push_str(&expr.accept(self));
        }

        result.push(')');
        result
    }

    pub fn print(&self, expr: &Expr) -> String {
        expr.accept(self)
    }
}

impl ExprVisitor<String> for AstPrinter {
    fn visit_binary(
        &self,
        left: &Box<Expr>,
        op: &crate::token::Token,
        right: &Box<Expr>,
    ) -> String {
        return self.parenthesize(&op.lexeme, &[&left, &right]);
    }

    fn visit_unary(&self, op: &Token, expr: &Box<Expr>) -> String {
        return self.parenthesize(&op.lexeme, &[&expr]);
    }

    fn visit_grouping(&self, expr: &Box<Expr>) -> String {
        return self.parenthesize("group", &[&expr]);
    }

    fn visit_literal(&self, literal: &Literal) -> String {
        match literal {
            Literal::Nil => String::from("nil"),
            _ => literal.to_string(),
        }
    }
    fn visit_ternary(
        &self,
        cond: &Box<Expr>,
        then_expr: &Box<Expr>,
        else_expr: &Box<Expr>,
    ) -> String {
        return self.parenthesize("?:", &[&cond, &then_expr, &else_expr]);
    }
}
