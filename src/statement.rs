use crate::ast::Expr;

pub enum Stmt {
    Expression(Box<Expr>),
    Print(Box<Expr>),
}

pub trait StmtVisitor<R> {
    fn visit_expression(&self, expr: &Box<Expr>) -> R;
    fn visit_print(&self, expr: &Box<Expr>) -> R;
}

impl Stmt {
    pub fn accept<R>(&self, visitor: &impl StmtVisitor<R>) -> R {
        match self {
            Stmt::Expression(expr) => visitor.visit_expression(expr),
            Stmt::Print(expr) => visitor.visit_print(expr),
        }
    }
}
