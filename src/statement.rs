use crate::{ast::Expr, token::Token};

pub enum Stmt {
    Block(Vec<Stmt>),
    Expression(Box<Expr>),
    If(Box<Expr>, Box<Stmt>, Option<Box<Stmt>>),
    Print(Box<Expr>),
    Var(Token, Option<Box<Expr>>),
    While(Box<Expr>, Box<Stmt>),
}

pub trait StmtVisitor<R> {
    fn visit_block(&mut self, statements: &Vec<Stmt>) -> R;
    fn visit_expression(&self, expr: &Box<Expr>) -> R;
    fn visit_if(
        &mut self,
        condition: &Box<Expr>,
        then_branch: &Box<Stmt>,
        else_branch: &Option<Box<Stmt>>,
    ) -> R;
    fn visit_while(&mut self, condition: &Box<Expr>, body: &Box<Stmt>) -> R;
    fn visit_print(&self, expr: &Box<Expr>) -> R;
    fn visit_var(&self, name: &Token, initializer: &Option<Box<Expr>>) -> R;
}

impl Stmt {
    pub fn accept<R>(&self, visitor: &mut impl StmtVisitor<R>) -> R {
        match self {
            Stmt::Block(statements) => visitor.visit_block(statements),
            Stmt::Expression(expr) => visitor.visit_expression(expr),
            Stmt::If(condition, then_branch, else_branch) => {
                visitor.visit_if(condition, then_branch, else_branch)
            }
            Stmt::Print(expr) => visitor.visit_print(expr),
            Stmt::Var(name, initializer) => visitor.visit_var(name, initializer),
            Stmt::While(condition, body) => visitor.visit_while(condition, body),
        }
    }
}
