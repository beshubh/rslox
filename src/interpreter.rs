use crate::ast::{Expr, ExprVisitor};
use crate::lox::Lox;
use crate::token::{Literal, Token, TokenType};
use std::any::Any;
use std::fmt;

#[derive(Debug)]
pub struct RunTimeError {
    pub token: Token,
    pub message: String,
}

impl fmt::Display for RunTimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Runtime error: {} at {}", self.message, self.token)
    }
}

impl RunTimeError {
    pub fn new(token: Token, message: &str) -> Self {
        RunTimeError {
            message: message.to_string(),
            token,
        }
    }
}

impl std::error::Error for RunTimeError {}

type Result<T> = std::result::Result<T, RunTimeError>;

pub struct Interpreter;

impl Interpreter {
    pub fn interpret(&self, expr: &Box<Expr>) {
        let res = self.evaluate(expr);
        if let Err(err) = res {
            Lox::runtime_error(err);
            return;
        }
        println!("{:?}", self.stringify(res.unwrap()));
    }

    pub fn stringify(&self, obj: Box<dyn Any>) -> String {
        if obj.is::<Option<()>>() {
            return "nil".to_string();
        }
        if obj.is::<f64>() {
            let text = obj.downcast_ref::<f64>().unwrap().to_string();
            if text.ends_with(".0") {
                return text[0..text.len() - 2].to_string();
            }
            return text;
        }
        if obj.is::<bool>() {
            return obj.downcast_ref::<bool>().unwrap().to_string();
        }
        return obj.downcast_ref::<String>().unwrap().to_string();
    }

    fn evaluate(&self, expr: &Box<Expr>) -> Result<Box<dyn Any>> {
        return expr.accept(self);
    }

    fn is_truthy(&self, obj: Box<dyn Any>) -> bool {
        // false and nil are false everything else is true REF: ruby
        if obj.is::<Option<()>>() {
            return false;
        }
        if obj.is::<bool>() {
            return *obj.downcast_ref::<bool>().unwrap();
        }
        return true;
    }

    fn is_equal(&self, a: Box<dyn Any>, b: Box<dyn Any>) -> bool {
        if a.is::<Option<()>>() && b.is::<Option<()>>() {
            return true;
        }
        if a.is::<Option<()>>() || b.is::<Option<()>>() {
            return false;
        }
        if a.is::<f64>() && b.is::<f64>() {
            return a.downcast_ref::<f64>() == b.downcast_ref::<f64>();
        }
        if a.is::<String>() && b.is::<String>() {
            return a.downcast_ref::<String>() == b.downcast_ref::<String>();
        }
        if a.is::<bool>() && b.is::<bool>() {
            return a.downcast_ref::<bool>() == b.downcast_ref::<bool>();
        }
        return false;
    }

    fn check_number_operand(&self, op: &Token, operand: &Box<dyn Any>) -> Result<()> {
        if !operand.is::<f64>() {
            return Err(RunTimeError::new(op.clone(), "Operand must be a number."));
        }
        Ok(())
    }

    fn check_number_operand_s(
        &self,
        op: &Token,
        left: &Box<dyn Any>,
        right: &Box<dyn Any>,
    ) -> Result<()> {
        if !left.is::<f64>() || !right.is::<f64>() {
            return Err(RunTimeError::new(op.clone(), "Operands must be numbers"));
        }
        Ok(())
    }
}

impl ExprVisitor<Result<Box<dyn Any>>> for Interpreter {
    fn visit_literal(&self, literal: &Literal) -> Result<Box<dyn Any>> {
        match literal {
            Literal::Number(num) => Ok(Box::new(*num) as Box<dyn Any>),
            Literal::String(s) => Ok(Box::new(s.clone()) as Box<dyn Any>),
            Literal::Boolean(b) => Ok(Box::new(*b) as Box<dyn Any>),
            Literal::Nil => Ok(Box::new(()) as Box<dyn Any>),
        }
    }

    fn visit_grouping(&self, expr: &Box<Expr>) -> Result<Box<dyn Any>> {
        Ok(self.evaluate(expr)?)
    }

    fn visit_unary(&self, op: &Token, expr: &Box<Expr>) -> Result<Box<dyn Any>> {
        let right = self.evaluate(expr)?;
        match op.token_type {
            TokenType::BANG => return Ok(Box::new(!self.is_truthy(right))),
            TokenType::MINUS => {
                self.check_number_operand(op, &right)?;
                return Ok(Box::new(
                    -self.evaluate(expr)?.downcast_ref::<f64>().unwrap(),
                ));
            }
            _ => Err(RunTimeError::new(op.clone(), "Unknown unary operator")),
        }
    }

    fn visit_binary(
        &self,
        left: &Box<Expr>,
        op: &Token,
        right: &Box<Expr>,
    ) -> Result<Box<dyn Any>> {
        let left = self.evaluate(left)?;
        let right = self.evaluate(right)?;
        match op.token_type {
            TokenType::GREATER => {
                self.check_number_operand_s(op, &left, &right)?;
                return Ok(Box::new(
                    left.downcast_ref::<f64>().unwrap() > right.downcast_ref::<f64>().unwrap(),
                ));
            }
            TokenType::GREATEREQUAL => {
                self.check_number_operand_s(op, &left, &right)?;
                return Ok(Box::new(
                    left.downcast_ref::<f64>().unwrap() >= right.downcast_ref::<f64>().unwrap(),
                ));
            }
            TokenType::LESS => {
                self.check_number_operand_s(op, &left, &right)?;
                return Ok(Box::new(
                    left.downcast_ref::<f64>().unwrap() < right.downcast_ref::<f64>().unwrap(),
                ));
            }
            TokenType::LESSEQUAL => {
                self.check_number_operand_s(op, &left, &right)?;
                return Ok(Box::new(
                    left.downcast_ref::<f64>().unwrap() <= right.downcast_ref::<f64>().unwrap(),
                ));
            }
            TokenType::BANGEQUAL => {
                return Ok(Box::new(
                    left.downcast_ref::<f64>().unwrap() != right.downcast_ref::<f64>().unwrap(),
                ));
            }
            TokenType::EQUALEQUAL => {
                return Ok(Box::new(self.is_equal(left, right)));
            }
            TokenType::MINUS => {
                self.check_number_operand_s(op, &left, &right)?;
                return Ok(Box::new(
                    left.downcast_ref::<f64>().unwrap() - right.downcast_ref::<f64>().unwrap(),
                ));
            }
            TokenType::PLUS => {
                if left.is::<f64>() && right.is::<f64>() {
                    return Ok(Box::new(
                        left.downcast_ref::<f64>().unwrap() + right.downcast_ref::<f64>().unwrap(),
                    ));
                }
                if left.is::<String>() && right.is::<String>() {
                    return Ok(Box::new(format!(
                        "{}{}",
                        left.downcast_ref::<String>().unwrap(),
                        right.downcast_ref::<String>().unwrap()
                    )));
                }

                if left.is::<String>() && right.is::<f64>() {
                    return Ok(Box::new(format!(
                        "{}{}",
                        left.downcast_ref::<String>().unwrap(),
                        right.downcast_ref::<f64>().unwrap()
                    )));
                }
                if left.is::<f64>() && right.is::<String>() {
                    return Ok(Box::new(format!(
                        "{}{}",
                        left.downcast_ref::<f64>().unwrap(),
                        right.downcast_ref::<String>().unwrap()
                    )));
                }
                return Err(RunTimeError::new(
                    op.clone(),
                    "Operands not compatible for addition/concatenation",
                ));
            }
            TokenType::SLASH => {
                self.check_number_operand_s(op, &left, &right)?;
                if right.downcast_ref::<f64>().unwrap() == &0.0 {
                    return Err(RunTimeError::new(
                        op.clone(),
                        "Division by zero is not allowed.",
                    ));
                }
                return Ok(Box::new(
                    left.downcast_ref::<f64>().unwrap() / right.downcast_ref::<f64>().unwrap(),
                ));
            }
            TokenType::STAR => Ok(Box::new(
                left.downcast_ref::<f64>().unwrap() * right.downcast_ref::<f64>().unwrap(),
            )),
            _ => {
                panic!("Unknown binary operator: {:?}", op.token_type);
            }
        }
    }

    fn visit_ternary(
        &self,
        cond: &Box<Expr>,
        then_expr: &Box<Expr>,
        else_expr: &Box<Expr>,
    ) -> Result<Box<dyn Any>> {
        let cond = self.evaluate(cond)?;
        if self.is_truthy(cond) {
            return self.evaluate(then_expr);
        }
        return self.evaluate(else_expr);
    }
}
