use crate::ast::{Expr, ExprVisitor};
use crate::environment::Environment;
use crate::lox::Lox;
use crate::statement::{Stmt, StmtVisitor};
use crate::token::{Literal, Token, TokenType};
use std::any::Any;
use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

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

pub struct Interpreter {
    environment: Rc<Environment>,
}

impl Interpreter {
    pub fn new(environment: Rc<Environment>) -> Self {
        Interpreter { environment }
    }

    pub fn interpret(&mut self, statements: &Vec<Stmt>) {
        for stmt in statements {
            let res = self.execute(stmt);
            if let Err(err) = res {
                Lox::runtime_error(err);
                return;
            }
        }
    }

    fn execute(&mut self, stmt: &Stmt) -> Result<()> {
        return stmt.accept(self);
    }

    fn execute_block(
        &mut self,
        statements: &Vec<Stmt>,
        environment: Rc<Environment>,
    ) -> Result<()> {
        let previous = self.environment.clone();
        self.environment = environment;
        for stmt in statements {
            self.execute(stmt)?;
        }
        self.environment = previous;
        Ok(())
    }

    fn evaluate(&self, expr: &Box<Expr>) -> Result<Rc<RefCell<dyn Any>>> {
        return expr.accept(self);
    }

    pub fn stringify(&self, obj: Rc<RefCell<dyn Any>>) -> String {
        let obj = obj.borrow();
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

    fn is_truthy(&self, obj: Rc<RefCell<dyn Any>>) -> bool {
        // false and nil are false everything else is true REF: ruby
        let obj = obj.borrow();
        if obj.is::<Option<()>>() {
            return false;
        }
        if obj.is::<bool>() {
            return *obj.downcast_ref::<bool>().unwrap();
        }
        return true;
    }

    fn is_equal(&self, a: Rc<RefCell<dyn Any>>, b: Rc<RefCell<dyn Any>>) -> bool {
        let a = a.borrow();
        let b = b.borrow();

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

    fn check_number_operand(&self, op: &Token, operand: &Rc<RefCell<dyn Any>>) -> Result<()> {
        let operand = operand.borrow();
        if !operand.is::<f64>() {
            return Err(RunTimeError::new(op.clone(), "Operand must be a number."));
        }
        Ok(())
    }

    fn check_number_operand_s(
        &self,
        op: &Token,
        left: &Rc<RefCell<dyn Any>>,
        right: &Rc<RefCell<dyn Any>>,
    ) -> Result<()> {
        let left = left.borrow();
        let right = right.borrow();
        if !left.is::<f64>() || !right.is::<f64>() {
            return Err(RunTimeError::new(op.clone(), "Operands must be numbers"));
        }
        Ok(())
    }
}

impl ExprVisitor<Result<Rc<RefCell<dyn Any>>>> for Interpreter {
    fn visit_literal(&self, literal: &Literal) -> Result<Rc<RefCell<dyn Any>>> {
        match literal {
            Literal::Number(num) => Ok(Rc::new(RefCell::new(*num)) as Rc<RefCell<dyn Any>>),
            Literal::String(s) => Ok(Rc::new(RefCell::new(s.clone())) as Rc<RefCell<dyn Any>>),
            Literal::Boolean(b) => Ok(Rc::new(RefCell::new(*b)) as Rc<RefCell<dyn Any>>),
            Literal::Nil => Ok(Rc::new(RefCell::new(())) as Rc<RefCell<dyn Any>>),
        }
    }

    fn visit_grouping(&self, expr: &Box<Expr>) -> Result<Rc<RefCell<dyn Any>>> {
        Ok(self.evaluate(expr)?)
    }

    fn visit_unary(&self, op: &Token, expr: &Box<Expr>) -> Result<Rc<RefCell<dyn Any>>> {
        let right = self.evaluate(expr)?;
        match op.token_type {
            TokenType::BANG => return Ok(Rc::new(RefCell::new(!self.is_truthy(right)))),
            TokenType::MINUS => {
                self.check_number_operand(op, &right)?;
                return Ok(Rc::new(RefCell::new(
                    -self.evaluate(expr)?.borrow().downcast_ref::<f64>().unwrap(),
                )));
            }
            _ => Err(RunTimeError::new(op.clone(), "Unknown unary operator")),
        }
    }

    fn visit_binary(
        &self,
        left: &Box<Expr>,
        op: &Token,
        right: &Box<Expr>,
    ) -> Result<Rc<RefCell<dyn Any>>> {
        let left = self.evaluate(left)?;
        let right = self.evaluate(right)?;
        match op.token_type {
            TokenType::GREATER => {
                self.check_number_operand_s(op, &left, &right)?;
                return Ok(Rc::new(RefCell::new(
                    left.borrow().downcast_ref::<f64>().unwrap()
                        > right.borrow().downcast_ref::<f64>().unwrap(),
                )));
            }
            TokenType::GREATEREQUAL => {
                self.check_number_operand_s(op, &left, &right)?;
                return Ok(Rc::new(RefCell::new(
                    left.borrow().downcast_ref::<f64>().unwrap()
                        >= right.borrow().downcast_ref::<f64>().unwrap(),
                )));
            }
            TokenType::LESS => {
                self.check_number_operand_s(op, &left, &right)?;
                return Ok(Rc::new(RefCell::new(
                    left.borrow().downcast_ref::<f64>().unwrap()
                        < right.borrow().downcast_ref::<f64>().unwrap(),
                )));
            }
            TokenType::LESSEQUAL => {
                self.check_number_operand_s(op, &left, &right)?;
                return Ok(Rc::new(RefCell::new(
                    left.borrow().downcast_ref::<f64>().unwrap()
                        <= right.borrow().downcast_ref::<f64>().unwrap(),
                )));
            }
            TokenType::BANGEQUAL => {
                return Ok(Rc::new(RefCell::new(
                    left.borrow().downcast_ref::<f64>().unwrap()
                        != right.borrow().downcast_ref::<f64>().unwrap(),
                )));
            }
            TokenType::EQUALEQUAL => {
                return Ok(Rc::new(RefCell::new(self.is_equal(left, right))));
            }
            TokenType::MINUS => {
                self.check_number_operand_s(op, &left, &right)?;
                return Ok(Rc::new(RefCell::new(
                    left.borrow().downcast_ref::<f64>().unwrap()
                        - right.borrow().downcast_ref::<f64>().unwrap(),
                )));
            }
            TokenType::PLUS => {
                if left.borrow().is::<f64>() && right.borrow().is::<f64>() {
                    return Ok(Rc::new(RefCell::new(
                        left.borrow().downcast_ref::<f64>().unwrap()
                            + right.borrow().downcast_ref::<f64>().unwrap(),
                    )));
                }
                if left.borrow().is::<String>() && right.borrow().is::<String>() {
                    return Ok(Rc::new(RefCell::new(format!(
                        "{}{}",
                        left.borrow().downcast_ref::<String>().unwrap(),
                        right.borrow().downcast_ref::<String>().unwrap()
                    ))));
                }

                if left.borrow().is::<String>() && right.borrow().is::<f64>() {
                    return Ok(Rc::new(RefCell::new(format!(
                        "{}{}",
                        left.borrow().downcast_ref::<String>().unwrap(),
                        right.borrow().downcast_ref::<f64>().unwrap()
                    ))));
                }
                if left.borrow().is::<f64>() && right.borrow().is::<String>() {
                    return Ok(Rc::new(RefCell::new(format!(
                        "{}{}",
                        left.borrow().downcast_ref::<f64>().unwrap(),
                        right.borrow().downcast_ref::<String>().unwrap()
                    ))));
                }
                return Err(RunTimeError::new(
                    op.clone(),
                    "Operands not compatible for addition/concatenation",
                ));
            }
            TokenType::SLASH => {
                self.check_number_operand_s(op, &left, &right)?;
                if right.borrow().downcast_ref::<f64>().unwrap() == &0.0 {
                    return Err(RunTimeError::new(
                        op.clone(),
                        "Division by zero is not allowed.",
                    ));
                }
                return Ok(Rc::new(RefCell::new(
                    left.borrow().downcast_ref::<f64>().unwrap()
                        / right.borrow().downcast_ref::<f64>().unwrap(),
                )));
            }
            TokenType::STAR => Ok(Rc::new(RefCell::new(
                left.borrow().downcast_ref::<f64>().unwrap()
                    * right.borrow().downcast_ref::<f64>().unwrap(),
            ))),
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
    ) -> Result<Rc<RefCell<dyn Any>>> {
        let cond = self.evaluate(cond)?;
        if self.is_truthy(cond) {
            return self.evaluate(then_expr);
        }
        return self.evaluate(else_expr);
    }

    fn visit_var(&self, name: &Token) -> Result<Rc<RefCell<dyn Any>>> {
        return self.environment.get(name.clone());
    }

    fn visit_assign(&self, name: &Token, expr: &Box<Expr>) -> Result<Rc<RefCell<dyn Any>>> {
        let value = self.evaluate(expr)?;
        self.environment.assign(name.clone(), value.clone())?;
        return Ok(value);
    }
}

impl StmtVisitor<Result<()>> for Interpreter {
    fn visit_expression(&self, expr: &Box<Expr>) -> Result<()> {
        self.evaluate(expr)?;
        Ok(())
    }

    fn visit_print(&self, expr: &Box<Expr>) -> Result<()> {
        let res = self.evaluate(expr)?;
        println!("{}", self.stringify(res));
        Ok(())
    }

    fn visit_var(&self, name: &Token, initializer: &Option<Box<Expr>>) -> Result<()> {
        if initializer.is_none() {
            return Ok(());
        }
        let initializer = initializer.as_ref().unwrap();
        let value = self.evaluate(&initializer)?;
        self.environment.define(name.clone(), value);
        Ok(())
    }

    fn visit_block(&mut self, statements: &Vec<Stmt>) -> Result<()> {
        let block_env = Rc::new(Environment::new(Some(Rc::clone(&self.environment))));
        self.execute_block(statements, block_env)?;
        Ok(())
    }
}
