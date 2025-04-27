use crate::ast::{Expr, ExprVisitor};
use crate::environment::Environment;
use crate::lox::Lox;
use crate::lox_callable::LoxCallable;
use crate::lox_function::LoxFuntion;
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

pub type Result<T> = std::result::Result<T, RunTimeError>;

pub struct Interpreter {
    pub globals: Rc<Environment>,
    pub environment: RefCell<Rc<Environment>>,
}

impl Interpreter {
    pub fn new(environment: Rc<Environment>) -> Self {
        let globals = environment.clone();
        let environment = RefCell::new(environment.clone());

        struct NativeClockFunction;
        impl LoxCallable for NativeClockFunction {
            fn arity(&self) -> usize {
                return 0;
            }

            fn call(
                &self,
                _interpreter: &Interpreter,
                _arguments: Vec<Rc<RefCell<dyn Any>>>,
            ) -> Result<Rc<RefCell<dyn Any>>> {
                let now = std::time::SystemTime::now()
                    .duration_since(std::time::UNIX_EPOCH)
                    .expect("Time went backwards")
                    .as_millis();

                return Ok(Rc::new(RefCell::new(now)) as Rc<RefCell<dyn Any>>);
            }
        }
        impl fmt::Display for NativeClockFunction {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "<native_fn>")
            }
        }

        globals.define(
            "clock".to_string(),
            Rc::new(RefCell::new(
                Box::new(NativeClockFunction) as Box<dyn LoxCallable>
            )) as Rc<RefCell<dyn Any>>,
        );
        Interpreter {
            globals,
            environment,
        }
    }

    pub fn interpret(&self, statements: &Vec<Stmt>) {
        for stmt in statements {
            let res = self.execute(stmt);
            if let Err(err) = res {
                Lox::runtime_error(err);
                return;
            }
        }
    }

    pub fn execute(&self, stmt: &Stmt) -> Result<()> {
        stmt.accept(self)
    }

    pub fn execute_block(
        &self,
        statements: &Vec<Stmt>,
        block_environment: Rc<Environment>,
    ) -> Result<()> {
        let previous = self.environment.borrow().clone();
        self.environment.replace(block_environment);

        for stmt in statements {
            self.execute(stmt)?;
        }
        self.environment.replace(previous);
        Ok(())
    }

    fn evaluate(&self, expr: &Box<Expr>) -> Result<Rc<RefCell<dyn Any>>> {
        expr.accept(self)
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
        obj.downcast_ref::<String>().unwrap().to_string()
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
        true
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
        false
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
            Literal::Nil => Ok(Rc::new(RefCell::new(Option::<()>::None)) as Rc<RefCell<dyn Any>>),
        }
    }

    fn visit_logical(
        &self,
        left: &Box<Expr>,
        logical_op: &Token,
        right: &Box<Expr>,
    ) -> Result<Rc<RefCell<dyn Any>>> {
        let left = self.evaluate(left)?;
        if logical_op.token_type == TokenType::OR {
            if self.is_truthy(left.clone()) {
                return Ok(left);
            }
        } else {
            if !self.is_truthy(left.clone()) {
                return Ok(left);
            }
        }
        self.evaluate(right)
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
        self.evaluate(else_expr)
    }

    fn visit_var(&self, name: &Token) -> Result<Rc<RefCell<dyn Any>>> {
        self.environment.borrow().get(name.clone())
    }

    fn visit_assign(&self, name: &Token, expr: &Box<Expr>) -> Result<Rc<RefCell<dyn Any>>> {
        let value = self.evaluate(expr)?;
        self.environment
            .borrow()
            .assign(name.clone(), value.clone())?;
        Ok(value)
    }

    fn visit_call(
        &self,
        callee: &Box<Expr>,
        paren: &Token,
        arguments_expr: &Vec<Expr>,
    ) -> Result<Rc<RefCell<dyn Any>>> {
        let callee = self.evaluate(callee)?;
        let mut arguments = vec![];
        for argument in arguments_expr {
            let boxed_arg = Box::new(argument.clone());
            arguments.push(self.evaluate(&boxed_arg)?);
        }
        let callee = callee.borrow();
        if !callee.is::<Box<dyn LoxCallable>>() {
            return Err(RunTimeError::new(
                paren.clone(),
                "Can only call functions and classes.",
            ));
        }
        let func = callee.downcast_ref::<Box<dyn LoxCallable>>().unwrap();
        if arguments.len() != func.arity() {
            return Err(RunTimeError::new(
                paren.clone(),
                &format!(
                    "Expected {} arguments but got {}.",
                    func.arity(),
                    arguments.len()
                ),
            ));
        }
        func.call(&self, arguments)
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
            self.environment.borrow_mut().define(
                name.lexeme.clone(),
                Rc::new(RefCell::new(Option::<()>::None)),
            );
            return Ok(());
        }
        let initializer = initializer.as_ref().unwrap();
        let value = self.evaluate(&initializer)?;
        self.environment
            .borrow_mut()
            .define(name.lexeme.clone(), value);
        Ok(())
    }

    fn visit_block(&self, statements: &Vec<Stmt>) -> Result<()> {
        let block_env = Rc::new(Environment::new(Some(Rc::clone(
            &self.environment.borrow(),
        ))));
        self.execute_block(statements, block_env)?;
        Ok(())
    }

    fn visit_if(
        &self,
        condition: &Box<Expr>,
        then_branch: &Box<Stmt>,
        else_branch: &Option<Box<Stmt>>,
    ) -> Result<()> {
        let cond = self.evaluate(condition)?;
        if self.is_truthy(cond) {
            self.execute(&then_branch)?;
        } else if else_branch.is_some() {
            self.execute(else_branch.as_ref().unwrap())?;
        }
        Ok(())
    }

    fn visit_while(&self, condition: &Box<Expr>, body: &Box<Stmt>) -> Result<()> {
        while self.is_truthy(self.evaluate(condition)?) {
            self.execute(body)?;
        }
        Ok(())
    }

    fn visit_function(
        &self,
        name: &Token,
        parameters: &Vec<Token>,
        body: &Vec<Stmt>,
    ) -> Result<()> {
        let function = LoxFuntion::new(name.clone(), parameters.clone(), body.clone());
        let val = Rc::new(RefCell::new(Box::new(function) as Box<dyn LoxCallable>))
            as Rc<RefCell<dyn Any>>;

        self.environment
            .borrow_mut()
            .define(name.lexeme.clone(), val);
        Ok(())
    }
}
