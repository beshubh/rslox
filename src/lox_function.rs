use crate::environment::Environment;
use crate::interpreter::Interpreter;
use crate::interpreter::InterpreterError;
use crate::interpreter::Result;
use crate::token::Token;
use crate::{lox_callable::LoxCallable, statement::Stmt};
use core::fmt;
use std::any::Any;
use std::cell::RefCell;
use std::rc::Rc;

pub struct LoxFuntion {
    name: Token,
    params: Vec<Token>,
    body: Vec<Stmt>,
}

impl LoxFuntion {
    pub fn new(name: Token, params: Vec<Token>, body: Vec<Stmt>) -> Self {
        LoxFuntion { name, params, body }
    }
}

impl LoxCallable for LoxFuntion {
    fn arity(&self) -> usize {
        self.params.len()
    }

    fn call(
        &self,
        interpreter: &Interpreter,
        arguments: Vec<Rc<RefCell<dyn Any>>>,
    ) -> Result<Rc<RefCell<dyn Any>>> {
        let environment = Environment::new(Some(interpreter.globals.clone()));
        for (i, param) in self.params.iter().enumerate() {
            environment.define(param.lexeme.clone(), arguments.get(i).unwrap().clone());
        }
        let res = interpreter.execute_block(&self.body, Rc::new(environment));
        // BUG: there is a bug in handling return values, i don't know if its in this code or in interpreter but there is a bug
        // after bit of run through, the bug is in lexical scoping & variable modification, in recursion, the original parameter
        // passed to the function itself gets modified after recursive call finishes.
        if let Err(e) = res {
            match e {
                InterpreterError::Return(val) => {
                    if let Some(v) = val.value {
                        eprintln!("DEBUG: return some {:?}", v);
                        return Ok(v);
                    } else {
                        eprintln!("DEBUG: return none");
                        return Ok(
                            Rc::new(RefCell::new(Option::<()>::None)) as Rc<RefCell<dyn Any>>
                        );
                    }
                }
                _ => {
                    return Err(e);
                }
            }
        }
        Ok(Rc::new(RefCell::new(Option::<()>::None)) as Rc<RefCell<dyn Any>>)
    }
}

impl fmt::Display for LoxFuntion {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<fn {} >", self.name.lexeme)
    }
}
