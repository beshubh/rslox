use crate::interpreter::RunTimeError;
use crate::token::Token;
use std::any::Any;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub struct Environment {
    values: HashMap<String, Rc<RefCell<dyn Any>>>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            values: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: String, value: Rc<RefCell<dyn Any>>) {
        self.values.insert(name, value);
    }

    pub fn get(&self, name: Token) -> Result<Rc<RefCell<dyn Any>>, RunTimeError> {
        match self.values.get(&name.lexeme) {
            Some(value) => {
                let value = value.clone();
                return Ok(value);
            }
            None => Err(RunTimeError::new(
                name.clone(),
                &format!("Undefined variable '{}'.", name.lexeme),
            )),
        }
    }
}
