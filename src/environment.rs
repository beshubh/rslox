use crate::interpreter::RunTimeError;
use crate::token::Token;
use std::any::Any;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub struct Environment {
    values: RefCell<HashMap<String, Rc<RefCell<dyn Any>>>>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            values: RefCell::new(HashMap::new()),
        }
    }

    pub fn define(&mut self, name: Token, value: Rc<RefCell<dyn Any>>) {
        self.values.borrow_mut().insert(name.lexeme, value);
    }

    pub fn assign(&self, name: Token, value: Rc<RefCell<dyn Any>>) -> Result<(), RunTimeError> {
        let mut map = self.values.borrow_mut();
        if map.contains_key(&name.lexeme) {
            map.insert(name.lexeme, value);
            return Ok(());
        }
        Err(RunTimeError::new(
            name.clone(),
            &format!("Undefined variable '{}'.", name.lexeme),
        ))
    }

    pub fn get(&self, name: Token) -> Result<Rc<RefCell<dyn Any>>, RunTimeError> {
        match self.values.borrow().get(&name.lexeme) {
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
