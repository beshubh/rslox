use crate::interpreter::RunTimeError;
use crate::token::Token;
use std::any::Any;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub struct Environment {
    values: RefCell<HashMap<String, Rc<RefCell<dyn Any>>>>,
    enclosing: Option<Rc<Environment>>,
}

impl Environment {
    pub fn new(enclosing: Option<Rc<Environment>>) -> Self {
        Environment {
            values: RefCell::new(HashMap::new()),
            enclosing,
        }
    }

    pub fn define(&self, name: String, value: Rc<RefCell<dyn Any>>) {
        self.values.borrow_mut().insert(name, value);
    }

    pub fn assign(&self, name: Token, value: Rc<RefCell<dyn Any>>) -> Result<(), RunTimeError> {
        let mut map = self.values.borrow_mut();
        if map.contains_key(&name.lexeme) {
            map.insert(name.lexeme, value);
            return Ok(());
        }
        if self.enclosing.is_some() {
            return self.enclosing.clone().unwrap().assign(name, value);
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
            None => {
                if let Some(enclosing) = &self.enclosing {
                    return enclosing.clone().get(name);
                }
                Err(RunTimeError::new(
                    name.clone(),
                    &format!("Undefined variable '{}'.", name.lexeme),
                ))
            }
        }
    }
}
