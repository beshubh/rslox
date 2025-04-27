use crate::interpreter::Interpreter;
use crate::interpreter::Result;

use std::any::Any;
use std::cell::RefCell;
use std::rc::Rc;

pub trait LoxCallable {
    fn call(
        &self,
        interpreter: &Interpreter,
        arguments: Vec<Rc<RefCell<dyn Any>>>,
    ) -> Result<Rc<RefCell<dyn Any>>>;

    fn arity(&self) -> usize;
}
