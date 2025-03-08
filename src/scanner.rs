use crate::token::*;

pub struct Scanner {
    source: String,
}

impl Scanner {
    pub fn new(source: String) -> Self {
        Self { source }
    }

    pub fn scanTokens(&self) -> Vec<String> {
        vec![]
    }

    fn is_at_end(&self) -> bool {
        true
    }
}
