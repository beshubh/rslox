/*
/// Grammer for Lox (minimal)
/// expression     -> equality;
/// equality       -> comparison (( "!=" | "==" )  comparison)* ; // allowing expression like a == b, a != b, a == b != c
/// comparison     -> term ((">" | ">=" | "<" | "<=") term)* ;
/// term           ->  factor (("+" | "-") factor)* // allow a + b, a - b, a + b / c
/// factor         ->  unary ( ("/" | "*") unary)* ;
/// unary          -> ("!", "-") unary | primary ;
/// primary        -> NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")";
*/

use crate::ast::Expr;
use crate::token::{Token, TokenType};

pub struct Parser {
    pub tokens: Vec<Token>,
    pub current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, current: 0 }
    }

    fn expression(&mut self) -> Expr {
        return self.equality();
    }

    fn equality(&mut self) -> Expr {
        let mut expr = self.comparison();

        while self.match_type(&[TokenType::BANGEQUAL, TokenType::EQUALEQUAL]) {
            let operator = self.previous();
            let right = self.comparison();
            expr = Expr::Binary(Box::new(expr), operator.clone(), Box::new(right));
        }
        expr
    }

    fn comparison(&mut self) -> Expr {}

    fn match_type(&mut self, token_types: &[TokenType]) -> bool {
        for ttype in token_types {
            if self.check(ttype) {
                self.advance();
                return true;
            }
        }
        false
    }

    fn check(&self, ttype: &TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }
        self.peek().token_type == *ttype
    }
    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        return self.previous();
    }

    fn previous(&self) -> &Token {
        return &self.tokens[self.current - 1];
    }

    fn is_at_end(&self) -> bool {
        return self.peek().token_type == TokenType::EOF;
    }

    fn peek(&self) -> &Token {
        return &self.tokens[self.current];
    }
}
