/*
/// Grammar for Lox (minimal)
/// comma_expression    -> "(" expression ("," expression)* ")";
/// expression          -> equality;
/// equality            -> comparison (( "!=" | "==" )  comparison)* ; // allowing expression like a == b, a != b, a == b != c
/// comparison          -> term ((">" | ">=" | "<" | "<=") term)* ;
/// term                ->  factor (("+" | "-") factor)* ;// allow a + b, a - b, a + b / c
/// factor              ->  unary ( ("/" | "*") unary)* ;
/// unary               -> ("!", "-") unary | primary ;
/// primary             -> NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")";
*/
use crate::ast::Expr;
use crate::lox::Lox;
use crate::token::{self, Token, TokenType};
use std::fmt;

pub struct Parser {
    pub tokens: Vec<Token>,
    pub current: usize,
}

#[derive(Debug)]
pub struct ParseError;

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Parse error")
    }
}

impl std::error::Error for ParseError {}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, current: 0 }
    }

    pub fn parse(&mut self) -> Option<Expr> {
        if let Ok(expr) = self.comma_expression() {
            return Some(expr);
        } else {
            None
        }
    }

    fn comma_expression(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.expression()?;
        // 1 + 2, 3 - 4, a + b, c + d
        while self.match_type(&[TokenType::COMMA]) {
            let operator = self.previous().clone();
            let right = self.expression()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }
        return Ok(expr);
    }

    fn expression(&mut self) -> Result<Expr, ParseError> {
        self.equality()
    }

    fn equality(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.comparison()?;
        while self.match_type(&[TokenType::BANGEQUAL, TokenType::EQUALEQUAL]) {
            let operator = self.previous().clone();
            let right = self.comparison()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }
        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.term()?;

        while self.match_type(&[
            TokenType::GREATER,
            TokenType::GREATEREQUAL,
            TokenType::LESS,
            TokenType::LESSEQUAL,
        ]) {
            let operator = self.previous().clone();
            let right = self.term()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }
        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.factor()?;

        while self.match_type(&[TokenType::PLUS, TokenType::MINUS]) {
            let operator = self.previous().clone();
            let right = self.factor()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }
        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.unary()?;
        while self.match_type(&[TokenType::SLASH, TokenType::STAR]) {
            let operator = self.previous().clone();
            let right = self.unary()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }
        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, ParseError> {
        if self.match_type(&[TokenType::BANG, TokenType::MINUS]) {
            let operator = self.previous().clone();
            let right = self.unary()?;
            return Ok(Expr::Unary(operator, Box::new(right)));
        }
        self.primary()
    }

    fn primary(&mut self) -> Result<Expr, ParseError> {
        if self.match_type(&[TokenType::FALSE]) {
            return Ok(Expr::Literal(token::Literal::Boolean(false)));
        }
        if self.match_type(&[TokenType::TRUE]) {
            return Ok(Expr::Literal(token::Literal::Boolean(true)));
        }
        if self.match_type(&[TokenType::NIL]) {
            return Ok(Expr::Literal(token::Literal::Nil));
        }

        if self.match_type(&[TokenType::NUMBER, TokenType::STRING]) {
            return Ok(Expr::Literal(
                self.previous().literal.as_ref().unwrap().clone(),
            ));
        }

        if self.match_type(&[TokenType::LEFTPAREN]) {
            let expr = self.expression()?;
            self.consume(&TokenType::RIGHTPAREN, "Expect ')' after expression.")?;
            return Ok(Expr::Grouping(Box::new(expr)));
        }

        Err(self.error(self.peek(), "Expect expression."))
    }

    fn consume(&mut self, ttype: &TokenType, message: &str) -> Result<&Token, ParseError> {
        if self.check(ttype) {
            return Ok(self.advance());
        }
        Err(self.error(self.peek(), message))
    }

    fn error(&self, token: &Token, message: &str) -> ParseError {
        Lox::error_token(token.clone(), message);
        ParseError {}
    }

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
        self.previous()
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }

    fn is_at_end(&self) -> bool {
        self.peek().token_type == TokenType::EOF
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }
}
