use crate::ast::Expr;
use crate::lox::Lox;
use crate::statement::Stmt;
use crate::token::{self, Token, TokenType};
use std::{fmt, vec};

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

    fn synchronize(&mut self) {
        self.advance();

        while !self.is_at_end() {
            if self.previous().token_type == TokenType::SEMICOLON {
                return;
            }
            match self.peek().token_type {
                TokenType::CLASS
                | TokenType::FUN
                | TokenType::VAR
                | TokenType::FOR
                | TokenType::IF
                | TokenType::WHILE
                | TokenType::PRINT
                | TokenType::RETURN => {
                    return;
                }
                _ => {}
            }
        }
        self.advance();
    }

    pub fn parse(&mut self) -> Vec<Stmt> {
        let mut statements = vec![];
        while !self.is_at_end() {
            let stmt = self.decleration();
            if let Some(stmt) = stmt {
                statements.push(stmt);
            }
        }
        return statements;
    }

    fn decleration(&mut self) -> Option<Stmt> {
        if self.match_type(&[TokenType::FUN]) {
            let res = self.function("function");
            if let Err(_) = res {
                self.synchronize();
                return None;
            }
            return Some(res.unwrap());
        }

        if self.match_type(&[TokenType::VAR]) {
            let res = self.var_decl();
            if let Err(_) = res {
                self.synchronize();
                return None;
            }
            return Some(res.unwrap());
        }

        let res = self.statement();
        if let Err(_) = res {
            self.synchronize();
            return None;
        }
        return Some(res.unwrap());
    }

    fn function(&mut self, kind: &str) -> Result<Stmt, ParseError> {
        let name = self
            .consume(&TokenType::IDENTIFIER, &format!("Expect {} name.", kind))?
            .clone();
        self.consume(
            &TokenType::LEFTPAREN,
            &format!("Expect '(' after {} name.", kind),
        )?;
        let mut parameters = vec![];
        if !self.check(&TokenType::RIGHTPAREN) {
            loop {
                if parameters.len() >= 255 {
                    self.error(self.peek(), "Can't have more than 255 parameters.");
                }
                let param = self
                    .consume(&TokenType::IDENTIFIER, "Expect parameter name.")?
                    .clone();
                parameters.push(param);
                if !self.match_type(&[TokenType::COMMA]) {
                    break;
                }
            }
        }
        self.consume(
            &TokenType::RIGHTPAREN,
            &format!("Expect ')' after {} name.", kind),
        )?;
        self.consume(
            &TokenType::LEFTBRACE,
            &format!("Expect '{{' after {} body.", kind),
        )?;

        let body = self.block()?;
        Ok(Stmt::Function(name, parameters.clone(), body))
    }

    fn var_decl(&mut self) -> Result<Stmt, ParseError> {
        let name = self
            .consume(&TokenType::IDENTIFIER, "Expect variable name.")
            .cloned()?;
        let mut initializer: Option<Box<Expr>> = None;

        if self.match_type(&[TokenType::EQUAL]) {
            initializer = Some(Box::new(self.expression()?));
        }

        self.consume(
            &TokenType::SEMICOLON,
            "Expect ';' after variable declaration.",
        )?;
        Ok(Stmt::Var(name, initializer))
    }

    fn statement(&mut self) -> Result<Stmt, ParseError> {
        if self.match_type(&[TokenType::FOR]) {
            return self.for_statement();
        }
        if self.match_type(&[TokenType::IF]) {
            return self.if_statement();
        }
        if self.match_type(&[TokenType::PRINT]) {
            return self.print_statement();
        }

        if self.match_type(&[TokenType::WHILE]) {
            return self.while_statement();
        }
        if self.match_type(&[TokenType::LEFTBRACE]) {
            return Ok(Stmt::Block(self.block()?));
        }

        return self.expression_statement();
    }

    fn for_statement(&mut self) -> Result<Stmt, ParseError> {
        // de-sugaring for loop to the while loop;

        self.consume(&TokenType::LEFTPAREN, "Expect '(' after for.")?;
        let initializer;

        if self.match_type(&[TokenType::SEMICOLON]) {
            initializer = None;
        } else if self.match_type(&[TokenType::VAR]) {
            initializer = Some(self.var_decl()?);
        } else {
            initializer = Some(self.expression_statement()?);
        }
        let mut condition = None;
        if !self.check(&TokenType::SEMICOLON) {
            condition = Some(self.expression()?);
        }
        self.consume(&TokenType::SEMICOLON, "Expect ';' after condition.")?;
        let mut increment = None;
        if !self.check(&TokenType::RIGHTPAREN) {
            increment = Some(self.expression()?);
        }

        self.consume(&TokenType::RIGHTPAREN, "Expect ')' after for clauses.")?;

        let mut body = self.statement()?;
        // make a block with body of for loop and increment expression in the end.
        if increment.is_some() {
            body = Stmt::Block(vec![body, Stmt::Expression(Box::new(increment.unwrap()))]);
        }

        // if condiiton is not provided means its infinite loop.
        if condition.is_none() {
            condition = Some(Expr::Literal(token::Literal::Boolean(true)));
        }
        // de-sugar the for into while using the body of for and condition of for in the while.
        body = Stmt::While(Box::new(condition.unwrap()), Box::new(body));

        // if there is initializer we make a block again that runs the initializer first and then the while loop.
        if initializer.is_some() {
            body = Stmt::Block(vec![initializer.unwrap(), body]);
        }
        Ok(body)
    }

    fn if_statement(&mut self) -> Result<Stmt, ParseError> {
        self.consume(&TokenType::LEFTPAREN, "Expect '(' after 'if'.")?;
        let condition = self.expression()?;
        self.consume(&TokenType::RIGHTPAREN, "Expect ')' after condition.")?;

        let then_branch = self.statement()?;
        let mut else_branch: Option<Box<Stmt>> = None;
        if self.match_type(&[TokenType::ELSE]) {
            else_branch = Some(Box::new(self.statement()?))
        }
        Ok(Stmt::If(
            Box::new(condition),
            Box::new(then_branch),
            else_branch,
        ))
    }

    fn print_statement(&mut self) -> Result<Stmt, ParseError> {
        let value = self.expression()?;
        self.consume(&TokenType::SEMICOLON, "Expect ';' after value.")?;
        Ok(Stmt::Print(Box::new(value)))
    }

    fn while_statement(&mut self) -> Result<Stmt, ParseError> {
        self.consume(&TokenType::LEFTPAREN, "Expect '(' after 'while'.")?;
        let condition = self.expression()?;
        self.consume(&TokenType::RIGHTPAREN, "Expect ')' after condition.")?;
        let body = self.statement()?;
        Ok(Stmt::While(Box::new(condition), Box::new(body)))
    }

    fn block(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut statements = vec![];
        while !self.check(&TokenType::RIGHTBRACE) && !self.is_at_end() {
            let stmt = self.decleration();
            if let Some(stmt) = stmt {
                statements.push(stmt);
            }
        }
        self.consume(&TokenType::RIGHTBRACE, "Expect '}' after block.")?;
        Ok(statements)
    }

    fn expression_statement(&mut self) -> Result<Stmt, ParseError> {
        let value = self.expression()?;
        self.consume(&TokenType::SEMICOLON, "Expect ';' after expression.")?;
        Ok(Stmt::Expression(Box::new(value)))
    }

    fn assignment(&mut self) -> Result<Expr, ParseError> {
        let expr = self.or()?;
        if self.match_type(&[TokenType::EQUAL]) {
            let equals = self.previous().clone();
            let value = self.assignment()?;
            if let Expr::Var(name) = expr {
                return Ok(Expr::Assign(name, Box::new(value)));
            }
            self.error(&equals, "Invalid assignment target.");
        }
        return Ok(expr);
    }

    fn or(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.and()?;
        while self.match_type(&[TokenType::OR]) {
            let operator = self.previous().clone();
            let right = self.and()?;
            expr = Expr::Logical(Box::new(expr), operator, Box::new(right));
        }
        Ok(expr)
    }

    fn and(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.equality()?;
        while self.match_type(&[TokenType::AND]) {
            let operator = self.previous().clone();
            let right = self.equality()?;
            expr = Expr::Logical(Box::new(expr), operator, Box::new(right));
        }
        Ok(expr)
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

    fn ternary(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.equality()?;
        if self.match_type(&[TokenType::QUESTIONMARK]) {
            let then_expr = self.expression()?;
            self.consume(
                &TokenType::COLON,
                "Expect ':' after the branch of ternay expression.",
            )?;
            let else_expr = self.ternary()?;
            expr = Expr::Ternary(Box::new(expr), Box::new(then_expr), Box::new(else_expr));
            return Ok(expr);
        }
        Ok(expr)
    }

    fn expression(&mut self) -> Result<Expr, ParseError> {
        self.assignment()
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
        self.call()
    }

    fn call(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.primary()?;
        loop {
            if self.match_type(&[TokenType::LEFTPAREN]) {
                expr = self.finish_call(expr)?;
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn finish_call(&mut self, callee: Expr) -> Result<Expr, ParseError> {
        let mut arguments = vec![];
        if !self.check(&TokenType::RIGHTPAREN) {
            loop {
                if arguments.len() >= 255 {
                    self.error(self.peek(), "Can't have more than 255 arguments.");
                }
                arguments.push(self.expression()?);
                if !self.match_type(&[TokenType::COMMA]) {
                    break;
                }
            }
        }
        let paren = self.consume(&TokenType::RIGHTPAREN, "Expect ')' after arguments.")?;
        Ok(Expr::Call(Box::new(callee), paren.clone(), arguments))
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

        if self.match_type(&[TokenType::IDENTIFIER]) {
            return Ok(Expr::Var(self.previous().clone()));
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
