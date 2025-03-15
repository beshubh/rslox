use phf::phf_map;

use crate::lox::Lox;
use crate::token::*;

pub struct Scanner {
    source_chars: Vec<char>,
    start: usize,
    current: usize,
    line: usize,
    tokens: Vec<Token>,
}

static KEYWORDS: phf::Map<&'static str, TokenType> = phf_map! {
    "and" => TokenType::AND,
    "class" => TokenType::CLASS,
    "else" => TokenType::ELSE,
    "false" => TokenType::FALSE,
    "fun" => TokenType::FUN,
    "for" => TokenType::FOR,
    "if" => TokenType::IF,
    "nil" => TokenType::NIL,
    "or" => TokenType::OR,
    "print" => TokenType::PRINT,
    "return" => TokenType::RETURN,
    "super" => TokenType::SUPER,
    "this" => TokenType::THIS,
    "true" => TokenType::TRUE,
    "var" => TokenType::VAR,
    "while" => TokenType::WHILE,
};

impl Scanner {
    pub fn new(source: String) -> Self {
        let chars = source.chars().collect();
        Self {
            start: 0,
            current: 0,
            line: 1,
            source_chars: chars,
            tokens: vec![],
        }
    }

    pub fn scan_tokens(&mut self) -> Vec<Token> {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }
        self.tokens
            .push(Token::new(TokenType::EOF, "".to_string(), None, self.line));
        self.tokens.clone()
    }

    fn scan_token(&mut self) {
        let c = self.advance();
        match c {
            '(' => self.add_token(TokenType::LEFTPAREN),
            ')' => self.add_token(TokenType::RIGHTPAREN),
            '{' => self.add_token(TokenType::LEFTBRACE),
            '}' => self.add_token(TokenType::RIGHTBRACE),
            ',' => self.add_token(TokenType::COMMA),
            '.' => self.add_token(TokenType::DOT),
            '-' => self.add_token(TokenType::MINUS),
            '+' => self.add_token(TokenType::PLUS),
            ';' => self.add_token(TokenType::SEMICOLON),
            '*' => self.add_token(TokenType::STAR),
            '?' => self.add_token(TokenType::QUESTIONMARK),
            ':' => self.add_token(TokenType::COLON),
            '!' => {
                let ttype = if self.match_char('=') {
                    TokenType::BANGEQUAL
                } else {
                    TokenType::BANG
                };

                self.add_token(ttype);
            }
            '=' => {
                let ttype = if self.match_char('=') {
                    TokenType::EQUALEQUAL
                } else {
                    TokenType::EQUAL
                };

                self.add_token(ttype);
            }
            '<' => {
                let ttype = if self.match_char('=') {
                    TokenType::LESSEQUAL
                } else {
                    TokenType::LESS
                };

                self.add_token(ttype);
            }
            '>' => {
                let ttype = if self.match_char('=') {
                    TokenType::GREATEREQUAL
                } else {
                    TokenType::GREATER
                };

                self.add_token(ttype);
            }
            '/' => {
                if self.match_char('/') {
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                } else if self.match_char('*') {
                    loop {
                        if self.is_at_end() {
                            break;
                        }
                        if self.peek() == '*' && self.peek_next() == '/' {
                            self.advance();
                            self.advance();
                            break;
                        }
                        if self.peek() == '\n' {
                            self.line += 1;
                        }
                        self.advance();
                    }
                } else {
                    self.add_token(TokenType::SLASH);
                }
            }
            ' ' | '\r' | '\t' => {}
            '\n' => {
                self.line += 1;
            }
            '"' => self.scan_string(),
            _ => {
                if self.is_digit(c) {
                    self.scan_number();
                } else if self.is_alpha(c) {
                    self.scan_identifier();
                } else {
                    Lox::error(self.line, "Unexpected character.");
                }
            }
        }
    }

    fn match_char(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }
        if self.source_chars[self.current] != expected {
            return false;
        }
        self.current += 1;
        true
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            return '\0';
        }
        return self.source_chars[self.current];
    }

    fn peek_next(&self) -> char {
        if self.current + 1 >= self.source_chars.len() {
            return '\0';
        }
        return self.source_chars[self.current + 1];
    }

    fn is_digit(&self, c: char) -> bool {
        c.is_ascii_digit()
    }

    fn is_alpha(&self, c: char) -> bool {
        return c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c == '_';
    }

    fn is_alphanumeric(&self, c: char) -> bool {
        return self.is_alpha(c) || self.is_digit(c);
    }

    fn scan_string(&mut self) {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }
        if self.is_at_end() {
            Lox::error(self.line, "Unterminated string.");
            return;
        }
        // closing "
        self.advance();

        let value = self.source_chars[self.start + 1..self.current - 1]
            .iter()
            .collect::<String>();
        self.add_token_with_literal(TokenType::STRING, Some(Literal::String(value)));
    }

    fn scan_number(&mut self) {
        while self.is_digit(self.peek()) {
            self.advance();
        }
        if self.peek() == '.' && self.is_digit(self.peek_next()) {
            self.advance();
            while self.is_digit(self.peek()) {
                self.advance();
            }
        }

        let value = self.source_chars[self.start..self.current]
            .iter()
            .collect::<String>();
        self.add_token_with_literal(
            TokenType::NUMBER,
            Some(Literal::Number(value.parse::<f64>().unwrap())),
        );
    }

    fn scan_identifier(&mut self) {
        while self.is_alphanumeric(self.peek()) {
            self.advance();
        }
        let text = self.source_chars[self.start..self.current]
            .iter()
            .collect::<String>();
        if let Some(ttype) = KEYWORDS.get(&text) {
            self.add_token(ttype.clone());
        } else {
            self.add_token(TokenType::IDENTIFIER);
        }
    }

    fn advance(&mut self) -> char {
        let res = self.source_chars[self.current];
        self.current += 1;
        res
    }

    fn add_token(&mut self, ttype: TokenType) {
        self.add_token_with_literal(ttype, None);
    }

    fn add_token_with_literal(&mut self, ttype: TokenType, literal: Option<Literal>) {
        let text = self.source_chars[self.start..self.current]
            .iter()
            .collect::<String>();

        self.tokens
            .push(Token::new(ttype, text, literal, self.line));
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source_chars.len()
    }
}
