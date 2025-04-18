use crate::{
    ast_printer,
    interpreter::{self, RunTimeError},
    parser::Parser,
    scanner::Scanner,
    token::{Token, TokenType},
};
use core::str;
use std::io::{self, BufRead, Write};

static mut HAD_ERROR: bool = false;
static mut HAD_RUNTIME_ERROR: bool = false;

pub struct Lox {
    pub interpreter: interpreter::Interpreter,
}

impl Lox {
    pub fn run_file(&self, path: &str) -> anyhow::Result<()> {
        let contents = std::fs::read_to_string(path)?;
        self.run(contents)?;
        if Lox::had_error() {
            std::process::exit(69);
        }
        Ok(())
    }

    pub fn run_prompt(&mut self) -> anyhow::Result<()> {
        let stdin = io::stdin();
        let mut reader = stdin.lock();
        let mut line = String::new();
        loop {
            print!("rlox> ");
            io::stdout().flush()?;
            if reader.read_line(&mut line)? == 0 {
                break;
            }
            self.run(line.to_owned())?;
            Lox::set_had_error(false);
            line.clear();
        }
        Ok(())
    }

    fn run(&self, source: String) -> anyhow::Result<()> {
        let mut scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens();
        let mut parser = Parser::new(tokens);
        let expr = parser.parse();
        if Self::had_error() {
            return Err(anyhow::Error::msg("Error in parsing"));
        }
        if Self::had_runtime_error() {
            return Err(anyhow::Error::msg("Runtime error"));
        }
        let expr = expr.unwrap();
        // let printer = ast_printer::AstPrinter {};
        // println!("{}", printer.print(&expr));
        self.interpreter.interpret(&Box::new(expr));
        Ok(())
    }

    fn had_error() -> bool {
        unsafe { HAD_ERROR }
    }

    fn had_runtime_error() -> bool {
        unsafe { HAD_RUNTIME_ERROR }
    }

    fn set_had_error(had_error: bool) {
        unsafe { HAD_ERROR = had_error }
    }

    pub fn error(line: usize, message: &str) {
        Self::report(line, "", message);
    }

    fn report(line: usize, where_: &str, message: &str) {
        eprintln!("[line: {}] Error {}:{}", line, where_, message);
        Self::set_had_error(true);
    }

    pub fn error_token(token: Token, message: &str) {
        if token.token_type == TokenType::EOF {
            Self::report(token.line, "at end", message);
        } else {
            Self::report(token.line, &format!(" at '{}'", token.lexeme), message);
        }
    }
    pub fn runtime_error(err: RunTimeError) {
        eprintln!("{} \n [line {} ]", err, err.token.line);
        unsafe {
            HAD_RUNTIME_ERROR = true;
        }
    }
}
