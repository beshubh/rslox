use crate::scanner::Scanner;
use std::io::{self, BufRead, Write};

static mut HAD_ERROR: bool = false;

pub struct Lox {}

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
        for token in tokens {
            println!("{}", token);
        }
        Ok(())
    }

    fn had_error() -> bool {
        unsafe { HAD_ERROR }
    }

    fn set_had_error(had_error: bool) {
        unsafe { HAD_ERROR = had_error }
    }

    pub fn error(line: usize, message: String) {
        Self::report(line, "".to_string(), message);
    }

    fn report(line: usize, where_: String, message: String) {
        eprintln!("[line: {}] Error {}:{}", line, where_, message);
        Self::set_had_error(true);
    }
}
