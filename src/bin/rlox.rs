use clap::Parser;
use std::io::{self, BufRead, Write};

use rslox::scanner::Scanner;

#[derive(Parser, Debug, Clone)]
#[command(author = "Shubh")]
#[command(version = env!("CARGO_PKG_VERSION"))]
#[command(name= env!("CARGO_PKG_NAME"))]
#[command(about = env!("CARGO_PKG_DESCRIPTION"))]
struct Cli {
    script: Option<String>,
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();
    let mut lox = Lox { had_error: false };
    match cli.script {
        Some(script) => lox.run_file(&script),
        None => lox.run_prompt(),
    }
}

struct Lox {
    had_error: bool,
}

impl Lox {
    fn run_file(&self, path: &str) -> anyhow::Result<()> {
        let contents = std::fs::read_to_string(path)?;
        self.run(contents)?;
        if self.had_error {
            std::process::exit(69);
        }
        Ok(())
    }

    fn run_prompt(&mut self) -> anyhow::Result<()> {
        let stdin = io::stdin();
        let mut reader = stdin.lock();
        let mut line = String::new();
        loop {
            println!("rlox> ");
            io::stdout().flush()?;
            if reader.read_line(&mut line)? == 0 {
                break;
            }
            self.run(line.to_owned())?;
            self.had_error = false;
            line.clear();
        }
        Ok(())
    }

    fn run(&self, source: String) -> anyhow::Result<()> {
        let scanner = Scanner::new(source);
        let tokens = scanner.scanTokens();
        for token in tokens {
            println!("{}", token);
        }
        Ok(())
    }

    fn error(&mut self, line: i64, message: String) {
        report(line, "".to_string(), message);
        self.had_error = true;
    }
}

fn report(line: i64, where_: String, message: String) {
    eprintln!("[line: {}] Error {}:{}", line, where_, message);
}
