use clap::Parser;
use rslox::{environment, lox::Lox};
use std::rc::Rc;

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
    let interpreter =
        rslox::interpreter::Interpreter::new(Rc::new(environment::Environment::new(None)));
    let mut lox = Lox { interpreter };
    match cli.script {
        Some(script) => lox.run_file(&script),
        None => lox.run_prompt(),
    }
}
