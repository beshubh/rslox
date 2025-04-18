use clap::Parser;
use rslox::lox::Lox;

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
    let mut lox = Lox {
        interpreter: rslox::interpreter::Interpreter {},
    };
    match cli.script {
        Some(script) => lox.run_file(&script),
        None => lox.run_prompt(),
    }
}
