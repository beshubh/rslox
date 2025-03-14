use clap::Parser;
use rslox::{
    ast::Expr,
    ast_printer,
    lox::Lox,
    token::{Literal, Token, TokenType},
};

#[derive(Parser, Debug, Clone)]
#[command(author = "Shubh")]
#[command(version = env!("CARGO_PKG_VERSION"))]
#[command(name= env!("CARGO_PKG_NAME"))]
#[command(about = env!("CARGO_PKG_DESCRIPTION"))]
struct Cli {
    script: Option<String>,
}

fn main() -> anyhow::Result<()> {
    // let cli = Cli::parse();
    // let mut lox = Lox {};
    // match cli.script {
    //     Some(script) => lox.run_file(&script),
    //     None => lox.run_prompt(),
    // }
    let expr = Expr::Binary(
        Box::new(Expr::Unary(
            Token::new(TokenType::MINUS, String::from("-"), None, 1),
            Box::new(Expr::Literal(Literal::Number(123.0))),
        )),
        Token::new(TokenType::STAR, String::from("*"), None, 1),
        Box::new(Expr::Grouping(Box::new(Expr::Literal(Literal::Number(
            45.67,
        ))))),
    );
    println!("{}", ast_printer::print_ast(expr));
    Ok(())
}
