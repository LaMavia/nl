use std::{error::Error, io::stdin};

mod ast;
mod eval;
mod lexer;
mod position;

use ast::Parser;
use lexer::Lexer;

use serde;
use serde_json;

fn main() -> Result<(), Box<dyn Error>> {
    let source = "
    (let fact 
        (λ (n) 
            ( (= n 0)
                (1)
                (* n (fact (- n 1)))
            )
        )
        (fact 4)
    )
    ".to_string();

    let mut lexer = Lexer::new(source.to_owned());
    lexer.lex();

    println!("lexems:");
    for l in lexer.lexems() {
        println!("{:?}", l);
    }
    print!("\n");

    let tree = Parser::run(&lexer.lexems())?;

    println!("source: {}", source);
    println!("ast: {}", serde_json::to_string_pretty(&tree)?);
    println!("print: {}", tree);
    println!("eval: {}", eval::Evaluator::new().eval(tree)?);

    Ok(())
}
