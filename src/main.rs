#![feature(let_chains)]

use parser::Parser;
pub mod lexer;
pub mod parser;

fn main() {
    let file = std::fs::read_to_string("./testing/main.cyl").unwrap();
    let chars = file.chars().collect::<Vec<char>>();
    let mut lexer = lexer::Lexer::init(chars);
    lexer.all();

    let mut parser: Parser = lexer.into();
    println!("{:#?}", parser.parse());
}
