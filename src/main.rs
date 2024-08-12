#![feature(let_chains)]
#![feature(if_let_guard)]

use parser::Parser;
pub mod lexer;
pub mod parser;

fn main() {
    let file = std::fs::read_to_string("./testing/main.cyl").unwrap();
    let chars = file.chars().collect::<Vec<char>>();
    let mut lexer = lexer::Lexer::init(chars);
    lexer.all().unwrap();

    let mut parser: Parser = lexer.into();
    parser.parse().unwrap();
    parser.parse().unwrap();
    println!("{:#?}", parser);
}