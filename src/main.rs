#![feature(let_chains)]
#![feature(if_let_guard)]

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
    println!("{:#?}", parser.symbols);
}

/*

a_func_that_takes_a_func :: f (Int -> Int) = print(f(1)+1)
that_other_func :: x Int -> Int = x + 4
main :: () = {
    let to_print :: Str = "Hello, World!" in
    just_print_my_string(to_print)
}

*/
