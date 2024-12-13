from ast import Parser, Lexer # type: ignore

from asm import ASM # type: ignore


def run():
    file = open("boot.dal").read()
    lexer = Lexer(file)
    lexer.lex_all()
    print(f"{lexer.results}\n\n")
    parser = Parser(lexer.results)
    parser.parse_all()
    print(f"{parser.results}\n\n")
    code_generator = ASM(parser.results)
    code_generator.generate()
    print(f"{code_generator.lines}")


if __name__ == "__main__":
    run()
