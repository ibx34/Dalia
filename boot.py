from ast import Parser, Lexer # type: ignore


def run():
    file = open("boot.dal").read()
    lexer = Lexer(file)
    lexer.lex_all()
    print(f"{lexer.results}\n\n")
    parser = Parser(lexer.results)
    parser.parse_all()
    print(f"{parser.results}")


if __name__ == "__main__":
    run()
