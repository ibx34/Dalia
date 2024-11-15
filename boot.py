from abc import ABC, abstractmethod
from os import read
import sys
from enum import Enum, auto
from typing import Generic, Optional, TypeVar


class TT(Enum):
    COLON = ":"
    COMMA = ","
    BACKSLASH = "\\"
    FUNCTION_ARROW = "→"
    PLUS = "+"
    DOUBLE_COLON = "::"
    IDENT = "IDENT"
    STRING = "STRING"


class Token:
    def __init__(self, ty: TT | None, val=None) -> None:
        if ty == None:
            raise Exception("Token type was none...")
        self.ty = ty
        self.val = val

    def __repr__(self) -> str:
        return f"{self.ty} ({self.val})"


def is_valid_ident(c: str) -> bool:
    return c.isalnum() or c == "_"


T = TypeVar("T")


class Cursor(ABC, Generic[T]):
    def __init__(self, input: list[T]) -> None:
        super().__init__()
        self.input = input
        self.at = 0

    def advance(self) -> None:
        self.at += 1

    def current(self) -> T | None:
        if self.at >= len(self.input):
            return None
        return self.input[self.at]


class Lexer(Cursor):
    def __init__(self, input: str) -> None:
        super().__init__(list(input))
        self.results: list[Token] = []

    def lex_all(self) -> None:
        while c := self.current():
            if c == " ":
                self.advance()
                continue
            elif c == None:
                break
            lexed = self.lex()
            self.results.append(lexed)
            self.advance()

    def lex(self) -> Token:
        c = self.current()
        if c is None:
            raise Exception("Ran out of input")
        elif c == ":" and self.input[self.at + 1] == ":":
            self.advance()
            return Token(TT.DOUBLE_COLON)
        elif c not in TT and is_valid_ident(c):
            self.advance()
            temp_str = c
            while True:
                c = self.current()
                if (c is None) or (not is_valid_ident(c)):
                    self.at -= 1
                    break
                self.advance()
                temp_str += c
            return Token(TT.IDENT, val=temp_str)
        else:
            return Token(TT(c))


class Expr(ABC):
    @abstractmethod
    def __repr__(self) -> str:
        pass


class Symbol:
    def __init__(self, name: str, val: Expr, belongs_to: int) -> None:
        super().__init__()
        self.name = name
        self.val = val
        self.belongs_to = belongs_to

    def __repr__(self) -> str:
        return f'Symbol "{self.name}", value = {self.val}. Belongs to s.t. {self.belongs_to}'


class SymbolTable:
    def __init__(self, id: int, parent: int | None = None) -> None:
        self.symbols: dict[int, Symbol] = {}
        self.name_to_id: dict[str, int] = {}
        self.last_id = 0
        self.id = id
        self.parent = parent

    def lookup(self, name: str) -> Symbol | None:
        if name not in self.name_to_id:
            return None
        id = self.name_to_id[name]
        return self.lookup_by_id(id)

    def lookup_by_id(self, id: int) -> Symbol | None:
        if id not in self.symbols:
            return None
        return self.symbols[id]

    def insert(self, name: str, val: Expr) -> None:
        symbol = Symbol(name, val, self.id)
        self.last_id += 1
        self.symbols[self.last_id] = symbol
        self.name_to_id[name] = self.last_id

    def __repr__(self) -> str:
        return f"{self.symbols}"


class Identifier(Expr):
    def __init__(self, value: str) -> None:
        super().__init__()
        self.value = value

    def __repr__(self) -> str:
        return f"Ident({self.value})"


class TypeDef(Expr):
    # TODO: should name be Identifier?
    def __init__(self, name: str) -> None:
        super().__init__()
        self.name = name

    def __repr__(self) -> str:
        return f"TypeDef(N={self.name})"


class Lambda(Expr):
    def __init__(self, parameters: SymbolTable, body: Expr) -> None:
        super().__init__()
        self.parameters = parameters
        self.body = body

    def __repr__(self) -> str:
        return f"Lambda(P={self.parameters},B={self.body})"


class Assignment(Expr):
    def __init__(self, left: Expr, right: Expr) -> None:
        super().__init__()
        self.left = left
        self.right = right

    def __repr__(self) -> str:
        return f"Assignment ({self.left}) -> ({self.right})"


class Reference(Expr):
    def __init__(self, name: str, belongs_to: int) -> None:
        super().__init__()
        self.name = name
        self.belongs_to = belongs_to

    def __repr__(self) -> str:
        return f"Ref(ST={self.belongs_to}, Ref={self.name})"


class Parameter(Expr):
    def __repr__(self) -> str:
        return f"Parameter"


class Parser(Cursor):
    def __init__(self, input: list[Token]) -> None:
        super().__init__(input)
        self.results: list[Expr] = []
        zero_symbol_table = SymbolTable(0)
        # TODO: we are waiting for typedef!
        zero_symbol_table.insert("int", Identifier("int"))

        self.symbol_tables: dict[int, SymbolTable] = {0: zero_symbol_table}
        self.using_st: int = 0
        self.parsing_lambda_parameters = False
        self.op_stack: list[Expr] = []

    def lookup(self, name: str, symbol_table_id: int | None = None) -> Symbol | None:
        symbol_table_id = self.using_st if symbol_table_id is None else symbol_table_id
        if symbol_table_id is None or symbol_table_id not in self.symbol_tables:
            return None
        symbol_table = self.symbol_tables[symbol_table_id]
        symbol = symbol_table.lookup(name)
        if symbol is None and (symbol_table.parent is not None):
            return self.lookup(name, symbol_table.parent)
        return symbol

    def parse_all(self) -> None:
        while c := self.current():
            if c == None:
                break
            parsed = self.parse()
            if parsed is None:
                break
            self.results.append(parsed)
            self.advance()

    def parse(self) -> Expr | None:
        c = self.current()
        if c is None:
            return None
        elif c.ty == TT.IDENT:
            if c.val is None:
                raise Exception("Identifier with no value?")
            symbol = self.lookup(c.val)
            if symbol is None:
                self.advance()
                next = self.current()
                if (
                    next is not None
                    and next.ty is TT.IDENT
                    and self.parsing_lambda_parameters
                    and self.using_st in self.symbol_tables
                ):
                    sym_table = self.symbol_tables[self.using_st]
                    expr = self.parse()
                    if expr is not None and isinstance(expr, Reference):
                        sym_table.insert(c.val, expr)
                        return Parameter()
                self.at -= 1
                return Identifier(c.val)
            return Reference(c.val, symbol.belongs_to)
        elif c.ty == TT.BACKSLASH:
            self.advance()
            symbol_table_id = list(self.symbol_tables.items())[-1][0] + 1
            lambda_symbol_table = SymbolTable(symbol_table_id, 0)
            self.symbol_tables[symbol_table_id] = lambda_symbol_table
            self.using_st = symbol_table_id
            self.parsing_lambda_parameters = True
            while True:
                c = self.current()
                if c is not None:
                    if c.ty == TT.COMMA:
                        self.advance()
                        continue
                    elif c.ty == TT.DOUBLE_COLON:
                        self.advance()
                        ret_type = self.parse()
                        if ret_type is None or not isinstance(ret_type, Reference):
                            raise Exception(
                                f"Return type was not there or non identifier ({ret_type})"
                            )
                        lambda_symbol_table.insert("ret", ret_type)
                        self.symbol_tables[symbol_table_id] = lambda_symbol_table
                        self.advance()
                        c = self.current()
                        if c is None or c.ty is not TT.FUNCTION_ARROW:
                            raise Exception(
                                f"Expected f.n. arrow after ret type... ({c})"
                            )
                        self.advance()
                        break
                expr = self.parse()
                if expr is None or not isinstance(expr, Parameter):
                    self.at -= 1
                    break
                self.advance()

            body = self.parse()
            if body is None:
                raise Exception(f"Lambda must have body {self.current()}")
            popped = self.results.pop()
            if not isinstance(popped, Identifier):
                return popped
            return Assignment(popped, Lambda(lambda_symbol_table, body))
        raise Exception(f"Unhandled token: {c}")


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
