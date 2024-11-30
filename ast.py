from abc import ABC, abstractmethod
from os import read
import re
from enum import Enum, auto
from typing import Generic, Callable, TypeVar


class PrimitiveTypes(Enum):
    INT = 0
    STR = 1
    FLOAT = 2


class TT(Enum):
    COLON = ":"
    COMMA = ","
    BACKSLASH = "\\"
    FUNCTION_ARROW = "→"
    PLUS = "+"
    # minus!
    DASH = "-"
    DOUBLE_COLON = "::"
    OPEN_PAREN = "("
    CLOSE_PAREN = ")"
    OPEN_SQUARE = "["
    CLOSE_SQUARE = "]"
    IDENT = "IDENT"
    LITERAL = "LITERAL"


operators = {
    "+": {
        "precedence": 1,
        # 0 = Left, 1 = Right, 2 = None
        "associativity": 0,
    },
    "-": {
        "precedence": 1,
        # 0 = Left, 1 = Right, 2 = None
        "associativity": 0,
    },
}


class Token:
    def __init__(self, ty: TT | None, prim_ty: PrimitiveTypes = None, val=None) -> None:
        if ty == None:
            raise Exception("Token type was none...")
        self.ty = ty
        self.val = val
        self.prim_ty = prim_ty

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
            if c == " " or c == "\n":
                self.advance()
                continue
            elif c == None:
                break
            lexed = self.lex()
            self.results.append(lexed)
            self.advance()

    def collect_until(
        self,
        check: Callable[[str | None, str], bool],
        devance_b4_break: bool = False,
        start_str: str = "",
    ) -> str:
        temp_str = start_str
        while True:
            c = self.current()
            if check(c, temp_str):
                if devance_b4_break:
                    self.at -= 1
                break
            self.advance()
            temp_str += c
        return temp_str

    def lex(self) -> Token:
        c = self.current()
        if c is None:
            raise Exception("Ran out of input")
        elif c == ":" and self.input[self.at + 1] == ":":
            self.advance()
            return Token(TT.DOUBLE_COLON)
        elif c == '"':
            self.advance()
            string = self.collect_until(lambda c, _: (c is None) or c == '"')
            return Token(TT.LITERAL, prim_ty=PrimitiveTypes.STR, val=string)
        elif c not in TT and (is_valid_ident(c) or c == "."):
            self.advance()

            def identifier_check(c: str | None, rest: str) -> bool:
                if (c is None) or (not is_valid_ident(c)) and c != ".":
                    return True
                return False

            ident = self.collect_until(identifier_check, True, start_str=c)

            if "." in ident:
                try:
                    number = float(ident)
                    sign = 0 if number >= 0 else 1

                    number = abs(number)
                    integer = int(number)
                    fractional = number - integer
                    integer_bin = (
                        bin(integer).replace("0b", "") if integer != 0 else "0"
                    )

                    frac_bin = []  # List to store the fractional binary digits
                    while (
                        fractional and len(frac_bin) < 23 + 3
                    ):  # Stop after 23+3 bits to avoid overflow
                        fractional *= 2  # Multiply by 2 to shift digits left
                        bit = int(fractional)  # Extract the integer part (0 or 1)
                        frac_bin.append(str(bit))  # Append the bit to the list
                        fractional -= (
                            bit  # Remove the integer part from the fractional value
                        )
                    frac_bin = "".join(frac_bin)
                    combined_bin = integer_bin + "." + frac_bin

                    if (
                        "1" in combined_bin
                    ):  # Ensure there is at least one significant bit
                        first_one = combined_bin.index(
                            "1"
                        )  # Find the position of the first '1'
                        if "." in combined_bin and first_one > combined_bin.index("."):
                            first_one -= (
                                1  # Adjust for the position of the binary point
                            )
                        exponent = (
                            len(integer_bin) - 1 - first_one
                        )  # Calculate the exponent from normalization
                        mantissa = (integer_bin + frac_bin)[
                            first_one + 1 : first_one + 24
                        ]  # Extract mantissa bits
                    else:  # Special case for zero-like numbers
                        exponent = 0
                        mantissa = "0" * 23  # Mantissa is all zeros

                    # Step 4: Encode the exponent (add bias of 127)
                    exponent += 127  # Apply the bias to the exponent
                    exponent_bin = (
                        bin(exponent).replace("0b", "").zfill(8)
                    )  # Convert to 8-bit binary

                    # Step 5: Pad the mantissa to 23 bits
                    mantissa = mantissa.ljust(
                        23, "0"
                    )  # Ensure the mantissa has exactly 23 bits

                    # Combine the components into a 32-bit IEEE 754 representation
                    ieee754 = f"{sign}{exponent_bin}{mantissa}"
                    return Token(TT.LITERAL, val=ieee754, prim_ty=PrimitiveTypes.FLOAT)
                except ValueError:
                    raise Exception(
                        f'Something went wrong handling decimal: "{ident}"? check how many dots...'
                    )

            return Token(TT.IDENT, val=ident)
        else:
            return Token(TT(c))


class Expr(ABC):
    # This is a TEMPORARY SOLUTION!
    def __init__(self, num_of_advances: int) -> None:
        super().__init__()
        self.num_of_advances = num_of_advances

    @abstractmethod
    def __repr__(self) -> str:
        pass


def get_op(possible_op: Token | None) -> tuple[str, dict[str, int]] | None:
    if (
        possible_op is None
        or possible_op.ty is None
        or possible_op.ty not in [TT.PLUS, TT.DASH]
    ):
        return None
    op = operators[possible_op.ty.value]
    return (possible_op.ty.value, op)


def check_is_allowed(expr: Expr | None) -> bool:
    allowed = expr is not None or (
        isinstance(expr, Parenthesized)
        or isinstance(expr, Reference)
        or isinstance(expr, Literal)
    )
    if expr is not None and isinstance(expr, Identifier) and expr.for_assignment:
        allowed = False
    return allowed


class Symbol:
    def __init__(self, name: str, val: Expr, belongs_to: int, id: int) -> None:
        super().__init__()
        self.name = name
        self.val = val
        self.belongs_to = belongs_to
        self.id = id

    def __repr__(self) -> str:
        return f'Symbol "{self.name}", value = {self.val}. Belongs to = {self.belongs_to}. ID = {self.id}'


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
        self.last_id += 1
        symbol = Symbol(name, val, self.id, self.last_id)
        self.symbols[self.last_id] = symbol
        self.name_to_id[name] = self.last_id

    def __repr__(self) -> str:
        return f"{self.symbols}"


# Tuple as well as (<expr>)?
class Parenthesized(Expr):
    def __init__(self, num_of_advances: int, inner: Expr = None) -> None:
        super().__init__(num_of_advances)
        self.inner = inner

    def __repr__(self) -> str:
        return f"Parenthesized({self.inner})"


class ShuntingYardAlgorithmResults(Expr):
    def __init__(
        self, num_of_advances: int, operators: list[str], results: list[Expr]
    ) -> None:
        super().__init__(num_of_advances)
        self.oeprators = operators
        self.results = results

    def __repr__(self) -> str:
        return f"ShuntingYardAlgorithmResults({self.results}, ops={self.oeprators})"


class Identifier(Expr):
    def __init__(
        self, num_of_advances: int, value: str, for_assignment: bool = False
    ) -> None:
        super().__init__(num_of_advances)
        self.value = value
        self.for_assignment = for_assignment

    def __repr__(self) -> str:
        return f"Ident({self.value})"


class Tuple(Expr):
    def __init__(self, num_of_advances: int, values: list[Expr]) -> None:
        super().__init__(num_of_advances)
        self.values = values

    def __repr__(self) -> str:
        return f"Tuple({self.values})"


class TypeDef(Expr):
    # TODO: should name be Identifier?
    def __init__(self, num_of_advances: int, name: str) -> None:
        super().__init__(num_of_advances)
        self.name = name

    def __repr__(self) -> str:
        return f"TypeDef(N={self.name})"


class Lambda(Expr):
    def __init__(
        self, num_of_advances: int, parameters: SymbolTable, body: Expr
    ) -> None:
        super().__init__(num_of_advances)
        self.parameters = parameters
        self.body = body

    def __repr__(self) -> str:
        return f"Lambda(P={self.parameters},B={self.body})"


class Assignment(Expr):
    def __init__(self, num_of_advances: int, left: Expr, right: Expr) -> None:
        super().__init__(num_of_advances)
        self.left = left
        self.right = right

    def __repr__(self) -> str:
        return f"Assignment ({self.left}) -> ({self.right})"


class Reference(Expr):
    def __init__(
        self, num_of_advances: int, name: str, belongs_to: int, symbol_id: int
    ) -> None:
        super().__init__(num_of_advances)
        self.name = name
        self.symbol_id = symbol_id
        self.belongs_to = belongs_to

    def __repr__(self) -> str:
        return f"Ref(ST={self.belongs_to}, Ref={self.name}, ID={self.symbol_id})"


class Parameter(Expr):
    def __repr__(self) -> str:
        return f"Parameter"


class Literal(Expr):
    def __init__(self, num_of_advances: int, literal_ty: Expr, val: any) -> None:
        super().__init__(num_of_advances)
        self.literal_ty = literal_ty
        self.val = val

    def __repr__(self) -> str:
        return f"Literal(LTY={self.literal_ty}, V={self.val})"


class PrimitiveType(Expr):
    def __init__(self, num_of_advances: int, inner: PrimitiveTypes) -> None:
        super().__init__(num_of_advances)
        self.inner = inner

    def __repr__(self) -> str:
        return f"PrimitiveType(I={self.inner})"


class Parser(Cursor):
    def __init__(self, input: list[Token]) -> None:
        super().__init__(input)
        self.results: list[Expr] = []
        global_symbols = SymbolTable(0)
        # TODO: we are waiting for typedef!
        global_symbols.insert("int", PrimitiveType(0, PrimitiveTypes.INT))
        global_symbols.insert("str", PrimitiveType(0, PrimitiveTypes.STR))
        global_symbols.insert("float", PrimitiveType(0, PrimitiveTypes.FLOAT))

        self.symbol_tables: dict[int, SymbolTable] = {0: global_symbols}
        self.using_st: int = 0
        self.parsing_lambda_parameters = False
        self.op_stack: list[str] = []
        # temp solution
        self.current_number_of_advances = 0
        self.already_parsing_sya = False

    def advance(self) -> None:
        self.current_number_of_advances += 1
        return super().advance()

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
            self.current_number_of_advances = 0

    def parse(self) -> Expr | None:
        c = self.current()
        result: Expr | None = None
        if c is None:
            result = None
        elif c.ty == TT.LITERAL:
            if c.prim_ty is None or c.val is None:
                raise Exception("Invalid primitive type...how?")
            self.advance()
            result = Literal(0, PrimitiveType(0, c.prim_ty), c.val)
        elif c.ty == TT.IDENT:
            if c.val is None:
                raise Exception("Identifier with no value?")
            symbol = self.lookup(c.val)
            if symbol is not None:
                self.advance()
                result = Reference(
                    self.current_number_of_advances, c.val, symbol.belongs_to, symbol.id
                )
            else:
                next = self.input[self.at + 1]
                if (
                    next is not None
                    and next.ty is TT.IDENT
                    and self.parsing_lambda_parameters
                    and self.using_st in self.symbol_tables
                ):
                    self.advance()
                    sym_table = self.symbol_tables[self.using_st]
                    expr = self.parse()
                    if expr is not None and isinstance(expr, Reference):
                        sym_table.insert(c.val, expr)
                        result = Parameter(self.current_number_of_advances)
                else:
                    self.advance()
                    for_assignment = False
                    if (
                        (c2 := self.current())
                        and c2 is not None
                        and c2.ty == TT.BACKSLASH
                    ):
                        for_assignment = True
                    result = Identifier(
                        self.current_number_of_advances, c.val, for_assignment
                    )

        elif c.ty == TT.OPEN_PAREN:
            self.advance()
            the_between: list[Expr] = []
            has_comma: bool = False
            while True:
                c = self.current()
                if c is not None:
                    if c.ty == TT.CLOSE_PAREN:
                        self.advance()
                        break
                    elif c.ty == TT.COMMA:
                        self.advance()
                        has_comma = True
                        continue

                expr = self.parse()
                if expr is None:
                    self.at -= 1
                    break
                the_between.append(expr)
            if len(the_between) == 0:
                # We init Parenthesized with no expression so
                # that it is treated as an empty tuple, non value
                # or dead value. Its just a placeholder ig?
                result = Parenthesized(self.current_number_of_advances)
            elif len(the_between) == 1:
                # Init Parenthesized with an expression (the_between[0])
                # to do exactly what it says... for example (\ :: int ...)
                result = Parenthesized(self.current_number_of_advances, the_between[0])
            elif len(the_between) > 1 and has_comma:
                # Handle tuples
                result = Tuple(self.current_number_of_advances, the_between)

            # TODO: handle all function call arg parsing
            # function calls can use () but are not required
            # all this should be handled down where the
            # infix operators are but check if the previous
            # expression was a reference, or fn def and
            # then match arguments with arguments.
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
                        if ret_type is None:
                            raise Exception(
                                f"Return type was not there or non identifier ({ret_type})"
                            )
                        lambda_symbol_table.insert("ret", ret_type)
                        self.symbol_tables[symbol_table_id] = lambda_symbol_table
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
            body = self.parse()
            if body is None:
                raise Exception(f"Lambda must have body {self.current()}")
            popped = self.results.pop()
            if not isinstance(popped, Identifier):
                return popped

            self.using_st = 0
            result = Assignment(
                self.current_number_of_advances,
                popped,
                Lambda(self.current_number_of_advances, lambda_symbol_table, body),
            )

        # At this point, past previous parsing, we should have advanced past
        # the last token and now be face-to-face with the rare, elusive, OP!
        c = self.current()
        possible_op = get_op(c)
        if not check_is_allowed(result) or result is None or self.already_parsing_sya:
            return result
        if possible_op is None:
            return result

        self.advance()
        # NOW WE START! begin working with the shunting yard algorithm
        # for parsing arithmetic
        self.op_stack.append(possible_op[0])
        self.already_parsing_sya = True
        res: list[Expr] = [result]
        while True:
            c = self.current()
            possible_op = get_op(c)
            if c is None:
                break
            elif possible_op is not None:
                self.op_stack.append(possible_op[0])
                self.advance()
                continue

            parsed = self.parse()
            if not check_is_allowed(parsed) or parsed is None:
                self.at -= 1
                break
            res.append(parsed)

        sya_res = ShuntingYardAlgorithmResults(
            self.current_number_of_advances, self.op_stack, res
        )
        self.op_stack = []
        self.already_parsing_sya = False
        return sya_res  # type: ignore