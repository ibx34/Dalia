from ast_exprs import Application, Assignment, AstirExpr, Identifier, Lambda, Literal, Reference, ShuntingYardAlgorithmResults, Symbol, SymbolTable  # type: ignore
from common import Cursor, PrimitiveTypes


class Register:
    def __init__(self, reg: int, being_used_by: int):
        self.being_used_by = being_used_by
        self.reg = reg


class ASM(Cursor):
    def __init__(
        self, input: list[AstirExpr], symbol_tables: dict[int, SymbolTable]
    ) -> None:
        super().__init__(input)
        self.lines: list[str] = [".global _start", ".p2align 3"]
        self.symbol_tables: dict[int, SymbolTable] = symbol_tables
        # Format (ref_id, register)
        self.ref_id_and_register: list[tuple[int, int]] = []
        self.fn_register_man: dict[int, list[tuple[int, int]]] = {}
        self.current_usable_register = 0

        # This should be cleared after every generate call.
        self.registers_in_use: list[Register] = []

    def generate_all(self):
        while self.current() is not None:
            generated = self.generate()
            self.lines.extend(generated)
            self.advance()
            self.registers_in_use = []

    def lookup_symbol(self, symbol_table: int, symbol_id: int) -> Symbol | None:
        if symbol_table not in self.symbol_tables:
            return None
        _symbol_table: SymbolTable | None = self.symbol_tables[symbol_table]
        if _symbol_table is None:
            return None
        symbol = _symbol_table.lookup_by_id(symbol_id)
        return symbol

    def generate(self, expr: AstirExpr | None = None) -> list[str]:
        c_expr = self.current() if expr is None else expr
        to_add: list[str] = []
        if isinstance(c_expr, Assignment):
            if isinstance(c_expr.right, Lambda) and isinstance(c_expr.left, Identifier):
                symbols = c_expr.right.definition.parameters.symbols

                # Reserve registers for the arguments!!!
                def reserve_register(x: int) -> tuple[int, int]:
                    ret = (x, self.current_usable_register)
                    self.current_usable_register += 1
                    return ret

                reserved_registers: list[tuple[int, int]] = list(
                    map(reserve_register, symbols)
                )[:-1]
                self.fn_register_man[c_expr.right.belongs_to] = reserved_registers
                to_add.append(f"{c_expr.left.value}:")
                print(f"Being added: {c_expr.right.definition}")
                generated_body = self.generate(c_expr.right.body)
                print(f"Generated body -> {generated_body}")
                to_add.extend(generated_body)
                to_add.append("ret")
        elif isinstance(c_expr, Application):
            reserved_registers = self.fn_register_man[c_expr.lambda_ref.symbol_id]
            for idx, v in enumerate(reserved_registers):
                if not (0 <= idx < len(c_expr.parameters)):
                    raise Exception(f"Invalid application")
                param: AstirExpr = c_expr.parameters[idx]
                if not isinstance(param, Literal) or param.ty != PrimitiveTypes.INT:
                    raise Exception("TODO: HANDLE MORE THAN JUST LITERALS")
                to_add.append(f"mov x{v[1]}, {param.val}")

            symbol = self.lookup_symbol(
                c_expr.lambda_ref.belongs_to, c_expr.lambda_ref.symbol_id
            )
            if symbol is None:
                raise Exception(f"failed to find symbol {c_expr.lambda_ref.symbol_id}")
            to_add.append(f"bl {symbol.name}")
        elif isinstance(c_expr, Reference):
            symbol = self.lookup_symbol(c_expr.belongs_to, c_expr.symbol_id)
            if symbol is None:
                raise Exception("failed to lookup symbol")
            if isinstance(symbol.val, Reference):
                symbol2 = self.lookup_symbol(
                    symbol.val.belongs_to, symbol.val.symbol_id
                )
                register = self.current_usable_register
                self.ref_id_and_register.append((symbol.val.symbol_id, register))
                to_add.append(f"x{register}")  # Temp
        elif isinstance(c_expr, ShuntingYardAlgorithmResults):
            if len(c_expr.oeprators) > 0:
                raise Exception("Invalid shunting yard algorithm")
            stack: list[str] = []
            c_expr.results.reverse()
            while len(c_expr.results) > 0 and (term := c_expr.results.pop()):
                # TODO: make some like class method or something
                # to make this cleaner??
                if isinstance(term, Reference):
                    stack.extend(self.generate(term))
                elif isinstance(term, Literal):
                    if term.ty != PrimitiveTypes.INT:
                        raise Exception("Unexpected type.")
                    stack.append(str(term.val))
                elif isinstance(term, str):
                    if term == "+":
                        stack.reverse()
                        (item1, item2) = (stack.pop(), stack.pop())
                        if not item1.startswith("x"):
                            register = self.current_usable_register
                            to_add.append(f"mov x{register}, {item1}")
                            item1 = f"x{register}"
                            self.current_usable_register += 1
                        print(
                            f"Adding last two items on stack: {item1}, {item2} = {item1 + item2}"
                        )
                        to_add.append(f"add x0, {item1}, {item2}")

            print(f"{c_expr}")
        return to_add
