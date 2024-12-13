from ast import Cursor, Expr, Token # type: ignore


class ASM(Cursor):
    def __init__(self, input: list[Expr]) -> None:
        super().__init__(input)
        self.lines: list[str] = [".p2align 3"]

    def generate(self) -> None:
        c_expr = self.current()
        print(f"!! {c_expr}")
        return None