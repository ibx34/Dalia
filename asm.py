# from ast import Assignment, Cursor, Expr, Identifier, Lambda, Token # type: ignore


# class ASM(Cursor):
#     def __init__(self, input: list[Expr]) -> None:
#         super().__init__(input)
#         self.lines: list[str] = [".p2align 3"]

#     def generate(self) -> None:
#         c_expr = self.current()
#         if isinstance(c_expr, Assignment) and isinstance(c_expr.left, Identifier):
#             name = c_expr.left.value
#             if isinstance(c_expr.right, Lambda):
#                 print(f"Handling lambda assignment {c_expr.left}.")
#                 self.lines.append(f"{name}:")
#                 return None
#         print(f"!! {c_expr}")
#         return None