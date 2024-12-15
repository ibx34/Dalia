from ast_exprs import Assignment, AstirExpr, Identifier, Lambda, Symbol  # type: ignore
from common import Cursor


class ASM(Cursor):
    def __init__(self, input: list[AstirExpr]) -> None:
        super().__init__(input)
        self.lines: list[str] = [".global _start", ".p2align 3"]

    def generate(self) -> None:
        c_expr = self.current()
        if isinstance(c_expr, Assignment):
            if isinstance(c_expr.right, Lambda) and isinstance(c_expr.left, Identifier):
                lambda_def = c_expr.right.definition
                parameters_to_registers: dict[str, str] = {}
                last_reg = 0
                for k in lambda_def.parameters.symbols.keys():
                    parameter = lambda_def.parameters.symbols.get(k)
                    if parameter is None or not isinstance(parameter, Symbol):
                        break
                    elif parameter.name == "ret":
                        continue
                    parameters_to_registers[parameter.name] = f"X{last_reg}"
                    last_reg += 1
                # -1 to get rid of the return type. the length in the name may be temporary :)
                self.lines.append(
                    f"{c_expr.left.value}_{len(lambda_def.parameters.symbols.keys())-1}:"
                )
                pass
        return None 
