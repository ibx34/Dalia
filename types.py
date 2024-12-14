from ast_1 import Assignment, Expr, Lambda
from enum import Enum, auto


class PrimitiveTypes(Enum):
    INT = auto()
    STR = auto()
    FLOAT = auto()
    # ()
    UNIT = auto()


class Type(Expr):
    def __init__(self, inner: PrimitiveTypes | Expr):
        super().__init__(inner)
        self.inner = inner
        self.is_lambda = isinstance(inner, Lambda)
        if isinstance(inner, Assignment):
            right = inner.right
            if isinstance(right, Lambda):
                self.inner = right
# type: ignore
