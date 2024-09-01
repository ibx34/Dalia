module Tokens where

    data TokTy = Identifier String | StrLit String | CharLit Char | Bang | Colon | DColon | OpenP | CloseP | OpenCurlP | CloseCurlP | Eq | GreaterThan | LessThan | Dash | Plus | Backslash | NewLine
        deriving (Show, Eq)