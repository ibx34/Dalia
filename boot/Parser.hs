module Parser where

import Control.Monad.State
import Tokens (Context (Context, at, input, results), TokTy (Identifier))

data Expr = Ident deriving (Show) 

type PContext = Context [TokTy] Expr
type Parser a = State PContext a

initialParser :: [TokTy] -> PContext
initialParser input =
  Context
    { input = input,
      at = 0,
      results = []
    }

parse :: Parser [Expr]
parse = do
  ctx <- get
  let i = at ctx
  if i >= length (input ctx)
    then return (reverse $ results ctx)
    else do
      let currentTok = input ctx !! i
      let expr = Ident
      put ctx { at = i + 1, results = expr : results ctx }
      parse
