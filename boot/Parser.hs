module Parser where

import Control.Monad.State
import Tokens (TokTy)

data Expr deriving (Show)

data Context a = Context
  { input :: a,
    at :: Int,
    results :: [Expr]
  }
  deriving (Show)

type Parser a = State (Context TokTy) a

initialLexer :: String -> Context String
initialLexer input =
  Context
    { input = input,
      at = 0,
      results = []
    }

parse :: Parser [TokTy]
parse = parse