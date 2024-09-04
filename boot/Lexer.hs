module Lexer where

import Common (Context (Context, at, at_block, blocks, input, results, sym_table), LexerTokens)
import Control.Monad.State (State)
import Data.Map qualified as Map

type LexerContext = Common.Context String LexerTokens

type Lexer a = State LexerContext a

createLexer :: String -> LexerContext
createLexer a =
  Context
    { input = a,
      at = 0,
      results = [],
      blocks = [],
      sym_table = Map.empty,
      at_block = 0
    }

-- type PContext = Context [TokTy] Expr

-- type Parser a = State PContext a

-- initialParser :: [TokTy] -> PContext
-- initialParser input =
--   Context
--     { input = input,
--       at = 0,
--       results = [],
--       symbols = Map.empty,
--       at_block = 0
--     }
