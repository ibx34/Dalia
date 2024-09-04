module Lexer where

import Common (Context (Context, at, at_block, blocks, input, results, sym_table), LexerToken (..))
import Control.Monad.State (MonadState (get, put), State, gets)
import Data.Map qualified as Map

type LexerContext = Common.Context String LexerToken

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

-- lexOne :: Maybe LexerToken -> Lexer (Maybe LexerToken)
-- lexOne maybe = return Nothing

lexAll :: Lexer [LexerToken]
lexAll = gets results