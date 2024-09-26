module LowerStep1 where

import Common (Context (Context, at, at_block, blocks, c_multi_item, input, is_comment, results, sym_table), Keywords (..), LexerToken (..), Literals (..), isCurrentMultiItemComment, isWorkingOnMultiItem)
import Control.Monad (void, when)
import Control.Monad qualified
import Control.Monad.State (MonadState (get, put), State, gets, join, modify)
import Data.Bool (bool)
import Data.Char (isAlphaNum, isNumber)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Prelude hiding (lex)

type LowerStep1Context = Common.Context [LexerToken] String LexerToken

type LowerStep1 a = State LowerStep1Context a

data Expr = DD

createStairCase :: [LexerToken] -> LowerStep1Context
createStairCase a =
  Context
    { input = a,
      at = 0,
      results = [],
      blocks = [],
      sym_table = Map.empty,
      c_multi_item = Nothing,
      is_comment = False,
      at_block = 0
    }

step :: LexerToken -> LowerStep1 [LexerToken]
step = error "WHAT"