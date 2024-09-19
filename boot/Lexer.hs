module Lexer where

import Common (Context (Context, at, at_block, blocks, c_multi_item, input, results, sym_table), Keywords (..), LexerToken (..), Literals (..))
import Control.Monad (void, when)
import Control.Monad qualified
import Control.Monad.State (MonadState (get, put), State, gets, join, modify)
import Data.Char (isAlphaNum)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Prelude hiding (lex)

type LexerContext = Common.Context String LexerToken Char

type Lexer a = State LexerContext a

matchKeyword :: String -> Maybe Keywords
matchKeyword "typedef" = Just TypeDef

createLexer :: String -> LexerContext
createLexer a =
  Context
    { input = a,
      at = 0,
      results = [],
      blocks = [],
      sym_table = Map.empty,
      c_multi_item = Nothing,
      at_block = 0
    }

peekAndCurrentInternal :: Int -> Lexer (Maybe Char)
peekAndCurrentInternal n = do
  ctx <- get
  if at ctx + n < length (input ctx)
    then return $ Just (input ctx !! (at ctx + n))
    else return Nothing

advance :: Lexer [LexerToken]
advance = modify (\ctx -> ctx {at = at ctx + 1}) >> lexAll

peek :: Lexer (Maybe Char)
peek = peekAndCurrentInternal 1

current :: Lexer (Maybe Char)
current = peekAndCurrentInternal 0

lex :: Char -> Lexer [LexerToken]
lex _ = error "test"

lexAll :: Lexer [LexerToken]
lexAll = do
  ctx <- get
  current <- current
  case current of
    -- The fact we no longer push the results
    -- of what we just lexed means that the
    -- responsiblity of pushing new tokens
    -- back is up to the lex' fn, not lexAll
    -- this also goes for advancing.
    Just a -> lex a >> lexAll
    Nothing -> return (reverse $ results ctx)