module Lexer where

import Common (Context (Context, at, at_block, blocks, input, results, sym_table), LexerToken (..))
import Control.Monad qualified
import Control.Monad.State (MonadState (get, put), State, gets, join)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)

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

peekAndCurrentInternal :: Int -> Lexer (Maybe Char)
peekAndCurrentInternal n = do
  ctx <- get
  if at ctx + n < length (input ctx)
    then return $ Just (input ctx !! (at ctx + n))
    else return Nothing

peek :: Lexer (Maybe Char)
peek = peekAndCurrentInternal 1

current :: Lexer (Maybe Char)
current = peekAndCurrentInternal 0

advance :: Lexer ()
advance = do
  ctx <- get
  let newPos = at ctx + 1
  put ctx {at = newPos}

lexOne :: Maybe LexerToken -> Lexer (Maybe LexerToken)
lexOne prev = do
  case prev of
    Just ForwardSlash -> do
      return Nothing
    -- No previous token? This must not be a multi character token
    -- so we will handle it as any normal token (!, (, ), ], you get
    -- point...)
    Nothing -> do
      curr <- current
      case curr of
        Just ':' -> return (Just Colon)
        Just '/' -> lexOne (Just ForwardSlash)
        Nothing -> error "Unhandled token"

lexAll :: Lexer [LexerToken]
lexAll = gets results