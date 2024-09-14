module Lexer where

import Common (Context (Context, at, at_block, blocks, input, results, sym_table), LexerToken (..), Literals (..))
import Control.Monad (when)
import Control.Monad qualified
import Control.Monad.State (MonadState (get, put), State, gets, join, modify)
import Data.Char (isAlphaNum)
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

expectPeek :: Char -> Lexer Bool
expectPeek expected = do
  peeked <- peek
  case peeked of
    Just a -> return (expected == a)
    Nothing -> error "Peeking returned nothing (expectPeek)"

current :: Lexer (Maybe Char)
current = peekAndCurrentInternal 0

advance :: Int -> Lexer ()
advance by = do
  ctx <- get
  put ctx {at = at ctx + by}

collectUntil :: String -> (Char -> Bool) -> (String, Int)
collectUntil cs check =
  let (ident, rest) = span check cs
   in (ident, length ident)

lexOne :: Maybe LexerToken -> Lexer (Maybe LexerToken)
lexOne prev = do
  ctx <- get
  case prev of
    Just ForwardSlash -> do
      peeked <- peek
      -- error ("Would have been d slash! " ++ show (peeked == Just '/'))
      if peeked == Just '/'
        then do
          advance 2
          ctx <- get
          let (comment, len) = collectUntil (drop (at ctx) (input ctx)) (/= '\n')
          advance len
          return (Just Comment)
        else do
          return Nothing
    -- No previous token? This must not be a multi character token
    -- so we will handle it as any normal token (!, (, ), ], you get
    -- point...)
    Nothing -> do
      curr <- current
      ctx <- get
      case curr of
        Just ':' -> return (Just Colon)
        Just '/' -> lexOne (Just ForwardSlash)
        _ -> return Nothing

lexAll :: Lexer [LexerToken]
lexAll = do
  ctx <- get
  if at ctx >= length (input ctx)
    then return (reverse $ results ctx)
    else do
      ret <- lexOne Nothing
      case ret of
        Just ret -> do
          put ctx {results = ret : results ctx, at = at ctx + 1}
          lexAll
        Nothing -> do
          advance 1
          lexAll
