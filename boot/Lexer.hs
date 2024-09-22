{-# LANGUAGE LambdaCase #-}

module Lexer where

import Common (Context (Context, at, at_block, blocks, c_multi_item, input, is_comment, results, sym_table), Keywords (..), LexerToken (..), Literals (..), isCurrentMultiItemComment, isWorkingOnMultiItem)
import Control.Monad (void, when)
import Control.Monad qualified
import Control.Monad.State (MonadState (get, put), State, gets, join, modify)
import Data.Bool (bool)
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
      is_comment = False,
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
current = do
  ctx <- get
  if at ctx < length (input ctx)
    then return $ Just (input ctx !! at ctx)
    else return Nothing

endMultiCharCollection :: Lexer [LexerToken]
endMultiCharCollection = do
  ctx <- get
  let new_string = c_multi_item ctx
  case new_string of
    Just ns -> do
      put ctx {results = Literal (String ns) : results ctx, c_multi_item = Nothing}
      lexAll
    Nothing -> lexAll

collectUntil :: Bool -> Char -> (Char -> Bool) -> Lexer [LexerToken]
collectUntil is_comment c ch = do
  ctx <- get
  put ctx {c_multi_item = Just (maybe [c] (++ [c]) (c_multi_item ctx))}
  modify (\ctx -> ctx {at = at ctx + 1})
  peeked <- peek
  case peeked of
    Just peekedChar | ch peekedChar -> lex peekedChar
    _ -> endMultiCharCollection

lex :: Char -> Lexer [LexerToken]
lex '!' = do
  ctx <- get
  put ctx {at = at ctx + 1, results = Bang : results ctx}
  lexAll
lex '\n' = do
  ctx <- get
  bool advance endMultiCharCollection (isCurrentMultiItemComment ctx)
lex '/' = do
  ctx <- get
  peek >>= \case
    Just '/' ->
      let (comment, other) = span (/= '\n') (drop (at ctx) (input ctx))
       in do
            modify (\ctx -> ctx {at = at ctx + length comment})
            lexAll
    a -> error ("Unexpected character trailing /: " ++ show a)
lex a
  | isAlphaNum a || a == '_' = collectUntil False a (\a -> isAlphaNum a || a == '_')
lex _ = do
  ctx <- get
  if isWorkingOnMultiItem ctx
    then endMultiCharCollection
    else advance

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
