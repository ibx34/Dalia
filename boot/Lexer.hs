{-# LANGUAGE LambdaCase #-}

module Lexer where

import Common (Context (Context, at, at_block, blocks, c_multi_item, input, is_comment, results, sym_table), Keywords (..), LexerToken (..), Literals (..), Primes (Type), isCurrentMultiItemComment, isWorkingOnMultiItem)
import Control.Monad (void, when)
import Control.Monad qualified
import Control.Monad.State (MonadState (get, put), State, gets, join, modify)
import Data.Bool (bool)
import Data.Char (isAlphaNum, isNumber)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Distribution.Compat.CharParsing (CharParsing (string))
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

getKeyword :: [Char] -> LexerToken
getKeyword "typedef" = Keyword TypeDef
getKeyword a = Literal (Ident a)

endMultiCharCollection :: Lexer [LexerToken]
endMultiCharCollection = do
  ctx <- get
  let new_string = c_multi_item ctx
  case new_string of
    Just ns -> do
      put ctx {results = getKeyword ns : results ctx, c_multi_item = Nothing}
      lexAll
    Nothing -> lexAll

collectUntil :: Char -> (Char -> Bool) -> Lexer [LexerToken]
collectUntil c ch = do
  ctx <- get
  put ctx {c_multi_item = Just (maybe [c] (++ [c]) (c_multi_item ctx))}
  modify (\ctx -> ctx {at = at ctx + 1})
  current <- current
  case current of
    Just currentChar | ch currentChar -> lex currentChar
    _ -> endMultiCharCollection

pushBack :: LexerToken -> Lexer [LexerToken]
pushBack tok = do
  ctx <- get
  put ctx {at = at ctx + 1, results = tok : results ctx}
  lexAll

lex :: Char -> Lexer [LexerToken]
lex '!' = pushBack Bang
lex '+' = pushBack Plus
lex '|' = pushBack Pipe
lex '=' = pushBack Eq
lex ':' = do
  ctx <- get
  peek >>= \case
    Just ':' -> do
      modify (\ctx -> ctx {at = at ctx + 1})
      pushBack DColon
    _ -> pushBack Colon
lex '(' = pushBack OpenP
lex '{' = pushBack OpenCurlyP
lex '}' = pushBack CloseCurlyP
lex '[' = pushBack OpenSquareP
lex ']' = pushBack CloseSquareP
lex ',' = pushBack Comma
lex ')' = pushBack CloseP
lex '→' = pushBack FunctionArrow
lex '\\' = pushBack Backslash
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
lex '\'' = error "CHARACTER UNSUPPORTED!"
lex '"' = do
  -- This and char is temporary... will make better later when i Feel like it?
  ctx <- get
  modify (\ctx -> ctx {at = at ctx + 1})
  ctx <- get
  let (string, other) = span (/= '"') (drop (at ctx) (input ctx))
   in do
        ctx <- get
        -- we add `+1` obviously for the closing "!
        put ctx {at = at ctx + length string + 1, results = Literal (String string) : results ctx}
        lexAll
lex a
  | a == 't' = do
      ctx <- get
      peek >>= \case
        Just '\'' -> do
          modify (\ctx -> ctx {at = at ctx + 1})
          pushBack (Prime Type)
        _ -> collectUntil 't' (\a -> isAlphaNum a || a == '_')
  | isAlphaNum a || a == '_' = collectUntil a (\a -> isAlphaNum a || a == '_')
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
