{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Parser where

import Common (Context (Context, at, c_multi_item, input, is_comment, results, sym_tables, using), Expr (Place), Keywords (..), LexerToken (..), Literals (..), Primes (Type), SymbolInfo (SymbolInfo, name, val, _type), SymbolTable (SymbolTable, last_id, name_to_id, owner, table), SymbolType (PrimitiveType), isCurrentMultiItemComment, isWorkingOnMultiItem)
import Control.Monad (void, when)
import Control.Monad qualified
import Control.Monad.State (MonadState (get, put), State, gets, join, modify)
import Data.Bool (bool)
import Data.Char (isAlphaNum, isNumber)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Distribution.Compat.CharParsing (CharParsing (string))
import Prelude hiding (lex)

type ParserContext = Common.Context [LexerToken] Expr Expr

type Parser a = State ParserContext a

-- TODO: Make helper fn to make creating maps + insert nicer PLSSS
createParser :: [LexerToken] -> ParserContext
createParser a =
  let global_symbol_table = Map.empty
      global_symbol_table' =
        Map.insert
          0
          SymbolInfo
            { _type = PrimitiveType,
              val = Nothing,
              name = "int"
            }
          global_symbol_table
      name_to_id = Map.empty
      name_to_id' = Map.insert "int" 0 name_to_id
      sym_tables = Map.empty
      sym_tables' =
        Map.insert
          0
          SymbolTable
            { table = global_symbol_table',
              last_id = 0,
              name_to_id = name_to_id,
              owner = Nothing
            }
          sym_tables
   in Context
        { input = a,
          at = 0,
          results = [],
          using = Nothing,
          sym_tables = sym_tables',
          c_multi_item = Nothing,
          is_comment = False
        }

advance :: Parser ()
advance = do
  ctx <- get
  put ctx {at = at ctx + 1}

peek :: Parser (Maybe LexerToken)
peek = do
  ctx <- get
  if at ctx + 1 < length (input ctx)
    then return $ Just (input ctx !! (at ctx + 1))
    else return Nothing

current :: Parser (Maybe LexerToken)
current = do
  ctx <- get
  if at ctx < length (input ctx)
    then return $ Just (input ctx !! at ctx)
    else return Nothing

currentSymTab :: Parser SymbolTable
currentSymTab = do
  ctx <- get
  let symbol_table_id = fromMaybe 0 (using ctx)
      symbol_table = Map.lookup symbol_table_id (sym_tables ctx)
   in case symbol_table of
        Just st -> return st
        Nothing -> error "Could not find default symbol table, or current symbol (how is this possible?)"

getSymbol :: String -> Parser SymbolInfo 
getSymbol = do
  ctx <- get
  error "TODO!"

-- Arguemnt lists are universal ... one big blanket function, yay!
-- make the whole `Maybe LexerToken` nicer? I cant decide if the
-- optioanl value should be handlered her
parseArgList :: SymbolTable -> Maybe LexerToken -> Parser SymbolTable
parseArgList a (Just OpenP) = do
  modify (\ctx -> ctx {at = at ctx + 1})
  ctx <- get
  current <- current
  inner_arg_list <- parseArgList SymbolTable {table = Map.empty, name_to_id = Map.empty, owner = Nothing, last_id = 0} current
  error ("Inner arg list: " ++ show inner_arg_list)
parseArgList a (Just Comma) = do
  modify (\ctx -> ctx {at = at ctx + 1})
  ctx <- get
  current <- current
  parseArgList a current
parseArgList a (Just DColon) = do
  modify (\ctx -> ctx {at = at ctx + 1})
  return a
parseArgList _ (Just a) = do
  parsed <- parse a
  error ("Parsed Argument " ++ show parsed)
parseArgList _ a = do
  error ("Unexpected Argument " ++ show a)

parse :: LexerToken -> Parser (Maybe Expr)
parse (Literal (Ident i)) = do
  modify (\ctx -> ctx {at = at ctx + 1})
  current >>= \case
    Just Backslash -> do
      modify (\ctx -> ctx {at = at ctx + 1})
      current <- current
      -- I call this starting_symbol_table instead of argument list
      -- smth like that because I have decided that there will be
      -- no clear disctinction of lambda symbol table and the lambda
      -- argument list because at the end of the day the arguments
      -- introducted will have to be in the symbol table to be used.
      starting_symbol_table <- parseArgList SymbolTable {table = Map.empty, name_to_id = Map.empty, owner = Nothing, last_id = 0} current
      return (Just Place)
    Just (Literal (Ident i)) -> do
      error ("Sub-ident: " ++ i)
    a -> error ("ASC Unkown: " ++ show a)
parse a = error ("Unkown: " ++ show a)

parseAll :: Parser [Expr]
parseAll = do
  ctx <- get
  current <- current
  case current of
    Just a -> parse a >> parseAll
    Nothing -> return (reverse $ results ctx)