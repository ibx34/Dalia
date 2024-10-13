{-# LANGUAGE LambdaCase #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Parser where

import Common (Context (Context, at, c_multi_item, input, is_comment, results, sym_tables, using), Expr (PrimitiveType, SymbolReference), Keywords (..), LexerToken (..), Literals (..), Primes (Type), PrimitiveType (Int'), Symbol (Symbol, from_lib, name, val), SymbolTable (SymbolTable, last_id, name_to_id, parent, symbols), isCurrentMultiItemComment, isWorkingOnMultiItem, si, st)
import Control.Monad (void, when)
import Control.Monad qualified
import Control.Monad.State (MonadState (get, put), State, gets, join, modify)
import Data.Bool (bool)
import Data.Char (isAlphaNum, isNumber)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Type.Coercion (sym)
import Distribution.Compat.CharParsing (CharParsing (string))
import Distribution.Compiler (CompilerFlavor (JHC))
import Lexer (Lexer)
import Prelude hiding (lex)

type ParserContext = Common.Context [LexerToken] Expr Expr

type Parser a = State ParserContext a

-- TODO: Make helper fn to make creating maps + insert nicer PLSSS
createParser :: [LexerToken] -> ParserContext
createParser a =
  let sym_tables =
        Map.fromList
          [ ( 0,
              SymbolTable
                { symbols =
                    Map.fromList
                      [ ( 0,
                          Symbol
                            { val = Just (PrimitiveType Int'),
                              name = "int",
                              from_lib = Nothing
                            }
                        )
                      ],
                  name_to_id = Map.fromList [("int", 0)],
                  last_id = 0,
                  parent = Nothing
                }
            )
          ]
   in Context
        { input = a,
          at = 0,
          results = [],
          using = 0,
          sym_tables = sym_tables,
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

getSymboltable :: Int -> Parser SymbolTable
getSymboltable i = do
  ctx <- get
  let symbol_table = Map.lookup i (sym_tables ctx)
   in case symbol_table of
        Just st -> return st
        Nothing -> error ("Could not find default symbol table, or current symbol\n\n" ++ show (sym_tables ctx))

getSymbol :: String -> Maybe Int -> Parser (Symbol, Int, Int)
getSymbol s st =
  get >>= \ctx ->
    let symbol_table_id = fromMaybe (using ctx) st
     in getSymboltable symbol_table_id >>= \current_symbol_table ->
          case Map.lookup s (name_to_id current_symbol_table) of
            Just symbol_id -> case Map.lookup symbol_id (symbols current_symbol_table) of
              Just symbol -> return (symbol, symbol_id, symbol_table_id)
              Nothing -> error "TODO!!"
            Nothing -> do
              getSymbol
                s
                ( case parent current_symbol_table of
                    Just id -> Just id
                    Nothing -> error ("Could not find the symbol '" ++ s ++ "'")
                )

insertSymbol :: SymbolTable -> String -> Expr -> Parser SymbolTable
insertSymbol st n e =
  let next_id = last_id st + 1
   in return
        SymbolTable
          { symbols =
              Map.insert
                next_id
                Symbol
                  { val = Just e,
                    name = n,
                    from_lib = Nothing
                  }
                (symbols st),
            parent = parent st,
            last_id = next_id,
            name_to_id = Map.insert n next_id (name_to_id st)
          }

parseLambdaParameters :: Maybe LexerToken -> SymbolTable -> Parser SymbolTable
parseLambdaParameters (Just (Literal (Ident i))) st = do
  modify (\ctx -> ctx {at = at ctx + 1})
  current >>= \case
    Just c ->
      parse c >>= \expr -> do
        st' <- insertSymbol st i expr
        current >>= \c -> parseLambdaParameters c st'
    Nothing -> error "Current was Nothing."
parseLambdaParameters (Just DColon) st = do
  modify (\ctx -> ctx {at = at ctx + 1})
  return st
parseLambdaParameters (Just Comma) st = do
  modify (\ctx -> ctx {at = at ctx + 1})
  current >>= \c -> parseLambdaParameters c st

parse :: LexerToken -> Parser Expr
parse Backslash = do
  modify (\ctx -> ctx {at = at ctx + 1})
  parameters <- current >>= \c -> parseLambdaParameters c SymbolTable {symbols = Map.empty, name_to_id = Map.empty, parent = Nothing, last_id = 0}
  error (show parameters)
parse (Literal (Ident i)) = do
  (symbol, si', st') <- getSymbol i Nothing
  modify (\ctx -> ctx {at = at ctx + 1})
  return
    SymbolReference
      { si = si',
        st = st'
      }
parse x = error ("Unkown: " ++ show x)

parseAll :: Parser [Expr]
parseAll = do
  ctx <- get
  current <- current
  case current of
    Just a -> parse a >> parseAll
    Nothing -> return (reverse $ results ctx)