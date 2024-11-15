{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Parser where

import Common (Associativity (Left'), Context (Context, at, c_multi_item, input, is_comment, last_symbol_table_id, op_stack, results, sym_tables, temp_output, using), Expr (Assignment, Fake, Lambda, Literal', PrimitiveType, SYA, SymbolReference, expr, left, op, parameters, right), Keywords (..), LexerToken (..), Literals (..), Operators (Plus), Primes (Type), PrimitiveType (Int'), ShuntingYardAlgoData (SYAExpr, SYAOp), Symbol (Symbol, from_lib, name, val), SymbolTable (SymbolTable, last_id, name_to_id, parent, symbols), isCurrentMultiItemComment, isOp, isWorkingOnMultiItem, ltToOp, opToLt, precedenceAndAssociativity, si, st)
import Control.Monad (void, when)
import Control.Monad qualified
import Control.Monad.State (MonadState (get, put), State, gets, join, modify)
import Data.Bits (Bits (xor))
import Data.Bool (bool)
import Data.Char (isAlphaNum, isNumber)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Type.Coercion (sym)
import Distribution.Compat.CharParsing (CharParsing (string))
import Distribution.Compiler (CompilerFlavor (JHC))
import Lexer (Lexer)
import Text.Parsec (token)
import Text.Parsec.Expr (Operator)
import Prelude hiding (lex)
import Debug.Trace (trace)

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
                              name = Ident "int",
                              from_lib = Nothing
                            }
                        )
                      ],
                  name_to_id = Map.fromList [(Ident "int", 0)],
                  last_id = 0,
                  parent = Nothing
                }
            )
          ]
   in Context
        { input = a,
          at = 0,
          results = [],
          op_stack = [],
          temp_output = [],
          last_symbol_table_id = 0,
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

getSymbol :: Literals -> Maybe Int -> Parser (Maybe (Symbol, Int, Int))
getSymbol s st =
  get >>= \ctx ->
    let symbol_table_id = fromMaybe (using ctx) st
     in getSymboltable symbol_table_id >>= \current_symbol_table ->
          case Map.lookup s (name_to_id current_symbol_table) of
            Just symbol_id -> case Map.lookup symbol_id (symbols current_symbol_table) of
              Just symbol -> return (Just (symbol, symbol_id, symbol_table_id))
              Nothing -> return Nothing
            Nothing -> do
              if st == Just 0
                then return Nothing
                else getSymbol s (Just (fromMaybe 0 (parent current_symbol_table)))

insertSymbol :: SymbolTable -> Literals -> Expr -> Parser SymbolTable
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

-- parseLambdaParameters :: Maybe LexerToken -> SymbolTable -> Parser SymbolTable
-- parseLambdaParameters (Just (Literal lit@(Ident i))) st = do
--   modify (\ctx -> ctx {at = at ctx + 1})
--   current >>= \case
--     Just c ->
--       parse c >>= \expr -> do
--         st' <- insertSymbol st lit expr
--         current >>= \c -> parseLambdaParameters c st'
--     Nothing -> error "Current was Nothing1."
-- parseLambdaParameters (Just DColon) st = do
--   modify (\ctx -> ctx {at = at ctx + 1})
--   current >>= \case
--     Just c ->
--       parse c >>= \expr -> do
--         st' <- insertSymbol st (Ident "ret") expr
--         return st
--     Nothing -> error "Current was Nothing2."
-- parseLambdaParameters (Just Comma) st = do
--   modify (\ctx -> ctx {at = at ctx + 1})
--   current >>= \c -> parseLambdaParameters c st

-- safeLast :: [a] -> Parser (Maybe a)
-- safeLast [] = return Nothing
-- safeLast xs = return (Just (last xs))

-- shuntingYardAlgo :: String -> Maybe LexerToken -> [Operators] -> [ShuntingYardAlgoData] -> Parser Expr
-- shuntingYardAlgo a Nothing os oq = return (SYA (oq ++ map SYAOp os))
-- shuntingYardAlgo a (Just tok) os oq
--   | tok == Dash || tok == Plus' =
--       safeLast os >>= \case
--         Just last ->
--           let (nop_prec, nop_assoc) = precedenceAndAssociativity tok
--               (last_prec, last_assoc) = precedenceAndAssociativity (opToLt last)
--            in if last_prec >= nop_prec && nop_assoc == Left'
--                 then
--                   current >>= \case
--                     Just c ->
--                       let (x : xs) = reverse os
--                        in shuntingYardAlgo "1" (Just c) (ltToOp tok : xs) (SYAOp last : oq)
--                     Nothing -> return $ SYA (oq ++ map SYAOp os)
--                 else error "Infix parsing ended unexpectedly"
--         Nothing ->
--           current >>= \case
--             Just c -> do
--               modify (\ctx -> ctx {at = at ctx + 1})
--               shuntingYardAlgo "1" (Just c) (ltToOp tok : os) oq
--             Nothing -> return $ SYA (oq ++ map SYAOp os)
--   | otherwise = do
--       -- Other cases handle literals and expressions directly
--       case tok of
--         (Literal lit) -> do
--           parsed <- parse tok
--           modify (\ctx -> ctx {at = at ctx + 1})
--           current >>= \c -> shuntingYardAlgo "1" c os (SYAExpr parsed : oq)

-- parse :: LexerToken -> Parser Expr
-- parse tok@(Literal lit) = do
--   getSymbol lit Nothing >>= \case
--     Just (symbol, si', st') -> do
--       modify (\ctx -> ctx {at = at ctx + 1})
--       return SymbolReference {si = si', st = st'}
--     Nothing -> do
--       modify (\ctx -> ctx {at = at ctx + 1})
--       current >>= \case
--         Just c
--           | c == Eq -> do
--               modify (\ctx -> ctx {at = at ctx + 1})
--               following_expr <-
--                 current >>= \case
--                   Just c' -> parse c'
--                   Nothing -> error "Current was none4..."
--               return Assignment {left = Literal' lit, right = following_expr, op = 0}
--           | otherwise -> return (Literal' lit)
--         Nothing -> return (Literal' lit)
-- parse Backslash = do
--   modify (\ctx -> ctx {at = at ctx + 1})
--   parameters <- current >>= \c -> parseLambdaParameters c SymbolTable {symbols = Map.empty, name_to_id = Map.empty, parent = Nothing, last_id = 0}
--   ctx <- get
--   let new_symbol_table_id = (last_symbol_table_id ctx + 1)
--    in do
--         modify (\ctx -> ctx {sym_tables = Map.insert new_symbol_table_id parameters (sym_tables ctx), using = new_symbol_table_id})
--         current >>= \case
--           Just FunctionArrow -> do
--             modify (\ctx -> ctx {at = at ctx + 1})
--             current >>= \case
--               Just c -> do
--                 expr <- parse c
--                 modify (\ctx -> ctx {using = 0})
--                 return
--                   Lambda
--                     { expr = expr,
--                       parameters = parameters
--                     }
--               Nothing -> error "Current was Nothing3."
--           a -> error ("Expected function arrow to follow ret of lambda, instead got: " ++ show a)
-- parse x = error ("Unkown: " ++ show x)

parseAll :: Parser [Expr]
parseAll = do
  ctx <- get
  current <- current
  case current of
    Just a -> do
      parsed <- parse a
      modify (\ctx -> ctx {results = parsed : results ctx, at = at ctx + 1})
      parseAll
    Nothing -> return (reverse $ results ctx)