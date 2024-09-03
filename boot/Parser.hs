module Parser where

import Control.Monad.State
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Tokens (Context (Context, at, at_block, input, results, symbols), Expr (Unit), TokTy (CloseP, Colon, Dash, Eq, GreaterThan, Identifier, OpenP))

type PContext = Context [TokTy] Expr

type Parser a = State PContext a

peek :: Int -> Parser (Maybe TokTy)
peek n = do
  ctx <- get
  let i = at ctx
      len = length (input ctx)
  return $ if i + n < len then Just (input ctx !! (i + n)) else Nothing

initialParser :: [TokTy] -> PContext
initialParser input =
  Context
    { input = input,
      at = 0,
      results = [],
      symbols = Map.empty,
      at_block = 0
    }

-- Handles right side (of =, not ::) of assignment
parseAssignment :: [TokTy] -> Parser (Maybe Expr)
parseAssignment a = do
  let a = filter (\a -> a /= Dash || a /= GreaterThan) a
  ctx <- get
  modify $ \ctx -> ctx {at = length (input ctx) + 1}
  error (show a)

parseIdent :: TokTy -> Parser (Maybe Expr)
parseIdent (Identifier ident) = do
  ctx <- get
  let i = at ctx
  first <- peek 0
  second <- peek 1
  case (first, second) of
    -- Handles left side (of =, not ::) of assignment
    (Just Colon, Just Colon) -> do
      modify $ \ctx -> ctx {at = i + 2} --  results = Ident "Whatever" : results ctx
      --   let (ident, rest) = span check cs
      let (arg_list, rest) = span (== Eq) (drop i (input ctx))
      parseAssignment arg_list
    (_, _) -> return Nothing

parseExpr :: Maybe TokTy -> Parser (Maybe Expr)
parseExpr a = do
  ctx <- get
  let i = at ctx
  if i >= length (input ctx)
    then return Nothing -- Return Nothing on EOF
    else do
      let currentTok = fromMaybe (input ctx !! i) a
      case currentTok of
        OpenP -> do
          modify $ \ctx -> ctx {at = i + 1}
          peeked <- peek 0
          case peeked of
            Just CloseP -> do
                modify $ \ctx -> ctx {at = i + 1}
                return (Just Unit)
            _ -> return Nothing
        Identifier _ -> do
          modify $ \ctx -> ctx {at = i + 1}
          parseIdent currentTok
        _ -> error "Unknown token"

parse :: Parser [Expr]
parse = do
  maybeExpr <- parseExpr Nothing
  case maybeExpr of
    Nothing -> gets (reverse . results)
    Just expr -> do
      modify $ \ctx -> ctx {results = expr : results ctx}
      parse
