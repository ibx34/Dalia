module Parser where

import Control.Monad.State
import Tokens (Context (Context, at, input, results), TokTy (Colon, Identifier))

data Expr = Ident deriving (Show)

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
      results = []
    }

parseIdent :: TokTy -> Parser [Expr]
parseIdent (Identifier ident) = do
  ctx <- get
  let i = at ctx
  first <- peek 0
  second <- peek 1
  case (first, second) of
    (Just Colon, Just Colon) -> do
      modify $ \ctx -> ctx {at = i + 2, results = Ident : results ctx}
      -- put ctx {at = i + 1, results = expr : results ctx}
      error "Function def"
    (_, _) -> error "Exected something.."
  gets results

parse :: Parser [Expr]
parse = do
  ctx <- get
  let i = at ctx
  if i >= length (input ctx)
    then return (reverse $ results ctx)
    else do
      let currentTok = input ctx !! i
      case currentTok of
        Identifier _ -> do
          modify $ \ctx -> ctx {at = i + 1}
          parseIdent currentTok
        _ -> error "Unkown...?"
      parse
