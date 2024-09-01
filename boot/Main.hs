import Control.Monad (when)
import Control.Monad.State
import Data.Char (isAlpha, isAlphaNum)
import Data.Map qualified as Map
import Parser (parse)
import Tokens (TokTy (Backslash, Bang, CharLit, CloseCurlP, CloseP, Colon, DColon, Dash, Eq, GreaterThan, Identifier, LessThan, NewLine, OpenCurlP, OpenP, Plus, StrLit))

isTokStr :: TokTy -> Bool
isTokStr (StrLit _) = True
isTokStr (CharLit _) = True
isTokStr _ = False

data Context a = Context
  { input :: a,
    at :: Int,
    results :: [TokTy]
  }
  deriving (Show)

type Lexer a = State (Context String) a

peek :: Int -> Lexer (Maybe Char)
peek n = do
  ctx <- get
  let i = at ctx
      len = length (input ctx)
  return $ if i + n < len then Just (input ctx !! (i + n)) else Nothing

initialLexer :: String -> Context String
initialLexer input =
  Context
    { input = input,
      at = 0,
      results = []
    }

data CollectTy = String | Char | Ident | Comment

lexCollectUntil :: CollectTy -> String -> (Char -> Bool) -> (Maybe TokTy, Int)
lexCollectUntil ct cs check =
  let (ident, rest) = span check cs
   in case ct of
        String -> (Just (StrLit ident), length ident)
        Ident -> (Just (Identifier ident), length ident)
        Char -> (Just (CharLit (head ident)), 1)
        Comment -> (Nothing, length ident)

lexCollectUntilHandleAll :: (Maybe TokTy, Int) -> Lexer [TokTy]
lexCollectUntilHandleAll lexed_val = do
  ctx <- get
  let i = at ctx
  let (lexed, len) = lexed_val
  case lexed of
    Just ident -> do
      modify $ \ctx -> ctx {results = ident : results ctx}
      modify $ \ctx ->
        if isTokStr ident
          then do ctx {at = i + len + 2}
          else ctx {at = i + len}
    Nothing -> error "Failed to do that one thing... (1)"
  gets results

determineCollectTyAndCheck :: Char -> Int -> (CollectTy, Int, Char -> Bool)
determineCollectTyAndCheck '"' at = (String, at + 1, (/= '"'))
determineCollectTyAndCheck '\'' at = (Char, at + 1, (/= '\''))
determineCollectTyAndCheck c at
  | isAlphaNum c || c == '_' = (Ident, at, \ch -> isAlphaNum ch || ch == '_')
determineCollectTyAndCheck _ _ = error "Unsupported character"

lexAll :: Lexer [TokTy]
lexAll = do
  ctx <- get
  let i = at ctx
  if i >= length (input ctx)
    then return (reverse $ results ctx)
    else do
      let currentChar = input ctx !! i
      if isAlphaNum currentChar || currentChar == '_' || currentChar == '"' || currentChar == '\''
        then do
          let (collect_ty, drop_from, check) = determineCollectTyAndCheck currentChar i
          lexCollectUntilHandleAll (lexCollectUntil collect_ty (drop drop_from (input ctx)) check)
          lexAll
        else do
          case ( case currentChar of
                   ':' -> Just Colon
                   '!' -> Just Bang
                   '\n' -> Just NewLine
                   '(' -> Just OpenP
                   ')' -> Just CloseP
                   '{' -> Just OpenCurlP
                   '}' -> Just CloseCurlP
                   '=' -> Just Eq
                   '-' -> Just Dash
                   '+' -> Just Plus
                   '>' -> Just GreaterThan
                   '<' -> Just LessThan
                   '\\' -> Just Backslash
                   ' ' -> Nothing
                   _ -> error $ "Unknown character: " ++ [currentChar]
               ) of
            Just ty -> do
              put ctx {at = i + 1, results = ty : results ctx}
              lexAll
            Nothing -> do
              put ctx {at = i + 1}
              lexAll

main :: IO ()
main = do
  readdFile <- readFile "./tests/1.cyl"
  let lexer = initialLexer readdFile
      result = evalState lexAll lexer
  print result
