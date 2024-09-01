import Control.Monad (when)
import Control.Monad.State
import Data.Char (isAlpha, isAlphaNum)
import Data.Map qualified as Map

-- StrLit is temporary
data TokTy = Identifier String | StrLit String | Bang | Colon | OpenP | CloseP | OpenCurlP | CloseCurlP | Eq
  deriving (Show, Eq)

isTokStr :: TokTy -> Bool
isTokStr (StrLit _) = True
isTokStr _ = False

data Context a = Context
  { input :: a,
    at :: Int,
    results :: [TokTy]
  }
  deriving (Show)

type Lexer a = State (Context String) a

initialLexer :: String -> Context String
initialLexer input =
  Context
    { input = input,
      at = 0,
      results = []
    }

lexOne :: Char -> Maybe TokTy
lexOne x = case x of
  '!' -> Just Bang
  ':' -> Just Colon
  '(' -> Just OpenP
  ')' -> Just CloseP
  '{' -> Just OpenCurlP
  '}' -> Just CloseCurlP
  '=' -> Just Eq
  _ -> Nothing

data CollectTy = String | Ident | Comment

lexCollectUntil :: CollectTy -> String -> (Char -> Bool) -> (Maybe TokTy, Int)
lexCollectUntil ct cs check =
  let (ident, rest) = span check cs
   in case ct of
        String -> (Just (StrLit ident), length ident)
        Ident -> (Just (Identifier ident), length ident)
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

lexAll :: Lexer [TokTy]
lexAll = do
  ctx <- get
  let i = at ctx
  if i >= length (input ctx)
    then return (reverse $ results ctx)
    else do
      let currentChar = input ctx !! i
      if isAlphaNum currentChar || currentChar == '_'
        then do
          lexCollectUntilHandleAll (lexCollectUntil Ident (drop i (input ctx)) (\c -> isAlphaNum c || c == '_'))
          lexAll
        else
          if currentChar == '"'
            then do
              lexCollectUntilHandleAll (lexCollectUntil String (drop (i + 1) (input ctx)) (/= '"'))
              lexAll
            else case lexOne currentChar of
              Just token -> do
                put ctx {at = i + 1, results = token : results ctx}
                lexAll
              Nothing -> do
                put ctx {at = i + 1} -- Skip unknown characters
                lexAll

-- Example usage with a file read and lexing
main :: IO ()
main = do
  readdFile <- readFile "./tests/1.cyl"
  let lexer = initialLexer readdFile
      result = evalState lexAll lexer
  print result
