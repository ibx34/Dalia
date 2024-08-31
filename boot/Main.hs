import Control.Monad.State
import qualified Data.Map as Map
import Data.Char (isAlpha, isAlphaNum)

-- StrLit is temporary
data TokTy = Identifier String | StrLit String | Bang | Colon | OpenP | CloseP | OpenCurlP | CloseCurlP | Eq
    deriving (Show, Eq)

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
    _   -> Nothing

lexIdentifier :: String -> (TokTy, Int)
lexIdentifier cs =
    let (ident, rest) = span isIdentChar cs
    in (Identifier ident, length ident)

lexStrLit :: String -> (TokTy, Int)
lexStrLit cs =
    let (ident, rest) = span isStrChar cs
    in (StrLit ident, length ident)

isStrChar :: Char -> Bool
isStrChar c = c /= '"'

isIdentChar :: Char -> Bool
isIdentChar c = isAlphaNum c || c == '_'

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
          let (identifierToken, len) = lexIdentifier (drop i (input ctx))
          put ctx {at = i + len, results = identifierToken : results ctx}
          lexAll
        else if currentChar == '"'
         then do
           let (stringLiteralToken, len) = lexStrLit (drop (i + 1) (input ctx))
           put ctx {at = i + len + 2, results = stringLiteralToken : results ctx}
           lexAll
        else case lexOne currentChar of
          Just token -> do
            put ctx {at = i + 1, results = token : results ctx}
            lexAll
          Nothing -> do
            put ctx {at = i + 1}  -- Skip unknown characters
            lexAll

-- Example usage with a file read and lexing
main :: IO ()
main = do
  readdFile <- readFile "./tests/1.cyl"
  let lexer = initialLexer readdFile
      result = evalState lexAll lexer
  print result
