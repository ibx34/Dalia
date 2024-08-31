import Control.Monad.State

data TokTy = OpenP | CloseP | DColon | Colon | Eq | Dash | GThan | LThan | OpenCurlP | CloseCurlP | OpenSquareP | CloseSquareP

data Context a = Context
  { input   :: a,
    at      :: Int,
    current :: Char,
    results :: [Char]
  }
  deriving (Show)

type Lexer a = State (Context String) a

initialLexer :: String -> Context String
initialLexer input =
  Context
    { input = input,
      at = 0,
      current = head input,
      results = []
    }

lexAll :: Lexer [Char]
lexAll = do
  ctx <- get
  let i = at ctx
  if i >= length (input ctx)
    then return (reverse $ results ctx)
    else do
      let newChar = input ctx !! i
      put ctx { at = i + 1, current = newChar, results = newChar : results ctx }
      lexAll

main :: IO ()
main = do
  readdFile <- readFile "./tests/1.cyl"
  let lexer = initialLexer readdFile
      result = evalState lexAll lexer
  print result
