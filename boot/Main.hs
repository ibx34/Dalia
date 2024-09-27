import Control.Monad (when)
import Control.Monad.State
import Data.Char (isAlpha, isAlphaNum)
import Data.Map qualified as Map
import Distribution.Compat.CharParsing (lower)
import Lexer (createLexer, lexAll)
import LowerFirstStep (createStairCase, stepAll)

main :: IO ()
main = do
  readdFile <- readFile "./tests/1.dal"
  let lexer = createLexer readdFile
      lexerRes = evalState lexAll lexer
      stairCase = createStairCase lexerRes
      lowered = evalState stepAll stairCase
  -- print lexerRes
  print lowered