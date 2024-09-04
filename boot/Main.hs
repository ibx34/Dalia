import Control.Monad (when)
import Control.Monad.State
import Data.Char (isAlpha, isAlphaNum)
import Data.Map qualified as Map

main :: IO ()
main = do
  readdFile <- readFile "./tests/1.cyl"
  print readdFile
--   let lexer = initialLexer readdFile
--       lexerRes = evalState lexAll lexer
--       parser = initialParser lexerRes
--       result = evalState parse parser
--    in do
--         print lexerRes
--         print result
