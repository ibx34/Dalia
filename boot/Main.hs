module Main where

import GHC.IO.FD (openFile)
import GHC.IO.IOMode (IOMode (ReadMode))
import Control.Monad.State

data Context a = Context
  { input :: a,
    at :: Int
  }
  deriving (Show)

type Lexer a =  State (Context String)

initialLexer :: String -> Context String
initialLexer input = Context {
    input = input,
    at = 0
}

main :: IO ()
main = do
  testFile <- readFile "./tests/1.cyl"
  print $ initialLexer testFile