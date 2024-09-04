module Common where

import Data.Map qualified as Map

data Expr deriving (Show, Eq)

data Keywords = TypeDef
 
data Literals = String String | Char Char | Int Int
data LexerToken = Identifier String 
  | Literal Literals
  | OpenP
  | CloseP
  | Eq 
  | GreaterThan
  | LessThan 
  | Dash
  | Plus
  | DColon
  | Colon 
  | ForwardSlash
  | Backslash
  | OpenCurlyP
  | CloseCurlyP
  | OpenSquareP
  | CloseSquareP
  | SingleQuote
  | DoubleQuote
  

data SymbolInfo = SymbolInfo
  { _type :: String,
    val :: Maybe Expr,
    name :: String,
    block :: Int
  }
  deriving (Show, Eq)

type SymbolTable = Map.Map String SymbolInfo

data Block = Block
  { id :: Int,
    symbols :: SymbolTable
  }
  deriving (Show, Eq)

data Context i r = Context
  { input :: i,
    at :: Int,
    results :: [r],
    sym_table :: SymbolTable,
    blocks :: [Block],
    at_block :: Int
  }
  deriving (Show)