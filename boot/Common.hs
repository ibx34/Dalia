module Common where

import Data.Map qualified as Map

data Expr deriving (Show, Eq)

data Keywords = TypeDef deriving (Show, Eq)

data Literals = String String | Char Char | Int Int deriving (Show, Eq)

data LexerToken
  = Identifier String
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
  | Comment String -- We dont care, probably keep it for later use.
  deriving (Show, Eq)

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

data Context i r b = Context
  { input :: i,
    at :: Int,
    results :: [r],
    sym_table :: SymbolTable,
    blocks :: [Block],
    c_multi_item :: Maybe [b],
    at_block :: Int
  }
  deriving (Show)

-- class BasicEq a where
--    isEqual :: a -> a -> Bool