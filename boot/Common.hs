module Common where

import Data.Map qualified as Map
import Data.Maybe (isJust)
import Data.IntMap (Key)

data Expr deriving (Show, Eq)

data Keywords = TypeDef deriving (Show, Eq)

data Literals = String String | Char Char | Int Int deriving (Show, Eq)

data LexerToken
  = Identifier String
  | Keyword Keywords
  | Literal Literals
  | Bang
  | OpenP
  | CloseP
  | Eq
  | GreaterThan
  | LessThan
  | Dash
  | Pipe
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
    is_comment :: Bool,
    at_block :: Int
  }
  deriving (Show)

class IsWorkingOnMultiItem c where
  isWorkingOnMultiItem :: c i r b -> Bool
  isCurrentMultiItemComment :: c i r b -> Bool

instance IsWorkingOnMultiItem Context where
  isWorkingOnMultiItem (Context {c_multi_item}) = isJust c_multi_item
  isCurrentMultiItemComment (Context {is_comment}) = is_comment