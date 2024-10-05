module Common where

import Data.IntMap (Key)
import Data.Kind (Type)
import Data.Map
import Data.Map qualified as Map
import Data.Maybe (isJust)

data RefType = RType | Function | Variable deriving (Show, Eq)

data Expr
  = Lambda
      { symbols :: SymbolTable,
        expr :: Expr
      }
  | Assignment
      { left :: Expr,
        right :: Expr
      }
  | Reference
      { ref_ty :: RefType,
        id :: Int,
        symbol_table :: Maybe Int
      }
  | Place
  deriving (Show, Eq)

data Keywords = TypeDef deriving (Show, Eq)

data Literals = Ident String | String String | Char Char | Int Int deriving (Show, Eq)

data Primes = Type deriving (Show, Eq)

data LexerToken
  = Identifier String
  | Prime Primes
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
  | Comma
  | Plus
  | FunctionArrow
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

data SymbolType = Assignment' | PrimitiveType deriving (Show, Eq)

data SymbolInfo = SymbolInfo
  { _type :: SymbolType,
    val :: Maybe Expr,
    name :: String
  }
  deriving (Show, Eq)

data SymbolTable = SymbolTable
  { table :: Map.Map Int SymbolInfo,
    name_to_id :: Map.Map String Int,
    last_id :: Int,
    owner :: Maybe Int
  }
  deriving (Show, Eq)

data Context i r b = Context
  { input :: i,
    at :: Int,
    results :: [r],
    -- 0 is always the like "global"(?) symbol table
    sym_tables :: Map Int SymbolTable,
    using :: Maybe Int,
    c_multi_item :: Maybe [b],
    is_comment :: Bool
  }
  deriving (Show)

class IsWorkingOnMultiItem c where
  isWorkingOnMultiItem :: c i r b -> Bool
  isCurrentMultiItemComment :: c i r b -> Bool

instance IsWorkingOnMultiItem Context where
  isWorkingOnMultiItem (Context {c_multi_item}) = isJust c_multi_item
  isCurrentMultiItemComment (Context {is_comment}) = is_comment