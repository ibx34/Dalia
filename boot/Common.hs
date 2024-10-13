module Common where

import Data.IntMap (Key)
import Data.Kind (Type)
import Data.Map
import Data.Map qualified as Map
import Data.Maybe (isJust)
import Text.Parsec.Token (GenTokenParser (symbol))

-- TODO: when we get to the llvm add a function to convert these into
-- some llvm type shi
data PrimitiveType = Int' deriving (Show, Eq)

data Expr
  = SymbolReference
      { st :: Int,
        si :: Int
      }
  | PrimitiveType PrimitiveType
  deriving (Show, Eq)

data Symbol = Symbol
  { name :: String,
    val :: Maybe Expr,
    from_lib :: Maybe Int
  }
  deriving (Show, Eq)

data SymbolTable = SymbolTable
  { symbols :: Map Int Symbol,
    name_to_id :: Map String Int,
    last_id :: Int,
    parent :: Maybe Int
  }
  deriving (Show, Eq)

getSymbol :: SymbolTable -> Int -> Maybe Symbol
getSymbol (SymbolTable {symbols}) id = Map.lookup id symbols

getSymbolByName :: SymbolTable -> String -> Maybe Symbol
getSymbolByName st name = Map.lookup name (name_to_id st) >>= \id -> getSymbol st id

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

data Context i r b = Context
  { input :: i,
    at :: Int,
    results :: [r],
    -- 0 is always the like "global"(?) symbol table
    sym_tables :: Map Int SymbolTable,
    using :: Int,
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