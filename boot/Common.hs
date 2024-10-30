{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Common where

import Data.IntMap (Key)
import Data.Kind (Type)
import Data.Map
import Data.Map qualified as Map
import Data.Maybe (isJust)
import Text.Parsec.Token (GenTokenParser (symbol))

data Literals = Ident String | String String | Char Char | Int Int deriving (Show, Eq, Ord)

data Associativity = Left' | Right' | Non' deriving (Show, Eq)

data Operators = Plus | Minus | LeftP | RightP deriving (Show, Eq)

precedenceAndAssociativity :: LexerToken -> (Int, Associativity)
precedenceAndAssociativity Plus' = (1, Left')

ltToOp :: LexerToken -> Operators
ltToOp Plus' = Plus

opToLt :: Operators -> LexerToken
opToLt Plus = Plus'

isOp :: LexerToken -> Bool
isOp Plus' = True
isOp _ = False

data ShuntingYardAlgoData = SYAExpr Expr | SYAOp Operators deriving (Show, Eq)

-- TODO: when we get to the llvm add a function to convert these into
-- some llvm type shi
data PrimitiveType = Int' deriving (Show, Eq)

data Expr
  = SymbolReference
      { st :: Int,
        si :: Int
      }
  | BinaryOp Operators Expr Expr
  | SYA [ShuntingYardAlgoData]
  | Assignment
      { left :: Expr,
        right :: Expr,
        op :: Int
      }
  | Fake
  | Lambda
      { parameters :: SymbolTable,
        expr :: Expr
      }
  | Literal' Literals
  | PrimitiveType PrimitiveType
  deriving (Show, Eq)

data Symbol = Symbol
  { name :: Literals,
    val :: Maybe Expr,
    from_lib :: Maybe Int
  }
  deriving (Show, Eq)

data SymbolTable = SymbolTable
  { symbols :: Map Int Symbol,
    name_to_id :: Map Literals Int,
    last_id :: Int,
    parent :: Maybe Int
  }
  deriving (Show, Eq)

getSymbol :: SymbolTable -> Int -> Maybe Symbol
getSymbol (SymbolTable {symbols}) id = Map.lookup id symbols

getSymbolByName :: SymbolTable -> Literals -> Maybe Symbol
getSymbolByName st name = Map.lookup name (name_to_id st) >>= \id -> getSymbol st id

data Keywords = TypeDef deriving (Show, Eq)

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
  | Plus'
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
    -- The idea behind `temp_output` is that we can push
    -- expressions onto it for the Shunting Yard algorithm
    -- and when we end that expression we can clear it
    -- and move onto next expression
    temp_output :: [r],
    -- 0 is always the like "global"(?) symbol table
    sym_tables :: Map Int SymbolTable,
    last_symbol_table_id :: Int,
    op_stack :: [Operators],
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