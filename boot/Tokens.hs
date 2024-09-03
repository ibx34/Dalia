module Tokens where

import Data.Map qualified as Map

data Expr
  = Ident String
  | Unit
  | Assignment
      { left :: Expr,
        binder :: (),
        right :: Expr
      }
  | Fn
      { types :: [Expr],
        args :: [Expr],
        body :: Expr
      }
  deriving (Show, Eq)

type SymbolTable = Map.Map String SymbolInfo

data SymbolInfo = SymbolInfo
  { _type :: String, 
    val :: Maybe Expr,
    name :: String,
    block :: Int
  }
  deriving (Show, Eq)

data Context i r = Context
  { input :: i,
    at :: Int,
    results :: [r],
    symbols :: SymbolTable,
    at_block :: Int
  }
  deriving (Show)

data TokTy = Identifier String | StrLit String | CharLit Char | Bang | Colon | DColon | OpenP | CloseP | OpenCurlP | CloseCurlP | Eq | GreaterThan | LessThan | Dash | Plus | Backslash | NewLine
  deriving (Show, Eq)