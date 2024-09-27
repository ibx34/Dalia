{-# LANGUAGE LambdaCase #-}

module LowerFirstStep where

import Common (Context (Context, at, at_block, blocks, c_multi_item, input, is_comment, results, sym_table), Keywords (..), LexerToken (..), Literals (..), Primes (Type), isCurrentMultiItemComment, isWorkingOnMultiItem)
import Control.Monad (void, when)
import Control.Monad qualified
import Control.Monad.State (MonadState (get, put), State, gets, join, modify)
import Data.Bool (bool)
import Data.Char (isAlphaNum, isNumber)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Prelude hiding (lex)

data RefType = Type' deriving (Show, Eq)

data LoweredExpr
  = -- Reference allows for quick lookup of types, lambdas, etc.
    -- used throughout the current program. The id field
    -- correlates to a type, monad, whatever may be referenced
    -- in the central registry so we dont have to pass
    -- around all that extra baggage for simple jobs.
    Reference
      { ref_ty :: RefType,
        id :: Int,
        symbol_table :: Maybe Int
      }
  | Lambda
      { args :: [LoweredExpr],
        expected_ret :: LoweredExpr,
        body :: LoweredExpr
      }
  deriving (Show, Eq)

type LowerStep1Context = Common.Context [LexerToken] LoweredExpr LoweredExpr

type LowerStep1 a = State LowerStep1Context a

createStairCase :: [LexerToken] -> LowerStep1Context
createStairCase a =
  Context
    { input = a,
      at = 0,
      results = [],
      blocks = [],
      sym_table = Map.empty,
      c_multi_item = Nothing,
      is_comment = False,
      at_block = 0
    }

peekAndCurrentInternal :: Int -> LowerStep1 (Maybe LexerToken)
peekAndCurrentInternal n = do
  ctx <- get
  if at ctx + n < length (input ctx)
    then return $ Just (input ctx !! (at ctx + n))
    else return Nothing

advance :: LowerStep1 ()
advance = modify (\ctx -> ctx {at = at ctx + 1})

peek :: LowerStep1 (Maybe LexerToken)
peek = peekAndCurrentInternal 1

current :: LowerStep1 (Maybe LexerToken)
current = do
  ctx <- get
  if at ctx < length (input ctx)
    then return $ Just (input ctx !! at ctx)
    else return Nothing

stepThroughLambda :: LowerStep1 ()
stepThroughLambda = do
  ctx <- get
  error "WHATEVER"

step :: LexerToken -> LowerStep1 [LoweredExpr]
step (Prime Type) = error "PRIME!"
step (Literal (Ident i)) = do
  peek >>= \case
    Just Backslash -> do
      advance
      stepThroughLambda >> stepAll
    _ -> error "Unkown token following identifier"
step Backslash = do
  advance
  stepThroughLambda >> stepAll

stepAll :: LowerStep1 [LoweredExpr]
stepAll = do
  ctx <- get
  current <- current
  case current of
    -- The fact we no longer push the results
    -- of what we just lexed means that the
    -- responsiblity of pushing new tokens
    -- back is up to the lex' fn, not lexAll
    -- this also goes for advancing.
    Just a -> step a >> stepAll
    Nothing -> return (reverse $ results ctx)
