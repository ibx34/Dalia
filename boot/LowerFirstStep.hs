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

data LoweredExpr
  = Assignment
      { left :: LoweredExpr,
        right :: LoweredExpr
        -- todo: Maybe smth like what was used to assign??
      }
  | FunctionTy
      { name :: Maybe LoweredExpr,
        -- This isnt Maybe because a "function" assignment
        -- without any arguments is essentially just an
        -- assignment .......
        type_list :: [LoweredExpr],
        explicit_ret :: LoweredExpr
        -- any other options here
      }
  | Ident' Literals
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

advance :: LowerStep1 [LoweredExpr]
advance = modify (\ctx -> ctx {at = at ctx + 1}) >> stepAll

current :: LowerStep1 (Maybe LexerToken)
current = do
  ctx <- get
  if at ctx < length (input ctx)
    then return $ Just (input ctx !! at ctx)
    else return Nothing

step :: LexerToken -> LowerStep1 [LoweredExpr]
step (Prime Type) = error "PRIME!"
step (Literal (Ident i)) = error "IDENTIFIER!!"

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
