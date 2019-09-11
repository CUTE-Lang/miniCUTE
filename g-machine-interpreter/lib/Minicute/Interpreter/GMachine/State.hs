{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Minicute.Interpreter.GMachine.State
  ( InterpreterState( .. )
  , _stateCode
  , _stateStack
  , _stateHeap
  , _stateGlobal
  , getNextInstruction

  , InterpreterCode
  , initialCode
  , popInstructionFromCode

  , InterpreterStack
  , emptyStack

  , InterpreterHeap
  , emptyHeap
  , allocNode

  , InterpreterGlobal
  , emptyGlobal
  , addSupercombinatorToGlobal
  , updateNodeInGlobal

  , InterpreterNode( .. )
  ) where

import Prelude hiding ( fail )

import Control.Lens.Operators
import Control.Lens.TH
import Control.Lens.Wrapped ( _Wrapped )
import Control.Monad.Fail
import Control.Monad.Writer ( WriterT( WriterT ), runWriterT )
import Data.Data
import Data.List
import Data.Tuple
import GHC.Generics
import Language.Haskell.TH.Syntax
import Minicute.Data.Common
import Minicute.Data.GMachine.Instruction

import qualified Data.Map as Map
import qualified Minicute.Transpilers.GMachine as GMachine ( initialCode )

data InterpreterState
  = InterpreterState
    { stateCode :: InterpreterCode
    , stateStack :: InterpreterStack
    , stateHeap :: InterpreterHeap
    , stateGlobal ::InterpreterGlobal
    }
  deriving ( Generic
           , Typeable
           , Data
           , Lift
           , Eq
           , Ord
           )

newtype InterpreterCode
  = InterpreterCode [Instruction]
  deriving ( Generic
           , Typeable
           , Data
           , Lift
           , Eq
           , Ord
           )

newtype InterpreterStack
  = InterpreterStack [InterpreterAddress]
  deriving ( Generic
           , Typeable
           , Data
           , Lift
           , Eq
           , Ord
           )

newtype InterpreterHeap
  = InterpreterHeap [InterpreterNode]
  deriving ( Generic
           , Typeable
           , Data
           , Lift
           , Eq
           , Ord
           )

newtype InterpreterGlobal
  = InterpreterGlobal (Map.Map Identifier InterpreterNode)
  deriving ( Generic
           , Typeable
           , Data
           , Lift
           , Eq
           , Ord
           )

data InterpreterNode
  = NInteger Integer
  | NApplication InterpreterAddress InterpreterAddress
  | NGlobal Integer GMachineExpression
  deriving ( Generic
           , Typeable
           , Data
           , Lift
           , Eq
           , Ord
           )

newtype InterpreterAddress
  = InterpreterAddress Integer
  deriving ( Generic
           , Typeable
           , Data
           , Lift
           , Eq
           , Ord
           , Num
           )

instance (Lift a, Lift b) => Lift (Map.Map a b) where
  lift m = [| Map.fromList m' |]
    where
      m' = Map.toList m

makeWrapped ''InterpreterCode

initialCode :: InterpreterCode
initialCode = InterpreterCode GMachine.initialCode

popInstructionFromCode :: (MonadFail m) => InterpreterCode -> m (Instruction, InterpreterCode)
popInstructionFromCode
  = fmap swap . runWriterT
    . ( _Wrapped
        %%~ WriterT . maybe (fail "no more instruction") (pure . swap) . uncons
      )

makeWrapped ''InterpreterStack

emptyStack :: InterpreterStack
emptyStack = InterpreterStack []

makeWrapped ''InterpreterHeap

emptyHeap :: InterpreterHeap
emptyHeap = InterpreterHeap []

allocNode :: InterpreterNode -> InterpreterHeap -> m (InterpreterAddress, InterpreterHeap)
allocNode = undefined

makeWrapped ''InterpreterGlobal

emptyGlobal :: InterpreterGlobal
emptyGlobal = InterpreterGlobal Map.empty

addSupercombinatorToGlobal :: (Applicative m) => GMachineSupercombinator -> InterpreterGlobal -> m InterpreterGlobal
addSupercombinatorToGlobal (ident, arity, code)
  = _Wrapped %%~ pure . Map.insert ident (NGlobal (toInteger arity) code)

updateNodeInGlobal :: Identifier -> InterpreterNode -> InterpreterGlobal -> InterpreterGlobal
updateNodeInGlobal ident node = _Wrapped %~ Map.insert ident node

makeLensesFor
  [ ("stateCode", "_stateCode")
  , ("stateStack", "_stateStack")
  , ("stateHeap", "_stateHeap")
  , ("stateGlobal", "_stateGlobal")
  ]
  ''InterpreterState

getNextInstruction :: (MonadFail m) => InterpreterState -> m (Instruction, InterpreterState)
getNextInstruction
  = fmap swap . runWriterT
    . ( _stateCode
        %%~ WriterT . fmap swap . popInstructionFromCode
      )
