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

import Control.Lens.Getter ( use )
import Control.Lens.Operators
import Control.Lens.TH
import Control.Lens.Wrapped ( _Wrapped )
import Control.Monad.Fail ( MonadFail )
import Control.Monad.State
import Data.Data
import Data.List
import GHC.Generics
import Language.Haskell.TH.Syntax
import Minicute.Data.Common
import Minicute.Data.GMachine.Instruction
import Minicute.Interpreter.GMachine.Common

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

instance (Lift a, Lift b) => Lift (Map.Map a b) where
  lift m = [| Map.fromList m' |]
    where
      m' = Map.toList m

makeWrapped ''InterpreterCode

initialCode :: InterpreterCode
initialCode = InterpreterCode GMachine.initialCode

popInstructionFromCode :: (MonadState s m, s ~ InterpreterCode, MonadFail m) => m Instruction
popInstructionFromCode = do
  code <- use _Wrapped
  case uncons code of
    Just (instr, code') -> do
      _Wrapped .= code'
      return instr
    Nothing ->
      fail "no more instruction"

makeWrapped ''InterpreterStack

emptyStack :: InterpreterStack
emptyStack = InterpreterStack []

makeWrapped ''InterpreterHeap

emptyHeap :: InterpreterHeap
emptyHeap = InterpreterHeap []

allocNode :: (MonadState s m, s ~ InterpreterHeap) => InterpreterNode -> m InterpreterAddress
allocNode = undefined

makeWrapped ''InterpreterGlobal

emptyGlobal :: InterpreterGlobal
emptyGlobal = InterpreterGlobal Map.empty

addSupercombinatorToGlobal :: (MonadState s m, s ~ InterpreterGlobal, MonadFail m) => GMachineSupercombinator -> m ()
addSupercombinatorToGlobal (ident, arity, code)
  = _Wrapped %= Map.insert ident (NGlobal (toInteger arity) code)

updateNodeInGlobal :: Identifier -> InterpreterNode -> InterpreterGlobal -> InterpreterGlobal
updateNodeInGlobal ident node = _Wrapped %~ Map.insert ident node

makeLensesFor
  [ ("stateCode", "_stateCode")
  , ("stateStack", "_stateStack")
  , ("stateHeap", "_stateHeap")
  , ("stateGlobal", "_stateGlobal")
  ]
  ''InterpreterState

getNextInstruction :: (MonadState s m, s ~ InterpreterState, MonadFail m) => m Instruction
getNextInstruction = do
  code <- use _stateCode
  (instr, code') <- runStateT popInstructionFromCode code
  _stateCode .= code'
  return instr
