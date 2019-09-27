{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Minicute.Data.GMachine.State
  ( GMachineState( .. )
  , _stateCode
  , _stateStack
  , _stateHeap
  , _stateGlobal
  , getNextInstruction

  , GMachineCode
  , initialCode
  , popInstructionFromCode

  , GMachineStack
  , emptyStack

  , GMachineHeap
  , emptyHeap
  , allocNode

  , GMachineGlobal
  , emptyGlobal
  , addSupercombinatorToGlobal
  , updateNodeInGlobal

  , GMachineNode( .. )
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
import Minicute.Data.GMachine.Address
import Minicute.Data.GMachine.Instruction
import Minicute.Data.GMachine.Node

import qualified Data.Map as Map
import qualified Minicute.Transpilers.GMachine as GMachine ( initialCode )

data GMachineState
  = GMachineState
    { stateCode :: GMachineCode
    , stateStack :: GMachineStack
    , stateHeap :: GMachineHeap
    , stateGlobal ::GMachineGlobal
    }
  deriving ( Generic
           , Typeable
           , Data
           , Lift
           , Eq
           , Ord
           )

newtype GMachineCode
  = GMachineCode [Instruction]
  deriving ( Generic
           , Typeable
           , Data
           , Lift
           , Eq
           , Ord
           )

newtype GMachineStack
  = GMachineStack [GMachineAddress]
  deriving ( Generic
           , Typeable
           , Data
           , Lift
           , Eq
           , Ord
           )

newtype GMachineHeap
  = GMachineHeap [GMachineNode]
  deriving ( Generic
           , Typeable
           , Data
           , Lift
           , Eq
           , Ord
           )

newtype GMachineGlobal
  = GMachineGlobal (Map.Map Identifier GMachineNode)
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

makeWrapped ''GMachineCode

initialCode :: GMachineCode
initialCode = GMachineCode GMachine.initialCode

popInstructionFromCode :: (MonadState s m, s ~ GMachineCode, MonadFail m) => m Instruction
popInstructionFromCode = do
  code <- use _Wrapped
  case uncons code of
    Just (instr, code') -> do
      _Wrapped .= code'
      return instr
    Nothing ->
      fail "no more instruction"

makeWrapped ''GMachineStack

emptyStack :: GMachineStack
emptyStack = GMachineStack []

makeWrapped ''GMachineHeap

emptyHeap :: GMachineHeap
emptyHeap = GMachineHeap []

allocNode :: (MonadState s m, s ~ GMachineHeap) => GMachineNode -> m GMachineAddress
allocNode = undefined

makeWrapped ''GMachineGlobal

emptyGlobal :: GMachineGlobal
emptyGlobal = GMachineGlobal Map.empty

addSupercombinatorToGlobal :: (MonadState s m, s ~ GMachineGlobal, MonadFail m) => GMachineSupercombinator -> m ()
addSupercombinatorToGlobal (ident, arity, code)
  = _Wrapped %= Map.insert ident (NGlobal (toInteger arity) code)

updateNodeInGlobal :: Identifier -> GMachineNode -> GMachineGlobal -> GMachineGlobal
updateNodeInGlobal ident node = _Wrapped %~ Map.insert ident node

makeLensesFor
  [ ("stateCode", "_stateCode")
  , ("stateStack", "_stateStack")
  , ("stateHeap", "_stateHeap")
  , ("stateGlobal", "_stateGlobal")
  ]
  ''GMachineState

getNextInstruction :: (MonadState s m, s ~ GMachineState, MonadFail m) => m Instruction
getNextInstruction = do
  code <- use _stateCode
  (instr, code') <- runStateT popInstructionFromCode code
  _stateCode .= code'
  return instr
