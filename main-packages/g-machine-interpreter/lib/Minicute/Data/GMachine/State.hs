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
  ( module Minicute.Data.GMachine.Node

  , GMachineState( .. )
  , _code
  , _stack
  , _heap
  , _global
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
    { code :: GMachineCode
    , stack :: GMachineStack
    , heap :: GMachineHeap
    , global ::GMachineGlobal
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
  instrs <- use _Wrapped
  case uncons instrs of
    Just (instr, instrs') -> do
      _Wrapped .= instrs'
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
addSupercombinatorToGlobal (ident, arity, c)
  = _Wrapped %= Map.insert ident (NGlobal (toInteger arity) c)

updateNodeInGlobal :: Identifier -> GMachineNode -> GMachineGlobal -> GMachineGlobal
updateNodeInGlobal ident node = _Wrapped %~ Map.insert ident node

makeLensesFor
  [ ("code", "_code")
  , ("stack", "_stack")
  , ("heap", "_heap")
  , ("global", "_global")
  ]
  ''GMachineState

getNextInstruction :: (MonadState s m, s ~ GMachineState, MonadFail m) => m Instruction
getNextInstruction = do
  c <- use _code
  (instr, c') <- runStateT popInstructionFromCode c
  _code .= c'
  return instr
