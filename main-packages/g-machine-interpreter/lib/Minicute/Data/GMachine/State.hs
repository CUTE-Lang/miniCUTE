{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Minicute.Data.GMachine.State
  ( GMachineState

  , fetchNextInstruction
  , putInstruction
  , putInstructions
  , assertLastCode

  , allocNodeOnHeap
  , updateNodeOnHeap
  , findNodeOnHeap

  , findGlobalNode

  , pushAddrToAddrStack
  , pushAddrsToAddrStack
  , popAddrFromAddrStack
  , popAddrsFromAddrStack
  , popAllAddrsFromAddrStack
  , peekAddrOnAddrStack
  , peekNthAddrOnAddrStack
  , checkAddrStackSize

  , pushValueToValueStack
  , popValueFromValueStack

  , saveStateToDump
  , loadStateFromDump
  ) where

import Control.Lens.Getter ( use )
import Control.Lens.Operators
import Control.Lens.TH
import Control.Lens.Type
import Control.Monad.Fail ( MonadFail )
import Control.Monad.State
import Data.Data
import GHC.Generics
import Minicute.Data.Common
import Minicute.Data.GMachine.Address
import Minicute.Data.GMachine.Node

import qualified Minicute.Data.GMachine.Code as Code
import qualified Minicute.Data.GMachine.Global as Global
import qualified Minicute.Data.GMachine.Heap as Heap
import qualified Minicute.Data.GMachine.Stack as Stack

data GMachineState
  = GMachineState
    { code :: Code.Code
    , stack :: Stack.Stack
    , heap :: Heap.Heap
    , global :: Global.Global
    }
  deriving ( Generic
           , Typeable
           , Data
           , Eq
           , Ord
           )

makeLensesFor
  [ ("code", "_code")
  , ("stack", "_stack")
  , ("heap", "_heap")
  , ("global", "_global")
  ]
  ''GMachineState


fetchNextInstruction :: (MonadState s m, s ~ GMachineState, MonadFail m) => m Code.Instruction
fetchNextInstruction = applySubstructuralState _code Code.fetchNextInstruction

putInstruction :: (MonadState s m, s ~ GMachineState) => Code.Instruction -> m ()
putInstruction = applySubstructuralState _code . Code.putInstruction

putInstructions :: (MonadState s m, s ~ GMachineState) => [Code.Instruction] -> m ()
putInstructions = applySubstructuralState _code . Code.putInstructions

assertLastCode :: (MonadState s m, s ~ GMachineState, MonadFail m) => m ()
assertLastCode = applySubstructuralState _code Code.assertLastCode


allocNodeOnHeap :: (MonadState s m, s ~ GMachineState, MonadFail m) => Node -> m Address
allocNodeOnHeap = undefined

updateNodeOnHeap :: (MonadState s m, s ~ GMachineState) => Address -> Node -> m ()
updateNodeOnHeap = undefined

findNodeOnHeap :: (MonadState s m, s ~ GMachineState) => Address -> m Node
findNodeOnHeap = undefined


findGlobalNode :: (MonadState s m, s ~ GMachineState) => Identifier -> m Address
findGlobalNode = undefined


pushAddrToAddrStack :: (MonadState s m, s ~ GMachineState) => Address -> m ()
pushAddrToAddrStack = undefined

pushAddrsToAddrStack :: (MonadState s m, s ~ GMachineState) => [Address] -> m ()
pushAddrsToAddrStack = undefined

popAddrFromAddrStack :: (MonadState s m, s ~ GMachineState) => m Address
popAddrFromAddrStack = undefined

popAddrsFromAddrStack :: (MonadState s m, s ~ GMachineState) => Int -> m [Address]
popAddrsFromAddrStack = undefined

popAllAddrsFromAddrStack :: (MonadState s m, s ~ GMachineState) => m [Address]
popAllAddrsFromAddrStack = undefined

peekAddrOnAddrStack :: (MonadState s m, s ~ GMachineState) => m Address
peekAddrOnAddrStack = undefined

peekNthAddrOnAddrStack :: (MonadState s m, s ~ GMachineState) => Int -> m Address
peekNthAddrOnAddrStack = undefined

checkAddrStackSize :: (MonadState s m, s ~ GMachineState) => Int -> m Bool
checkAddrStackSize = undefined


pushValueToValueStack :: (MonadState s m, s ~ GMachineState) => Integer -> m ()
pushValueToValueStack = undefined

popValueFromValueStack :: (MonadState s m, s ~ GMachineState) => m Integer
popValueFromValueStack = undefined


saveStateToDump :: (MonadState s m, s ~ GMachineState) => m ()
saveStateToDump = undefined

loadStateFromDump :: (MonadState s m, s ~ GMachineState) => m ()
loadStateFromDump = undefined


applySubstructuralState :: (MonadState s m, s ~ GMachineState) => Lens' s a -> StateT a m r -> m r
applySubstructuralState _l action = do
  substructure <- use _l
  (result, substructure') <- runStateT action substructure
  _l .= substructure'
  return result
