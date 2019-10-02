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

  , allocNodeOnNodeHeap
  , updateNodeOnNodeHeap
  , findNodeOnNodeHeap

  , allocAddressOnGlobal
  , updateAddressOnGlobal
  , findAddressOnGlobal

  , pushAddrToAddressStack
  , pushAddrsToAddressStack
  , popAddrFromAddressStack
  , popAddrsFromAddressStack
  , popAllAddrsFromAddressStack
  , peekAddrOnAddressStack
  , peekNthAddrOnAddressStack
  , checkAddressStackSize

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

import qualified Minicute.Data.GMachine.AddressStack as AddressStack
import qualified Minicute.Data.GMachine.Code as Code
import qualified Minicute.Data.GMachine.Global as Global
import qualified Minicute.Data.GMachine.NodeHeap as NodeHeap

data GMachineState
  = GMachineState
    { code :: Code.Code
    , addressStack :: AddressStack.AddressStack
    , nodeHeap :: NodeHeap.NodeHeap
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
  , ("addressStack", "_addressStack")
  , ("nodeHeap", "_nodeHeap")
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


allocNodeOnNodeHeap :: (MonadState s m, s ~ GMachineState) => Node -> m Address
allocNodeOnNodeHeap = applySubstructuralState _nodeHeap . NodeHeap.allocNode

updateNodeOnNodeHeap :: (MonadState s m, s ~ GMachineState, MonadFail m) => Address -> Node -> m ()
updateNodeOnNodeHeap = (applySubstructuralState _nodeHeap .) . NodeHeap.updateNode

findNodeOnNodeHeap :: (MonadState s m, s ~ GMachineState, MonadFail m) => Address -> m Node
findNodeOnNodeHeap = applySubstructuralState _nodeHeap . NodeHeap.findNode


allocAddressOnGlobal :: (MonadState s m, s ~ GMachineState) => Identifier -> Address -> m ()
allocAddressOnGlobal = (applySubstructuralState _global .) . Global.allocAddress

updateAddressOnGlobal :: (MonadState s m, s ~ GMachineState, MonadFail m) => Identifier -> Address -> m ()
updateAddressOnGlobal = (applySubstructuralState _global .) . Global.updateAddress

findAddressOnGlobal :: (MonadState s m, s ~ GMachineState, MonadFail m) => Identifier -> m Address
findAddressOnGlobal = applySubstructuralState _global . Global.findAddress


pushAddrToAddressStack :: (MonadState s m, s ~ GMachineState) => Address -> m ()
pushAddrToAddressStack = undefined

pushAddrsToAddressStack :: (MonadState s m, s ~ GMachineState) => [Address] -> m ()
pushAddrsToAddressStack = undefined

popAddrFromAddressStack :: (MonadState s m, s ~ GMachineState) => m Address
popAddrFromAddressStack = undefined

popAddrsFromAddressStack :: (MonadState s m, s ~ GMachineState) => Int -> m [Address]
popAddrsFromAddressStack = undefined

popAllAddrsFromAddressStack :: (MonadState s m, s ~ GMachineState) => m [Address]
popAllAddrsFromAddressStack = undefined

peekAddrOnAddressStack :: (MonadState s m, s ~ GMachineState) => m Address
peekAddrOnAddressStack = undefined

peekNthAddrOnAddressStack :: (MonadState s m, s ~ GMachineState) => Int -> m Address
peekNthAddrOnAddressStack = undefined

checkAddressStackSize :: (MonadState s m, s ~ GMachineState) => Int -> m Bool
checkAddressStackSize = undefined


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
