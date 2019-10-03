{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Minicute.Data.GMachine.State
  ( GMachineState
  , buildInitialState

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
  , checkSizeOfAddressStack

  , pushValueToValueStack
  , popValueFromValueStack

  , saveStateToDump
  , loadStateFromDump
  ) where

import Control.Lens.Getter ( use )
import Control.Lens.Iso ( iso )
import Control.Lens.Operators
import Control.Lens.TH
import Control.Lens.Type
import Control.Lens.Unsound
import Control.Monad.Fail ( MonadFail )
import Control.Monad.State
import Data.Data
import Data.Tuple.Minicute
import GHC.Generics
import Minicute.Data.Common
import Minicute.Data.GMachine.Address
import Minicute.Data.GMachine.Node

import qualified Minicute.Data.GMachine.AddressStack as AddressStack
import qualified Minicute.Data.GMachine.Code as Code
import qualified Minicute.Data.GMachine.Dump as Dump
import qualified Minicute.Data.GMachine.Global as Global
import qualified Minicute.Data.GMachine.NodeHeap as NodeHeap
import qualified Minicute.Data.GMachine.ValueStack as ValueStack

data GMachineState
  = GMachineState
    { code :: Code.Code
    , addressStack :: AddressStack.AddressStack
    , valueStack :: ValueStack.ValueStack
    , dump :: Dump.Dump
    , nodeHeap :: NodeHeap.NodeHeap
    , global :: Global.Global
    }
  deriving ( Generic
           , Typeable
           , Data
           , Eq
           , Ord
           , Show
           )

makeLensesFor
  [ ("code", "_code")
  , ("addressStack", "_addressStack")
  , ("valueStack", "_valueStack")
  , ("dump", "_dump")
  , ("nodeHeap", "_nodeHeap")
  , ("global", "_global")
  ]
  ''GMachineState

buildInitialState :: Code.GMachineProgram -> GMachineState
buildInitialState program
  = GMachineState
    { code = Code.initialCode
    , addressStack = AddressStack.emptyAddressStack
    , valueStack = ValueStack.emptyValueStack
    , dump = Dump.emptyDump
    , nodeHeap = initialNodeHeap
    , global = initialGlobal
    }
  where
    (globalEntries, initialNodeHeap)
      = runState buildGlobalEntriesAndHeap NodeHeap.emptyNodeHeap
    initialGlobal
      = execState buildGlobal Global.emptyGlobal

    buildGlobalEntriesAndHeap
      = forM program
        $ \(ident, arity, c) ->
            (,) ident <$> NodeHeap.allocNode (NGlobal (toInteger arity) c)
    buildGlobal
      = forM globalEntries
        $ uncurry Global.allocAddress


fetchNextInstruction :: (MonadState s m, s ~ GMachineState, MonadFail m) => m Code.Instruction
fetchNextInstruction = applySubstructuralAction _code Code.fetchNextInstruction

putInstruction :: (MonadState s m, s ~ GMachineState) => Code.Instruction -> m ()
putInstruction = applySubstructuralAction _code . Code.putInstruction

putInstructions :: (MonadState s m, s ~ GMachineState) => [Code.Instruction] -> m ()
putInstructions = applySubstructuralAction _code . Code.putInstructions

assertLastCode :: (MonadState s m, s ~ GMachineState, MonadFail m) => m ()
assertLastCode = applySubstructuralAction _code Code.assertLastCode


allocNodeOnNodeHeap :: (MonadState s m, s ~ GMachineState) => Node -> m Address
allocNodeOnNodeHeap = applySubstructuralAction _nodeHeap . NodeHeap.allocNode

updateNodeOnNodeHeap :: (MonadState s m, s ~ GMachineState, MonadFail m) => Address -> Node -> m ()
updateNodeOnNodeHeap = (applySubstructuralAction _nodeHeap .) . NodeHeap.updateNode

findNodeOnNodeHeap :: (MonadState s m, s ~ GMachineState, MonadFail m) => Address -> m Node
findNodeOnNodeHeap = applySubstructuralAction _nodeHeap . NodeHeap.findNode


allocAddressOnGlobal :: (MonadState s m, s ~ GMachineState) => Identifier -> Address -> m ()
allocAddressOnGlobal = (applySubstructuralAction _global .) . Global.allocAddress

updateAddressOnGlobal :: (MonadState s m, s ~ GMachineState, MonadFail m) => Identifier -> Address -> m ()
updateAddressOnGlobal = (applySubstructuralAction _global .) . Global.updateAddress

findAddressOnGlobal :: (MonadState s m, s ~ GMachineState, MonadFail m) => Identifier -> m Address
findAddressOnGlobal = applySubstructuralAction _global . Global.findAddress


pushAddrToAddressStack :: (MonadState s m, s ~ GMachineState) => Address -> m ()
pushAddrToAddressStack = applySubstructuralAction _addressStack . AddressStack.pushAddr

pushAddrsToAddressStack :: (MonadState s m, s ~ GMachineState) => [Address] -> m ()
pushAddrsToAddressStack = applySubstructuralAction _addressStack . AddressStack.pushAddrs

popAddrFromAddressStack :: (MonadState s m, s ~ GMachineState, MonadFail m) => m Address
popAddrFromAddressStack = applySubstructuralAction _addressStack AddressStack.popAddr

popAddrsFromAddressStack :: (MonadState s m, s ~ GMachineState, MonadFail m) => Int -> m [Address]
popAddrsFromAddressStack = applySubstructuralAction _addressStack . AddressStack.popAddrs

popAllAddrsFromAddressStack :: (MonadState s m, s ~ GMachineState) => m [Address]
popAllAddrsFromAddressStack = applySubstructuralAction _addressStack AddressStack.popAllAddrs

peekAddrOnAddressStack :: (MonadState s m, s ~ GMachineState) => m Address
peekAddrOnAddressStack = applySubstructuralAction _addressStack AddressStack.peekAddr

peekNthAddrOnAddressStack :: (MonadState s m, s ~ GMachineState) => Int -> m Address
peekNthAddrOnAddressStack = applySubstructuralAction _addressStack . AddressStack.peekNthAddr

checkSizeOfAddressStack :: (MonadState s m, s ~ GMachineState) => Int -> m Bool
checkSizeOfAddressStack = applySubstructuralAction _addressStack . AddressStack.checkSize


pushValueToValueStack :: (MonadState s m, s ~ GMachineState) => Integer -> m ()
pushValueToValueStack = applySubstructuralAction _valueStack . ValueStack.pushValue

popValueFromValueStack :: (MonadState s m, s ~ GMachineState, MonadFail m) => m Integer
popValueFromValueStack = applySubstructuralAction _valueStack ValueStack.popValue


saveStateToDump :: (MonadState s m, s ~ GMachineState) => m ()
saveStateToDump = _di <<.= Dump.emptyDumpItem >>= applySubstructuralAction _dump . Dump.saveState

loadStateFromDump :: (MonadState s m, s ~ GMachineState, MonadFail m) => m ()
loadStateFromDump = _di <~ applySubstructuralAction _dump Dump.loadState


_di :: Lens' GMachineState Dump.DumpItem
_di = lensProduct _code (lensProduct _addressStack _valueStack) . iso tupleUnzip2 tupleZip2

applySubstructuralAction :: (MonadState s m, s ~ GMachineState) => Lens' s a -> StateT a m r -> m r
applySubstructuralAction _l action = do
  substructure <- use _l
  (result, substructure') <- runStateT action substructure
  _l .= substructure'
  return result
