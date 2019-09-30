{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
module Minicute.Control.GMachine.Step
  ( GMachineStepMonadT
  , GMachineStepMonad

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


import Control.Monad.Fail
import Control.Monad.State
import Data.Data
import GHC.Generics
import Minicute.Data.Common
import Minicute.Data.GMachine.Address
import Minicute.Data.GMachine.Instruction
import Minicute.Data.GMachine.Node
import Minicute.Data.GMachine.State ( GMachineState )

type GMachineStepMonad = GMachineStepMonadT Maybe

newtype GMachineStepMonadT m a = GMachineStepMonadT (StateT GMachineState m a)
  deriving ( Generic
           , Typeable
           , Functor
           , Applicative
           , Monad
           , MonadFail
           )

deriving instance (Monad m) => MonadState GMachineState (GMachineStepMonadT m)


fetchNextInstruction :: (MonadFail m) => GMachineStepMonadT m Instruction
fetchNextInstruction = undefined

putInstruction :: Instruction -> GMachineStepMonadT m ()
putInstruction = undefined

putInstructions :: [Instruction] -> GMachineStepMonadT m ()
putInstructions = undefined

assertLastCode :: (MonadFail m) => GMachineStepMonadT m ()
assertLastCode = undefined


allocNodeOnHeap :: (MonadFail m) => GMachineNode -> GMachineStepMonadT m GMachineAddress
allocNodeOnHeap = undefined

updateNodeOnHeap :: GMachineAddress -> GMachineNode -> GMachineStepMonadT m ()
updateNodeOnHeap = undefined

findNodeOnHeap :: GMachineAddress -> GMachineStepMonadT m GMachineNode
findNodeOnHeap = undefined


findGlobalNode :: Identifier -> GMachineStepMonadT m GMachineAddress
findGlobalNode = undefined


pushAddrToAddrStack :: GMachineAddress -> GMachineStepMonadT m ()
pushAddrToAddrStack = undefined

pushAddrsToAddrStack :: [GMachineAddress] -> GMachineStepMonadT m ()
pushAddrsToAddrStack = undefined

popAddrFromAddrStack :: GMachineStepMonadT m GMachineAddress
popAddrFromAddrStack = undefined

popAddrsFromAddrStack :: Int -> GMachineStepMonadT m [GMachineAddress]
popAddrsFromAddrStack = undefined

popAllAddrsFromAddrStack :: GMachineStepMonadT m [GMachineAddress]
popAllAddrsFromAddrStack = undefined

peekAddrOnAddrStack :: GMachineStepMonadT m GMachineAddress
peekAddrOnAddrStack = undefined

peekNthAddrOnAddrStack :: Int -> GMachineStepMonadT m GMachineAddress
peekNthAddrOnAddrStack = undefined

checkAddrStackSize :: Int -> GMachineStepMonadT m Bool
checkAddrStackSize = undefined


pushValueToValueStack :: Integer -> GMachineStepMonadT m ()
pushValueToValueStack = undefined

popValueFromValueStack :: GMachineStepMonadT m Integer
popValueFromValueStack = undefined


saveStateToDump :: GMachineStepMonadT m ()
saveStateToDump = undefined

loadStateFromDump :: GMachineStepMonadT m ()
loadStateFromDump = undefined
