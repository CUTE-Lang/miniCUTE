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


allocNodeOnHeap :: (MonadFail m) => Node -> GMachineStepMonadT m Address
allocNodeOnHeap = undefined

updateNodeOnHeap :: Address -> Node -> GMachineStepMonadT m ()
updateNodeOnHeap = undefined

findNodeOnHeap :: Address -> GMachineStepMonadT m Node
findNodeOnHeap = undefined


findGlobalNode :: Identifier -> GMachineStepMonadT m Address
findGlobalNode = undefined


pushAddrToAddrStack :: Address -> GMachineStepMonadT m ()
pushAddrToAddrStack = undefined

pushAddrsToAddrStack :: [Address] -> GMachineStepMonadT m ()
pushAddrsToAddrStack = undefined

popAddrFromAddrStack :: GMachineStepMonadT m Address
popAddrFromAddrStack = undefined

popAddrsFromAddrStack :: Int -> GMachineStepMonadT m [Address]
popAddrsFromAddrStack = undefined

popAllAddrsFromAddrStack :: GMachineStepMonadT m [Address]
popAllAddrsFromAddrStack = undefined

peekAddrOnAddrStack :: GMachineStepMonadT m Address
peekAddrOnAddrStack = undefined

peekNthAddrOnAddrStack :: Int -> GMachineStepMonadT m Address
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
