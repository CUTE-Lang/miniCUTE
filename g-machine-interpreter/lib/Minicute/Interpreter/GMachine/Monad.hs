{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
module Minicute.Interpreter.GMachine.Monad
  ( InterpreterMonadT
  , InterpreterMonad

  , initializeInterpreterWith
  , addInterpreterStep

  , InterpreterStepMonadT
  , InterpreterStepMonad

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
import Control.Monad.Writer
import Data.Data
import GHC.Generics
import Minicute.Data.Common
import Minicute.Data.GMachine.Instruction
import Minicute.Interpreter.GMachine.Common
import Minicute.Interpreter.GMachine.State

type InterpreterMonad = InterpreterMonadT Maybe Maybe

newtype InterpreterMonadT m' m a
  = InterpreterMonadT
    (WriterT [InterpreterStepMonadT m' ()] m a)
  deriving ( Generic
           , Typeable
           , Functor
           , Applicative
           , Monad
           , MonadFail
           )

deriving instance (Monad m) => MonadWriter [InterpreterStepMonadT m' ()] (InterpreterMonadT m' m)

initializeInterpreterWith :: (Monad m) => GMachineProgram -> InterpreterMonadT m' m ()
initializeInterpreterWith = pure . const ()

addInterpreterStep :: (Monad m) => InterpreterStepMonadT m' () -> InterpreterMonadT m' m ()
addInterpreterStep = tell . pure


type InterpreterStepMonad = InterpreterStepMonadT Maybe

newtype InterpreterStepMonadT m a = InterpreterStepMonadT (StateT InterpreterState m a)
  deriving ( Generic
           , Typeable
           , Functor
           , Applicative
           , Monad
           , MonadFail
           )

deriving instance (Monad m) => MonadState InterpreterState (InterpreterStepMonadT m)


fetchNextInstruction :: (MonadFail m) => InterpreterStepMonadT m Instruction
fetchNextInstruction = undefined

putInstruction :: Instruction -> InterpreterStepMonadT m ()
putInstruction = undefined

putInstructions :: [Instruction] -> InterpreterStepMonadT m ()
putInstructions = undefined

assertLastCode :: (MonadFail m) => InterpreterStepMonadT m ()
assertLastCode = undefined


allocNodeOnHeap :: (MonadFail m) => InterpreterNode -> InterpreterStepMonadT m InterpreterAddress
allocNodeOnHeap = undefined

updateNodeOnHeap :: InterpreterAddress -> InterpreterNode -> InterpreterStepMonadT m ()
updateNodeOnHeap = undefined

findNodeOnHeap :: InterpreterAddress -> InterpreterStepMonadT m InterpreterNode
findNodeOnHeap = undefined


findGlobalNode :: Identifier -> InterpreterStepMonadT m InterpreterAddress
findGlobalNode = undefined


pushAddrToAddrStack :: InterpreterAddress -> InterpreterStepMonadT m ()
pushAddrToAddrStack = undefined

pushAddrsToAddrStack :: [InterpreterAddress] -> InterpreterStepMonadT m ()
pushAddrsToAddrStack = undefined

popAddrFromAddrStack :: InterpreterStepMonadT m InterpreterAddress
popAddrFromAddrStack = undefined

popAddrsFromAddrStack :: Int -> InterpreterStepMonadT m [InterpreterAddress]
popAddrsFromAddrStack = undefined

popAllAddrsFromAddrStack :: InterpreterStepMonadT m [InterpreterAddress]
popAllAddrsFromAddrStack = undefined

peekAddrOnAddrStack :: InterpreterStepMonadT m InterpreterAddress
peekAddrOnAddrStack = undefined

peekNthAddrOnAddrStack :: Int -> InterpreterStepMonadT m InterpreterAddress
peekNthAddrOnAddrStack = undefined

checkAddrStackSize :: Int -> InterpreterStepMonadT m Bool
checkAddrStackSize = undefined


pushValueToValueStack :: Integer -> InterpreterStepMonadT m ()
pushValueToValueStack = undefined

popValueFromValueStack :: InterpreterStepMonadT m Integer
popValueFromValueStack = undefined


saveStateToDump :: InterpreterStepMonadT m ()
saveStateToDump = undefined

loadStateFromDump :: InterpreterStepMonadT m ()
loadStateFromDump = undefined
