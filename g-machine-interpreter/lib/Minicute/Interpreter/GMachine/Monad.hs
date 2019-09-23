{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Minicute.Interpreter.GMachine.Monad
  ( InterpreterMonadT
  , InterpreterMonad

  , initializeInterpreterWith
  , addInterpreterStep

  , InterpreterStepMonadT
  , InterpreterStepMonad

  , fetchNextInstruction
  , putInstruction
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

  , pushValueToValueStack
  , popValueFromValueStack

  , saveStateToDump
  , loadStateFromDump
  ) where

import Control.Monad.Fail
import Data.Data
import GHC.Generics
import Language.Haskell.TH.Syntax
import Minicute.Data.Common
import Minicute.Data.GMachine.Instruction
import Minicute.Interpreter.GMachine.Common

type InterpreterMonad = InterpreterMonadT Maybe

newtype InterpreterMonadT m a = InterpreterMonadT (m a)
  deriving ( Generic
           , Typeable
           , Data
           , Lift
           , Eq
           , Ord
           , Functor
           , Applicative
           , Monad
           , MonadFail
           )


initializeInterpreterWith :: GMachineProgram -> InterpreterMonadT m ()
initializeInterpreterWith = undefined

addInterpreterStep :: InterpreterStepMonadT m a -> InterpreterMonadT m a
addInterpreterStep = undefined


type InterpreterStepMonad = InterpreterStepMonadT Maybe

newtype InterpreterStepMonadT m a = InterpreterStepMonadT (m a)
  deriving ( Generic
           , Typeable
           , Data
           , Lift
           , Eq
           , Ord
           , Functor
           , Applicative
           , Monad
           , MonadFail
           )


fetchNextInstruction :: (MonadFail m) => InterpreterStepMonadT m Instruction
fetchNextInstruction = undefined

putInstruction :: Instruction -> InterpreterStepMonadT m ()
putInstruction = undefined

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


pushValueToValueStack :: Integer -> InterpreterStepMonadT m ()
pushValueToValueStack = undefined

popValueFromValueStack :: InterpreterStepMonadT m Integer
popValueFromValueStack = undefined


saveStateToDump :: InterpreterStepMonadT m ()
saveStateToDump = undefined

loadStateFromDump :: InterpreterStepMonadT m ()
loadStateFromDump = undefined
