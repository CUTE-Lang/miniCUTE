{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Minicute.Interpreter.GMachine.Monad
  ( InterpreterMonadT
  , InterpreterMonad

  , initializeInterpreterWith

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

newtype InterpreterMonadT m a = InterpreterM (m a)
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


fetchNextInstruction :: (MonadFail m) => InterpreterMonadT m Instruction
fetchNextInstruction = undefined

putInstruction :: Instruction -> InterpreterMonadT m ()
putInstruction = undefined

assertLastCode :: (MonadFail m) => InterpreterMonadT m ()
assertLastCode = undefined


allocNodeOnHeap :: (MonadFail m) => InterpreterNode -> InterpreterMonadT m InterpreterAddress
allocNodeOnHeap = undefined

updateNodeOnHeap :: InterpreterAddress -> InterpreterNode -> InterpreterMonadT m ()
updateNodeOnHeap = undefined

findNodeOnHeap :: InterpreterAddress -> InterpreterMonadT m InterpreterNode
findNodeOnHeap = undefined


findGlobalNode :: Identifier -> InterpreterMonadT m InterpreterAddress
findGlobalNode = undefined


pushAddrToAddrStack :: InterpreterAddress -> InterpreterMonadT m ()
pushAddrToAddrStack = undefined

pushAddrsToAddrStack :: [InterpreterAddress] -> InterpreterMonadT m ()
pushAddrsToAddrStack = undefined

popAddrFromAddrStack :: InterpreterMonadT m InterpreterAddress
popAddrFromAddrStack = undefined

popAddrsFromAddrStack :: Int -> InterpreterMonadT m [InterpreterAddress]
popAddrsFromAddrStack = undefined

popAllAddrsFromAddrStack :: InterpreterMonadT m [InterpreterAddress]
popAllAddrsFromAddrStack = undefined

peekAddrOnAddrStack :: InterpreterMonadT m InterpreterAddress
peekAddrOnAddrStack = undefined

peekNthAddrOnAddrStack :: Int -> InterpreterMonadT m InterpreterAddress
peekNthAddrOnAddrStack = undefined


pushValueToValueStack :: Integer -> InterpreterMonadT m ()
pushValueToValueStack = undefined

popValueFromValueStack :: InterpreterMonadT m Integer
popValueFromValueStack = undefined


saveStateToDump :: InterpreterMonadT m ()
saveStateToDump = undefined

loadStateFromDump :: InterpreterMonadT m ()
loadStateFromDump = undefined
