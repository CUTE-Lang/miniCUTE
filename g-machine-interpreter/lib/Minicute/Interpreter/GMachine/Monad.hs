{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Minicute.Interpreter.GMachine.Monad
  ( InterpreterMonadT
  , InterpreterMonad

  , initializeInterpreterWith

  , fetchNextInstruction

  , allocNodeOnHeap
  , updateNodeOnHeap

  , findGlobalNode

  , pushAddrToStack
  , pushAddrsToStack
  , popAddrFromStack
  , popAddrsFromStack
  , peekNthAddrOnStack
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

allocNodeOnHeap :: (MonadFail m) => InterpreterNode -> InterpreterMonadT m InterpreterAddress
allocNodeOnHeap = undefined

updateNodeOnHeap :: InterpreterAddress -> InterpreterNode -> InterpreterMonadT m ()
updateNodeOnHeap = undefined

findGlobalNode :: Identifier -> InterpreterMonadT m InterpreterAddress
findGlobalNode = undefined

pushAddrToStack :: InterpreterAddress -> InterpreterMonadT m ()
pushAddrToStack = undefined

pushAddrsToStack :: [InterpreterAddress] -> InterpreterMonadT m ()
pushAddrsToStack = undefined

popAddrFromStack :: InterpreterMonadT m InterpreterAddress
popAddrFromStack = undefined

popAddrsFromStack :: Int -> InterpreterMonadT m InterpreterAddress
popAddrsFromStack = undefined

peekNthAddrOnStack :: Int -> InterpreterMonadT m InterpreterAddress
peekNthAddrOnStack = undefined
