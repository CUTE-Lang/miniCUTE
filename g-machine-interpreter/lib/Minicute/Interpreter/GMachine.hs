{-# LANGUAGE NamedFieldPuns #-}
module Minicute.Interpreter.GMachine
  ( interpretProgram
  ) where

import Control.Monad
import Minicute.Data.Common
import Minicute.Data.GMachine.Instruction
import Minicute.Interpreter.GMachine.Monad
import Minicute.Interpreter.GMachine.Common

interpretProgram :: GMachineProgram -> InterpreterMonad ()
interpretProgram program
  = initializeInterpreterWith program >> go
  where
    go = fetchNextInstruction >>= interpretInstruction >> go

interpretInstruction :: Instruction -> InterpreterMonad ()

interpretInstruction (IMakeInteger n) = interpretMakeInteger n
interpretInstruction IMakeApplication = interpretMakeApplication
interpretInstruction (IMakeGlobal i) = interpretMakeGlobal i
interpretInstruction (IMakePlaceholders n) = interpretMakePlaceholders n

interpretInstruction (IPop n) = interpretPop n
interpretInstruction (IDig n) = interpretDig n
interpretInstruction (IUpdate n) = interpretUpdate n
interpretInstruction (ICopy n) = interpretCopy n

interpretInstruction inst
  = error
    ( "interpretInstruction: "
      <> show inst
      <> " case is not yet implemented"
    )

-- interpreter functions for each instruction
interpretMakeInteger :: Integer -> InterpreterMonad ()
interpretMakeInteger n = do
  addr <- allocNodeOnHeap (NInteger n)
  pushAddrToStack addr

interpretMakeGlobal :: Identifier -> InterpreterMonad ()
interpretMakeGlobal i = do
  addr <- findGlobalNode i
  pushAddrToStack addr

interpretMakeApplication :: InterpreterMonad ()
interpretMakeApplication = do
  applyerAddr <- popAddrFromStack
  applyeeAddr <- popAddrFromStack
  addr <- allocNodeOnHeap (NApplication applyeeAddr applyerAddr)
  pushAddrToStack addr

-- Please check the direction of addrs.
-- (Thankfully, it only affects performance, not correctness)
interpretMakePlaceholders :: Int -> InterpreterMonad ()
interpretMakePlaceholders n = do
  addrs <- allocPlaceholders
  -- To reverse, or not to reverse, that is the question.
  pushAddrsToStack addrs
  where
    allocPlaceholders = replicateM n $ allocNodeOnHeap NEmpty

interpretPop :: Int -> InterpreterMonad ()
interpretPop n = void (popAddrsFromStack n)

interpretDig :: Int -> InterpreterMonad ()
interpretDig n = do
  addr <- popAddrFromStack
  _ <- popAddrsFromStack n
  pushAddrToStack addr

interpretUpdate :: Int -> InterpreterMonad ()
interpretUpdate n = do
  valueAddr <- peekNthAddrOnStack 0
  targetAddr <- peekNthAddrOnStack n
  updateNodeOnHeap targetAddr (NIndirect valueAddr)

interpretCopy :: Int -> InterpreterMonad ()
interpretCopy n = do
  addr <- peekNthAddrOnStack n
  pushAddrToStack addr
