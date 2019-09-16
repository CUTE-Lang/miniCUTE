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

interpretInstruction (IPushBasicValue v) = interpretPushBasicValue v
interpretInstruction IPushExtractedValue = interpretPushExtractedValue
interpretInstruction IWrapAsInteger = interpretWrapAsInteger
interpretInstruction IWrapAsStructure = interpretWrapAsStructure
interpretInstruction (IUpdateAsInteger n) = interpretUpdateAsInteger n
interpretInstruction (IUpdateAsStructure n) = interpretUpdateAsStructure n

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
  pushAddrToAddrStack addr

interpretMakeGlobal :: Identifier -> InterpreterMonad ()
interpretMakeGlobal i = do
  addr <- findGlobalNode i
  pushAddrToAddrStack addr

interpretMakeApplication :: InterpreterMonad ()
interpretMakeApplication = do
  applyerAddr <- popAddrFromAddrStack
  applyeeAddr <- popAddrFromAddrStack
  addr <- allocNodeOnHeap (NApplication applyeeAddr applyerAddr)
  pushAddrToAddrStack addr

-- Please check the direction of addrs.
-- (Thankfully, it only affects performance, not correctness)
interpretMakePlaceholders :: Int -> InterpreterMonad ()
interpretMakePlaceholders n = do
  addrs <- allocPlaceholders
  -- To reverse, or not to reverse, that is the question.
  pushAddrsToAddrStack addrs
  where
    allocPlaceholders = replicateM n $ allocNodeOnHeap NEmpty


interpretPop :: Int -> InterpreterMonad ()
interpretPop n = void (popAddrsFromAddrStack n)

interpretDig :: Int -> InterpreterMonad ()
interpretDig n = do
  addr <- popAddrFromAddrStack
  _ <- popAddrsFromAddrStack n
  pushAddrToAddrStack addr

interpretUpdate :: Int -> InterpreterMonad ()
interpretUpdate n = do
  valueAddr <- peekAddrOnAddrStack
  targetAddr <- peekNthAddrOnAddrStack n
  updateNodeOnHeap targetAddr (NIndirect valueAddr)

interpretCopy :: Int -> InterpreterMonad ()
interpretCopy n = do
  addr <- peekNthAddrOnAddrStack n
  pushAddrToAddrStack addr


interpretPushBasicValue :: Integer -> InterpreterMonad ()
interpretPushBasicValue = pushValueToValueStack

interpretPushExtractedValue :: InterpreterMonad ()
interpretPushExtractedValue = do
  addr <- popAddrFromAddrStack
  node <- findNodeOnHeap addr
  case node of
    NInteger v -> pushValueToValueStack v
    NStructure v fieldsAddr -> do
      fieldsNode <- findNodeOnHeap fieldsAddr
      case fieldsNode of
        NStructureFields 0 _ -> pushValueToValueStack v
        _ -> invalidNodeError fieldsNode
    _ -> invalidNodeError node
  where
    invalidNodeError node =
      error
      ( "interpretPushExtractedValue: "
        <> show node
        <> " node cannot be used as a primitive value"
      )

interpretWrapAsInteger :: InterpreterMonad ()
interpretWrapAsInteger = do
  v <- popValueFromValueStack
  addr <- allocNodeOnHeap (NInteger v)
  pushAddrToAddrStack addr

interpretWrapAsStructure :: InterpreterMonad ()
interpretWrapAsStructure = do
  v <- popValueFromValueStack
  fieldsAddr <- allocNodeOnHeap (NStructureFields 0 [])
  addr <- allocNodeOnHeap (NStructure v fieldsAddr)
  pushAddrToAddrStack addr

interpretUpdateAsInteger :: Int -> InterpreterMonad ()
interpretUpdateAsInteger n = do
  targetAddr <- peekNthAddrOnAddrStack n
  v <- popValueFromValueStack
  updateNodeOnHeap targetAddr (NInteger v)

interpretUpdateAsStructure :: Int -> InterpreterMonad ()
interpretUpdateAsStructure n = do
  targetAddr <- peekNthAddrOnAddrStack n
  v <- popValueFromValueStack
  fieldsAddr <- allocNodeOnHeap (NStructureFields 0 [])
  updateNodeOnHeap targetAddr (NStructure v fieldsAddr)
