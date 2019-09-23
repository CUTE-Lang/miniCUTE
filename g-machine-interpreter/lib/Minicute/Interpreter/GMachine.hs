{-# LANGUAGE NamedFieldPuns #-}
module Minicute.Interpreter.GMachine
  ( module Minicute.Interpreter.GMachine.Common
  , module Minicute.Interpreter.GMachine.Monad

  , interpretProgram
  ) where

import Control.Monad.Extra
import Data.List.Extra
import Minicute.Data.Common
import Minicute.Data.GMachine.Instruction
import Minicute.Interpreter.GMachine.Common
import Minicute.Interpreter.GMachine.Monad

interpretProgram :: GMachineProgram -> InterpreterMonad ()
interpretProgram program
  = initializeInterpreterWith program >> go
  where
    go = addInterpreterStep step >> go
    step = fetchNextInstruction >>= interpretInstruction

interpretInstruction :: Instruction -> InterpreterStepMonad ()

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

interpretInstruction (IPrimitive op) = interpretPrimitive op

interpretInstruction IUnwind = interpretUnwind

interpretInstruction IEval = interpretEval
interpretInstruction IReturn = interpretReturn

interpretInstruction inst
  = fail
    ( "interpretInstruction: "
      <> show inst
      <> " case is not yet implemented"
    )

-- interpreter functions for each instruction
interpretMakeInteger :: Integer -> InterpreterStepMonad ()
interpretMakeInteger n = do
  addr <- allocNodeOnHeap (NInteger n)
  pushAddrToAddrStack addr

interpretMakeGlobal :: Identifier -> InterpreterStepMonad ()
interpretMakeGlobal i = do
  addr <- findGlobalNode i
  pushAddrToAddrStack addr

interpretMakeApplication :: InterpreterStepMonad ()
interpretMakeApplication = do
  applyerAddr <- popAddrFromAddrStack
  applyeeAddr <- popAddrFromAddrStack
  addr <- allocNodeOnHeap (NApplication applyeeAddr applyerAddr)
  pushAddrToAddrStack addr

-- Please check the direction of addrs.
-- (Thankfully, it only affects performance, not correctness)
interpretMakePlaceholders :: Int -> InterpreterStepMonad ()
interpretMakePlaceholders n = do
  addrs <- allocPlaceholders
  -- To reverse, or not to reverse, that is the question.
  pushAddrsToAddrStack addrs
  where
    allocPlaceholders = replicateM n $ allocNodeOnHeap NEmpty


interpretPop :: Int -> InterpreterStepMonad ()
interpretPop n = void (popAddrsFromAddrStack n)

interpretDig :: Int -> InterpreterStepMonad ()
interpretDig n = do
  addr <- popAddrFromAddrStack
  _ <- popAddrsFromAddrStack n
  pushAddrToAddrStack addr

interpretUpdate :: Int -> InterpreterStepMonad ()
interpretUpdate n = do
  valueAddr <- peekAddrOnAddrStack
  targetAddr <- peekNthAddrOnAddrStack n
  updateNodeOnHeap targetAddr (NIndirect valueAddr)

interpretCopy :: Int -> InterpreterStepMonad ()
interpretCopy n = do
  addr <- peekNthAddrOnAddrStack n
  pushAddrToAddrStack addr


interpretPushBasicValue :: Integer -> InterpreterStepMonad ()
interpretPushBasicValue = pushValueToValueStack

interpretPushExtractedValue :: InterpreterStepMonad ()
interpretPushExtractedValue = do
  addr <- popAddrFromAddrStack
  node <- findNodeOnHeap addr
  case node of
    NInteger v -> pushValueToValueStack v
    NStructure v fieldsAddr -> do
      fieldsNode <- findNodeOnHeap fieldsAddr
      case fieldsNode of
        NStructureFields 0 _ -> pushValueToValueStack v
        _ -> invalidNodeFail fieldsNode
    _ -> invalidNodeFail node
  where
    invalidNodeFail node
      = fail
        ( "interpretPushExtractedValue: "
          <> show node
          <> " node cannot be used as a primitive value"
        )

interpretWrapAsInteger :: InterpreterStepMonad ()
interpretWrapAsInteger = do
  v <- popValueFromValueStack
  addr <- allocNodeOnHeap (NInteger v)
  pushAddrToAddrStack addr

interpretWrapAsStructure :: InterpreterStepMonad ()
interpretWrapAsStructure = do
  v <- popValueFromValueStack
  fieldsAddr <- allocNodeOnHeap (NStructureFields 0 [])
  addr <- allocNodeOnHeap (NStructure v fieldsAddr)
  pushAddrToAddrStack addr

interpretUpdateAsInteger :: Int -> InterpreterStepMonad ()
interpretUpdateAsInteger n = do
  targetAddr <- peekNthAddrOnAddrStack n
  v <- popValueFromValueStack
  updateNodeOnHeap targetAddr (NInteger v)

interpretUpdateAsStructure :: Int -> InterpreterStepMonad ()
interpretUpdateAsStructure n = do
  targetAddr <- peekNthAddrOnAddrStack n
  v <- popValueFromValueStack
  fieldsAddr <- allocNodeOnHeap (NStructureFields 0 [])
  updateNodeOnHeap targetAddr (NStructure v fieldsAddr)

interpretPrimitive :: PrimitiveOperator -> InterpreterStepMonad ()
interpretPrimitive op
  | Just binaryFun <- primitiveOpToBinaryFun op
  = do
      v0 <- popValueFromValueStack
      v1 <- popValueFromValueStack
      pushValueToValueStack (v0 `binaryFun` v1)
  | Just unaryFun <- primitiveOpToUnaryFun op
  = do
      v <- popValueFromValueStack
      pushValueToValueStack (unaryFun v)
  | otherwise
  = fail
    ( "interpretInstruction: "
      <> show op
      <> " case is not yet implemented"
    )

interpretUnwind :: InterpreterStepMonad ()
interpretUnwind = do
  assertLastCode
  addr <- popAddrFromAddrStack
  node <- findNodeOnHeap addr
  case node of
    NInteger _ -> do
      loadStateFromDump
      pushAddrToAddrStack addr
    NStructure _ _ -> do
      loadStateFromDump
      pushAddrToAddrStack addr
    NApplication funAddr _ -> do
      putInstruction IUnwind
      pushAddrToAddrStack addr
      pushAddrToAddrStack funAddr
    NIndirect addr' -> do
      putInstruction IUnwind
      pushAddrToAddrStack addr'
    NGlobal n code ->
      ifM (checkAddrStackSize (fromInteger n))
        (putInstructions code >> rearrangeStack (fromInteger (n - 1)))
        (putInstruction IReturn)
    _ ->
      fail
      ( "interpretUnwind: "
        <> show node
        <> " case is not allowed"
      )
  where
    rearrangeStack n = do
      addrs <- snoc <$> popAddrsFromAddrStack n <*> peekAddrOnAddrStack
      applyeeAddrs <- forM addrs $ \addr -> do
        node <- findNodeOnHeap addr
        case node of
          NApplication _ applyeeAddr ->
            return applyeeAddr
          _ ->
            fail "rearrangeStack: Invalid invocation of the function. Top most n nodes have to be NApplication nodes"
      pushAddrsToAddrStack applyeeAddrs

interpretEval :: InterpreterStepMonad ()
interpretEval = do
  addr <- popAddrFromAddrStack
  saveStateToDump
  pushAddrToAddrStack addr
  putInstruction IUnwind

interpretReturn :: InterpreterStepMonad ()
interpretReturn = do
  assertLastCode
  addrs <- popAllAddrsFromAddrStack
  addr <-
    case addrs of
      [] -> fail "interpretReturn: address stack should contain more than one address"
      _ -> return (last addrs)
  loadStateFromDump
  pushAddrToAddrStack addr

primitiveOpToBinaryFun :: PrimitiveOperator -> Maybe (Integer -> Integer -> Integer)
primitiveOpToBinaryFun POAdd = Just (+)
primitiveOpToBinaryFun POSub = Just (-)
primitiveOpToBinaryFun POMul = Just (*)
primitiveOpToBinaryFun PODiv = Just div

primitiveOpToUnaryFun :: PrimitiveOperator -> Maybe (Integer -> Integer)
primitiveOpToUnaryFun _ = Nothing
