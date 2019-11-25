{-# LANGUAGE ViewPatterns #-}
-- |
-- Copyright: (c) 2018-present Junyoung Clare Jang
-- License: BSD 3-Clause
module Minicute.Interpreter.GMachine.Instruction
  ( module Minicute.Data.Common

  , interpretInstruction
  ) where

import Prelude hiding ( fail )

import Control.Monad.Extra ( forM, ifM, replicateM, void )
import Control.Monad.Fail
import Minicute.Control.GMachine.Step
import Minicute.Data.Common
import Minicute.Data.GMachine.Instruction
import Minicute.Data.GMachine.Node

interpretInstruction :: Instruction -> GMachineStepMonad ()

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
interpretMakeInteger :: Integer -> GMachineStepMonad ()
interpretMakeInteger n = do
  addr <- allocNodeOnNodeHeap (NInteger n)
  pushAddrToAddressStack addr

interpretMakeGlobal :: Identifier -> GMachineStepMonad ()
interpretMakeGlobal i = do
  addr <- findAddressOnGlobal i
  pushAddrToAddressStack addr

interpretMakeApplication :: GMachineStepMonad ()
interpretMakeApplication = do
  applyerAddr <- popAddrFromAddressStack
  applyeeAddr <- popAddrFromAddressStack
  addr <- allocNodeOnNodeHeap (NApplication applyeeAddr applyerAddr)
  pushAddrToAddressStack addr

-- Please check the direction of addrs.
-- (Thankfully, it only affects performance, not correctness)
interpretMakePlaceholders :: Int -> GMachineStepMonad ()
interpretMakePlaceholders n = do
  addrs <- allocPlaceholders
  -- To reverse, or not to reverse, that is the question.
  pushAddrsToAddressStack addrs
  where
    allocPlaceholders = replicateM n $ allocNodeOnNodeHeap NEmpty


interpretPop :: Int -> GMachineStepMonad ()
interpretPop n = void (popAddrsFromAddressStack n)

interpretDig :: Int -> GMachineStepMonad ()
interpretDig n = do
  addr <- popAddrFromAddressStack
  _ <- popAddrsFromAddressStack n
  pushAddrToAddressStack addr

interpretUpdate :: Int -> GMachineStepMonad ()
interpretUpdate n = do
  valueAddr <- peekAddrOnAddressStack
  targetAddr <- peekNthAddrOnAddressStack (n + 1)
  updateNodeOnNodeHeap targetAddr (NIndirect valueAddr)

interpretCopy :: Int -> GMachineStepMonad ()
interpretCopy n = do
  addr <- peekNthAddrOnAddressStack (n + 1)
  pushAddrToAddressStack addr


interpretPushBasicValue :: Integer -> GMachineStepMonad ()
interpretPushBasicValue = pushValueToValueStack

interpretPushExtractedValue :: GMachineStepMonad ()
interpretPushExtractedValue = do
  addr <- popAddrFromAddressStack
  node <- findNodeOnNodeHeap addr
  case node of
    NInteger v -> pushValueToValueStack v
    NStructure v fieldsAddr -> do
      fieldsNode <- findNodeOnNodeHeap fieldsAddr
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

interpretWrapAsInteger :: GMachineStepMonad ()
interpretWrapAsInteger = do
  v <- popValueFromValueStack
  addr <- allocNodeOnNodeHeap (NInteger v)
  pushAddrToAddressStack addr

interpretWrapAsStructure :: GMachineStepMonad ()
interpretWrapAsStructure = do
  v <- popValueFromValueStack
  fieldsAddr <- allocNodeOnNodeHeap (NStructureFields 0 [])
  addr <- allocNodeOnNodeHeap (NStructure v fieldsAddr)
  pushAddrToAddressStack addr

interpretUpdateAsInteger :: Int -> GMachineStepMonad ()
interpretUpdateAsInteger n = do
  targetAddr <- peekNthAddrOnAddressStack (n + 1)
  v <- popValueFromValueStack
  updateNodeOnNodeHeap targetAddr (NInteger v)

interpretUpdateAsStructure :: Int -> GMachineStepMonad ()
interpretUpdateAsStructure n = do
  targetAddr <- peekNthAddrOnAddressStack (n + 1)
  v <- popValueFromValueStack
  fieldsAddr <- allocNodeOnNodeHeap (NStructureFields 0 [])
  updateNodeOnNodeHeap targetAddr (NStructure v fieldsAddr)

interpretPrimitive :: Primitive -> GMachineStepMonad ()
interpretPrimitive op
  | Just binaryFun <- primitiveToBinaryFun op
  = do
      v0 <- popValueFromValueStack
      v1 <- popValueFromValueStack
      pushValueToValueStack (v0 `binaryFun` v1)
  | Just unaryFun <- primitiveToUnaryFun op
  = do
      v <- popValueFromValueStack
      pushValueToValueStack (unaryFun v)
  | otherwise
  = fail
    ( "interpretInstruction: "
      <> show op
      <> " case is not yet implemented"
    )

interpretUnwind :: GMachineStepMonad ()
interpretUnwind = do
  assertLastCode
  addr <- popAddrFromAddressStack
  node <- findNodeOnNodeHeap addr
  case node of
    NApplication funAddr _ -> do
      putInstruction IUnwind
      pushAddrToAddressStack addr
      pushAddrToAddressStack funAddr
    NIndirect addr' -> do
      putInstruction IUnwind
      pushAddrToAddressStack addr'
    NGlobal 0 code -> do
      pushAddrToAddressStack addr
      putInstructions code
    NGlobal n code ->
      ifM (checkSizeOfAddressStack (fromInteger n))
        (putInstructions code >> rearrangeStack (fromInteger n))
        (putInstruction IReturn)
    (isValueNode -> True) -> do
      loadStateFromDump
      pushAddrToAddressStack addr
    _ ->
      fail
      ( "interpretUnwind: "
        <> show node
        <> " case is not allowed"
      )
  where
    -- @n@ should be greater than @1@.
    --
    -- Please check the direction of addrs.
    -- __WARNING: the direction actually affects correctness.__
    rearrangeStack :: Int -> GMachineStepMonad ()
    rearrangeStack n = do
      addrs <- popAddrsFromAddressStack n
      pushAddrToAddressStack (last addrs)
      applyeeAddrs <- forM addrs $ \addr -> do
        node <- findNodeOnNodeHeap addr
        case node of
          NApplication _ applyeeAddr ->
            pure applyeeAddr
          _ ->
            fail $ "rearrangeStack: Invalid invocation of the function. Top most " <> show n <> " nodes have to be NApplication nodes"
      pushAddrsToAddressStack applyeeAddrs

interpretEval :: GMachineStepMonad ()
interpretEval = do
  addr <- popAddrFromAddressStack
  saveStateToDump
  pushAddrToAddressStack addr
  putInstruction IUnwind

-- Please check the direction of addrs.
-- __WARNING: the direction actually affects correctness.__
interpretReturn :: GMachineStepMonad ()
interpretReturn = do
  assertLastCode
  addrs <- popAllAddrsFromAddressStack
  addr <-
    case addrs of
      _ : _ -> pure (last addrs)
      _ -> fail "interpretReturn: address stack should contain more than one address"
  loadStateFromDump
  pushAddrToAddressStack addr

primitiveToBinaryFun :: Primitive -> Maybe (Integer -> Integer -> Integer)
primitiveToBinaryFun PrimAdd = Just (+)
primitiveToBinaryFun PrimSub = Just (-)
primitiveToBinaryFun PrimMul = Just (*)
primitiveToBinaryFun PrimDiv = Just div
primitiveToBinaryFun PrimEq = Just ((boolToInteger .) . (==))
primitiveToBinaryFun PrimNe = Just ((boolToInteger .) . (/=))
primitiveToBinaryFun PrimLt = Just ((boolToInteger .) . (<))
primitiveToBinaryFun PrimLe = Just ((boolToInteger .) . (<=))
primitiveToBinaryFun PrimGt = Just ((boolToInteger .) . (>))
primitiveToBinaryFun PrimGe = Just ((boolToInteger .) . (>=))

primitiveToUnaryFun :: Primitive -> Maybe (Integer -> Integer)
primitiveToUnaryFun PrimAdd = Nothing
primitiveToUnaryFun PrimSub = Nothing
primitiveToUnaryFun PrimMul = Nothing
primitiveToUnaryFun PrimDiv = Nothing
primitiveToUnaryFun PrimEq = Nothing
primitiveToUnaryFun PrimNe = Nothing
primitiveToUnaryFun PrimLt = Nothing
primitiveToUnaryFun PrimLe = Nothing
primitiveToUnaryFun PrimGt = Nothing
primitiveToUnaryFun PrimGe = Nothing

boolToInteger :: Bool -> Integer
boolToInteger True = 1
boolToInteger False = 0
