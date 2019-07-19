{-# LANGUAGE OverloadedStrings #-}
module Minicute.Transpilers.Generator
  ( module Minicute.Data.GMachine.Instruction
  , generateMachineCode
  ) where

import Control.Monad
import Data.String
import Data.Word
import LLVM.IRBuilder
import Minicute.Data.GMachine.Instruction
import Minicute.Data.Minicute.Common

import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as ASTC
import qualified LLVM.AST.Type as ASTT

generateMachineCode :: GMachineProgram -> [AST.Definition]
generateMachineCode program = execModuleBuilder emptyModuleBuilder (generateMachineCodeProgram program)

generateMachineCodeProgram :: GMachineProgram -> ModuleBuilder ()
generateMachineCodeProgram program = forM_ program generateMachineCodeSc

generateMachineCodeSc :: GMachineSupercombinator -> ModuleBuilder ()
generateMachineCodeSc (Identifier binder, _, expr)
  = void . function functionName [] ASTT.void . const $ bodyBuilder
  where
    functionName = fromString ("minicute__user__defined__" <> binder)
    bodyBuilder = do
      emitBlockStart "entry"
      generateMachineCodeE expr

generateMachineCodeE :: GMachineExpression -> IRBuilderT ModuleBuilder ()
generateMachineCodeE = go []
  where
    go vStack (IPushBasicValue v : insts@(_ : _)) = do
      pName <- emitInstr typeInt32Ptr (AST.Alloca ASTT.i32 Nothing 0 [])
      emitInstrVoid (AST.Store False (operandInt 32 v) pName Nothing 0 [])
      vName <- emitInstr typeInt32 (AST.Load False pName Nothing 0 [])

      go (vName : vStack) insts
    go (vName : vStack) (IUpdateAsInteger n : insts@(_ : _)) = do
      hName <- emitInstr typeInt8Ptr (AST.Load False (AST.ConstantOperand constantNodeHeapPointer) Nothing 0 [])
      nName <- emitInstr typeNodeNIntegerPtr (AST.BitCast hName typeNodeNIntegerPtr [])
      nName' <- emitInstr typeNodeNIntegerPtr (AST.GetElementPtr True nName [operandInt 32 1] [])
      hName' <- emitInstr typeInt8Ptr (AST.BitCast nName' typeInt8Ptr [])
      emitInstrVoid (AST.Store False hName' (AST.ConstantOperand constantNodeHeapPointer) Nothing 0 [])
      tName <- emitInstr typeInt32Ptr (AST.GetElementPtr True nName [operandInt 32 0, operandInt 32 0] [])
      emitInstrVoid (AST.Store False (operandInt 32 1) tName Nothing 0 [])
      fName <- emitInstr typeInt32Ptr (AST.GetElementPtr True nName [operandInt 32 0, operandInt 32 1] [])
      emitInstrVoid (AST.Store False vName fName Nothing 0 [])

      sName <- emitInstr typeInt8PtrPtr (AST.Load False (AST.ConstantOperand constantAddrStackPointer) Nothing 0 [])
      sName' <- emitInstr typeInt8PtrPtr (AST.GetElementPtr True sName [operandInt 32 (toInteger (negate n))] [])
      emitInstrVoid (AST.Store False hName sName' Nothing 0 [])

      go vStack insts
    go _ _ = emitTerm (AST.Ret Nothing [])

-- *
-- We should extract or export the following definitions
-- since tests use them too.
operandInt :: Word32 -> Integer -> AST.Operand
operandInt w n = AST.ConstantOperand (ASTC.Int w n)

constantAddrStackPointer :: ASTC.Constant
constantAddrStackPointer = ASTC.GlobalReference typeInt8PtrPtrPtr "asp"

constantNodeHeapPointer :: ASTC.Constant
constantNodeHeapPointer = ASTC.GlobalReference typeInt8PtrPtr "nhp"

typeNodeNIntegerPtr :: ASTT.Type
typeNodeNIntegerPtr = ASTT.ptr typeNodeNInteger

typeNodeNInteger :: ASTT.Type
typeNodeNInteger = ASTT.NamedTypeReference "node.NInteger"

typeInt8PtrPtrPtr :: ASTT.Type
typeInt8PtrPtrPtr = ASTT.ptr typeInt8PtrPtr

typeInt8PtrPtr :: ASTT.Type
typeInt8PtrPtr = ASTT.ptr typeInt8Ptr

typeInt8Ptr :: ASTT.Type
typeInt8Ptr = ASTT.ptr typeInt8

typeInt8 :: ASTT.Type
typeInt8 = ASTT.i8

typeInt32PtrPtrPtr :: ASTT.Type
typeInt32PtrPtrPtr = ASTT.ptr typeInt32PtrPtr

typeInt32PtrPtr :: ASTT.Type
typeInt32PtrPtr = ASTT.ptr typeInt32Ptr

typeInt32Ptr :: ASTT.Type
typeInt32Ptr = ASTT.ptr typeInt32

typeInt32 :: ASTT.Type
typeInt32 = ASTT.i32
