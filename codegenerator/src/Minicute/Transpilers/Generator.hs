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
      pName <- alloca ASTT.i32 Nothing 0
      store (operandInt 32 v) 0 pName
      vName <- load pName 0

      go (vName : vStack) insts
    go (vName : vStack) (IUpdateAsInteger n : insts@(_ : _)) = do
      sName <- load (AST.ConstantOperand constantAddrStackPointer) 0
      sName' <- gep sName [operandInt 32 (toInteger (negate n))]
      nName <- load sName' 0
      _ <- call (AST.ConstantOperand constantUpdateNodeNInteger) [(vName, []), (nName, [])]

      go vStack insts
    go _ [IReturn] = do
      bName <- load (AST.ConstantOperand constantAddrBasePointer) 0
      bName' <- gep bName [operandInt 32 0]
      store bName' 0 (AST.ConstantOperand constantAddrStackPointer)
      retVoid
    go _ _ = error "generateMachineCodeE: Not yet implemented"

-- *
-- We should extract or export the following definitions
-- since tests use them too.
operandInt :: Word32 -> Integer -> AST.Operand
operandInt w n = AST.ConstantOperand (ASTC.Int w n)

constantUpdateNodeNInteger :: ASTC.Constant
constantUpdateNodeNInteger = ASTC.GlobalReference typeUpdateNodeNInteger "minicute_update_node_NInteger"

constantAddrStackPointer :: ASTC.Constant
constantAddrStackPointer = ASTC.GlobalReference typeInt8PtrPtrPtr "asp"

constantAddrBasePointer :: ASTC.Constant
constantAddrBasePointer = ASTC.GlobalReference typeInt8PtrPtrPtr "abp"

constantNodeHeapPointer :: ASTC.Constant
constantNodeHeapPointer = ASTC.GlobalReference typeInt8PtrPtr "nhp"

typeUpdateNodeNInteger :: ASTT.Type
typeUpdateNodeNInteger = ASTT.FunctionType ASTT.void [typeInt32, typeInt8Ptr] False

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
