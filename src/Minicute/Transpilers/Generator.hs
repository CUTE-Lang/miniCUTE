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
      nName <- emitInstr typeNodeNIntegerPtr (AST.BitCast (AST.ConstantOperand (ASTC.GetElementPtr True constantAddrStackPointer [ASTC.Int 32 (negate (toInteger n))])) typeNodeNInteger [])
      tName <- emitInstr typeInt32Ptr (AST.GetElementPtr True nName [operandInt 32 0, operandInt 32 0] [])
      emitInstrVoid (AST.Store False (operandInt 32 1) tName Nothing 0 [])
      fName <- emitInstr typeInt32Ptr (AST.GetElementPtr True nName [operandInt 32 0, operandInt 32 1] [])
      emitInstrVoid (AST.Store False vName fName Nothing 0 [])
      go vStack insts
    go _ _ = emitTerm (AST.Ret Nothing [])

-- *
-- We should extract or export the following definitions
-- since tests use them too.
operandInt :: Word32 -> Integer -> AST.Operand
operandInt w n = AST.ConstantOperand (ASTC.Int w n)

constantAddrStackPointer :: ASTC.Constant
constantAddrStackPointer = ASTC.GlobalReference typeInt32PtrPtr "asp"

typeNodeNIntegerPtr :: ASTT.Type
typeNodeNIntegerPtr = ASTT.ptr typeNodeNInteger

typeNodeNInteger :: ASTT.Type
typeNodeNInteger = ASTT.NamedTypeReference "node.NInteger"

typeInt32PtrPtr :: ASTT.Type
typeInt32PtrPtr = ASTT.ptr typeInt32Ptr

typeInt32Ptr :: ASTT.Type
typeInt32Ptr = ASTT.ptr typeInt32

typeInt32 :: ASTT.Type
typeInt32 = ASTT.i32
